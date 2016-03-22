%%%-------------------------------------------------------------------
%%% @author Bill Wang <bill@freecnpro.net>
%%% @copyright (C) 2015, Freecnpro.net
%%% @doc
%%%
%%% @end
%%% Created : 17. Jun 2015 11:37 AM
%%%-------------------------------------------------------------------
-module(observerweb_pro).
-author("bill@freecnpro.net").

-behaviour(gen_server).

%% API
-export([start_link/0, update/0, change_node/1]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-include("observer_backend.hrl").

%% Defines
-define(COL_PID,  0).
-define(COL_NAME, ?COL_PID+1).
%%-define(COL_TIME, 2).
-define(COL_REDS, ?COL_NAME+1).
-define(COL_MEM,  ?COL_REDS+1).
-define(COL_MSG,  ?COL_MEM+1).
-define(COL_FUN,  ?COL_MSG+1).

-define(SERVER, ?MODULE).

%% Records
-record(sort,
{
  sort_key=?COL_REDS,
  sort_incr=false
}).

-record(state, {
  info,
  etop,
  sort=#sort{},
  accum=[],
  node
}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link() ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%--------------------------------------------------------------------
%% @doc
%% Update info
%%
%% @end
%%--------------------------------------------------------------------
-spec(update() -> term()).
update() ->
  gen_server:call(?SERVER, update).

%%--------------------------------------------------------------------
%% @doc
%% Change node
%%
%% @end
%%--------------------------------------------------------------------
-spec(change_node(Node :: node()) -> term()).
change_node(Node) ->
  gen_server:cast(?SERVER, {change_node, Node}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
  {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).
init([]) ->
  {ok, #state{etop=#etop_info{}, info=array:new(), node = node()}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #state{}) ->
  {reply, Reply :: term(), NewState :: #state{}}).
handle_call(update, _From, State) ->
  {ProcInfo, State1} = get_update(State),
  Json = warp_json(ProcInfo),
  {reply, Json, State1}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_cast(Request :: term(), State :: #state{}) ->
  {noreply, NewState :: #state{}}).
handle_cast({change_node, NewNode}, #state{node=Node}=S0) ->
  State = case Node == NewNode of
            true -> S0;
            false -> S0#state{node=NewNode}
          end,
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
-spec(handle_info(Info :: timeout() | term(), State :: #state{}) ->
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_info(_Info, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #state{}) -> term()).
terminate(_Reason, _State) ->
  ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #state{},
    Extra :: term()) ->
  {ok, NewState :: #state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
get_update(#state{node=Node, sort=Sort=#sort{sort_key=KeyField}} = S0) ->
  Pid = spawn_link(Node,observer_backend,etop_collect,[self()]),
  Info = receive
           {Pid, EtopInfo=#etop_info{}} -> EtopInfo
         after 1000 -> exit(connection_lost)
         end,
  #etop_info{procinfo=ProcInfo0} = Info,
  {ProcInfo1, S1} = accum(ProcInfo0, S0),
  {_SO, ProcInfo} = sort(KeyField, Sort#sort{sort_key=undefined}, ProcInfo1),
  {ProcInfo, S1#state{info=ProcInfo, etop=Info#etop_info{procinfo=[]}}}.

accum(ProcInfo, State=#state{accum=true}) ->
  {ProcInfo, State};
accum(ProcInfo0, State=#state{accum=Previous}) ->
  ProcInfo = lists:sort(ProcInfo0),
  {accum2(ProcInfo,Previous,[]), State#state{accum=ProcInfo}}.

accum2([PI=#etop_proc_info{pid=Pid, reds=Reds, runtime=RT}|PIs],
    [#etop_proc_info{pid=Pid, reds=OldReds, runtime=OldRT}|Old], Acc) ->
  accum2(PIs, Old, [PI#etop_proc_info{reds=Reds-OldReds, runtime=RT-OldRT}|Acc]);
accum2(PIs=[#etop_proc_info{pid=Pid}|_], [#etop_proc_info{pid=OldPid}|Old], Acc)
  when Pid > OldPid ->
  accum2(PIs, Old, Acc);
accum2([PI|PIs], Old, Acc) ->
  accum2(PIs, Old, [PI|Acc]);
accum2([], _, Acc) -> Acc.

sort(Col, Opt, Table)
  when not is_list(Table) ->
  sort(Col,Opt,array:to_list(Table));
sort(Col, Opt=#sort{sort_key=Col, sort_incr=Bool}, Table) ->
  {Opt#sort{sort_incr=not Bool},
    array:from_list(lists:reverse(Table))};
sort(Col, S=#sort{sort_incr=true}, Table) ->
  {S#sort{sort_key=Col},
    array:from_list(lists:keysort(col_to_element(Col), Table))};
sort(Col, S=#sort{sort_incr=false}, Table) ->
  {S#sort{sort_key=Col},
    array:from_list(lists:reverse(lists:keysort(col_to_element(Col), Table)))}.

get_procinfo_data(Col, Info) ->
  element(col_to_element(Col), Info).
col_to_element(?COL_PID)  -> #etop_proc_info.pid;
col_to_element(?COL_NAME) -> #etop_proc_info.name;
col_to_element(?COL_MEM)  -> #etop_proc_info.mem;
%%col_to_element(?COL_TIME) -> #etop_proc_info.runtime;
col_to_element(?COL_REDS) -> #etop_proc_info.reds;
col_to_element(?COL_FUN)  -> #etop_proc_info.cf;
col_to_element(?COL_MSG)  -> #etop_proc_info.mq.

warp_json(Info) ->
  warp_json2(0, Info, []).

warp_json2(Index, Info, Json) ->
  case Index =:= array:size(Info) of
    true -> Json;
    false ->
      Pid = list_to_binary('observerweb_lib':to_str(get_procinfo_data(?COL_PID, array:get(Index, Info)))),
      Name = list_to_binary('observerweb_lib':to_str(get_procinfo_data(?COL_NAME, array:get(Index, Info)))),
      Reds = list_to_binary('observerweb_lib':to_str(get_procinfo_data(?COL_REDS, array:get(Index, Info)))),
      Mem = list_to_binary('observerweb_lib':to_str(get_procinfo_data(?COL_MEM, array:get(Index, Info)))),
      Msg = list_to_binary('observerweb_lib':to_str(get_procinfo_data(?COL_MSG, array:get(Index, Info)))),
      Fun = list_to_binary('observerweb_lib':to_str(get_procinfo_data(?COL_FUN, array:get(Index, Info)))),
      warp_json2(Index+1, Info,
        [{obj, [{"pid", Pid},{"name", Name},{"reds", Reds},{"mem",Mem},{"msg",Msg},{"fun",Fun}]} | Json])
  end.
