%%%-------------------------------------------------------------------
%%% @author Bill Wang <freecnpro@gmail.com>
%%% @copyright (C) 2015, Freecnpro.net
%%% @doc
%%%
%%% @end
%%% Created : 17. Jun 2015 11:42 AM
%%%-------------------------------------------------------------------
-module(observerweb_handler).
-author("freecnpro@gmail.com").

%% API
-export([init/2]).

init(Req, Opts) ->
  Method = cowboy_req:method(Req),
  HasBody = cowboy_req:has_body(Req),
  Req2 = process(Method, HasBody, Req),
  {ok, Req2, Opts}.

process(<<"POST">>, false, Req) ->
  cowboy_req:reply(400, [], <<"Missing body.">>, Req);
process(<<"POST">>, true, Req) ->
  {ok, PostVals, Req2} = cowboy_req:body_qs(Req),
  case proplists:get_value(<<"action">>, PostVals) of
    <<"get_sys">> ->
      Body = 'observerweb_lib':escape(do_process(get_sys, get_acc_node())),
      cowboy_req:reply(200, [{<<"content-type">>, <<"text/plain; charset=utf-8">>}], list_to_binary(Body), Req2);
    <<"get_perf">> ->
      Type = proplists:get_value(<<"type">>, PostVals),
      Body = do_process(get_perf, {get_acc_node(), binary_to_atom(Type, latin1)}),
      cowboy_req:reply(200, [{<<"content-type">>, <<"text/plain; charset=utf-8">>}], list_to_binary(Body), Req2);
    <<"get_pro">> ->
      Type = proplists:get_value(<<"type">>, PostVals),
      Body = do_process(get_pro, Type),
      cowboy_req:reply(200, [{<<"content-type">>, <<"text/plain; charset=utf-8">>}], Body, Req2);
    <<"change_node">> ->
      Node = proplists:get_value(<<"node">>, PostVals),
      Result = do_process(change_node, Node),
      cowboy_req:reply(200, [{<<"content-type">>, <<"text/plain; charset=utf-8">>}], Result, Req2);
    <<"connect_node">> ->
      Node = proplists:get_value(<<"node">>, PostVals),
      Cookie = proplists:get_value(<<"cookie">>, PostVals),
      Result = case do_process(connect_node, {Node, Cookie}) of
                 pang -> <<"Connect failed">>;
                 pong -> <<"ok">>
               end,
      cowboy_req:reply(200, [{<<"content-type">>, <<"text/plain; charset=utf-8">>}], Result, Req2);
    <<"get_nodes">> ->
      Body = rfc4627:encode({obj, [{nodes, get_bare_nodes()}]}),
      cowboy_req:reply(200, [{<<"content-type">>, <<"text/plain; charset=utf-8">>}], list_to_binary(Body), Req2);
    <<"del_node">> ->
      Node = proplists:get_value(<<"node">>, PostVals),
      del_node(Node),
      Req2
  end;
process(_, _, Req) ->
  %% Method not allowed.
  cowboy_req:reply(405, Req).

do_process(get_sys, Node) ->
  {Info, Stat, Alloc} = observerweb_sys:sys_info(Node),
  [{_SysName, SysValue},{_CPUName, CPUValue}] = Info,
  [{_MemName, MemValue},{_StatName, StatValue}] = Stat,
  rfc4627:encode({obj, [{"system", wrap_info(info, SysValue)},
    {"cpu", wrap_info(info, CPUValue)},
    {"memory", wrap_info(info,MemValue)},
    {"statistics", wrap_info(info, StatValue)},
    {"alloctor", wrap_info(alloc, Alloc)}]});
do_process(get_perf, {Node, Type}) ->
  Data0 = observerweb_perf:perf_info(Node, Type),
  case Type of
    scheduler ->
      Data = wrap_info(scheduler, Data0),
      rfc4627:encode({obj, [{scheduler, Data}]});
    _ ->
      rfc4627:encode({obj, Data0})
  end;

do_process(get_pro, Type) ->
  case Type of
    <<"all">> ->
      Data = observerweb_pro:update(),
      list_to_binary(rfc4627:encode(Data));
    _ ->
      io:format("Type: ~p~n", [Type])
  end;

do_process(change_node, Value) ->
  Node = binary_to_atom(Value, latin1),
  case lists:keyfind(Node, 1, get_nodes()) of
    {_, Cookie} ->
      erlang:set_cookie(node(), Cookie),
      case net_adm:ping(Node) of
        pang ->
          <<"false">>;
        pong ->
          insert_Data(acc_node, Node),
          observerweb_pro:change_node(Node),
          <<"true">>
      end;
    false ->
      <<"Node invalid">>
  end;
do_process(connect_node, {Value1, Value2}) ->
  try
    Node = binary_to_atom(Value1, latin1),
    Cookie = binary_to_atom(Value2, latin1),
    io:format("Node: ~p~nCookie:~p~n", [Node, Cookie]),
    erlang:set_cookie(node(), Cookie),
    case net_adm:ping(Node) of
      pang -> pang;
      pong ->
        add_node({Node, Cookie}),
        insert_Data(acc_node, Node),
        pong
    end
  catch _:_ ->
    pang
  end.

add_node({Node, Cookie}) ->
  Nodes = get_nodes(),
  NewNodes = case lists:keyfind(Node, 1, Nodes) of
               false -> [{Node, Cookie} | Nodes];
               _ -> [{Node, Cookie} | lists:keydelete(Node, 1, Nodes)]
             end,
  io:format("Nodes: ~p~n~p~n", [Nodes, NewNodes]),
  insert_Data(nodes, NewNodes).

del_node(Node) ->
  NewNodes = lists:keydelete(Node, 1, get_nodes()),
  insert_Data(nodes, NewNodes).

wrap_info(Type, Info) ->
  wrap_info2(Type, Info, []).

wrap_info2(alloc, [], Data) -> lists:reverse(Data);
wrap_info2(alloc, [{Name, BS, CS}|Alloc], Data) ->
  wrap_info2(alloc, Alloc, [{obj, [{name, Name}, {bs, (BS div 1024)}, {cs, (CS div 1024)}]} | Data]);

wrap_info2(scheduler, [], Data) -> lists:reverse(Data);
wrap_info2(scheduler, [{SchedulerId, ActiveTime, TotalTime}|Scheduler], Data) ->
  wrap_info2(scheduler, Scheduler, [{obj, [{shcedulerid, SchedulerId},{activetime, ActiveTime},{totaltime, TotalTime}]} | Data]);

wrap_info2(info, [], Data) -> lists:reverse(Data);
wrap_info2(info, [{Name, Value}|Stat], Data) ->
  wrap_info2(info, Stat, [{obj, [{name, list_to_binary(Name)}, {value, list_to_binary('observerweb_lib':to_str(Value))}]} | Data]).

get_acc_node() ->
  case get_data(acc_node) of
    [] -> node();
    [{_, Node}] -> Node
  end.

get_nodes() ->
  case get_data(nodes) of
    [] -> [{node(), erlang:get_cookie()}];
    [{_, Nodes1}] -> Nodes1
  end.

get_bare_nodes() ->
  get_bare_nodess(get_nodes(), []).

get_bare_nodess([], Data) -> lists:usort(Data);
get_bare_nodess([{Node, _Cookie} | Nodes], Data) ->
  get_bare_nodess(Nodes, [Node | Data]).

get_data(Key) ->
  {ok, Dets} = dets:open_file("observer_table"),
  Data = dets:lookup(Dets, Key),
  dets:close(Dets),
  Data.

insert_Data(Key, Data) ->
  {ok, Dets} = dets:open_file("observer_table"),
  dets:insert(Dets, {Key, Data}),
  dets:close(Dets).