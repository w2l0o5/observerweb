%%%-------------------------------------------------------------------
%%% @author Bill Wang <bill@freecnpro.net>
%%% @copyright (C) 2015, Freecnpro.net
%%% @doc
%%%
%%% @end
%%% Created : 17. Jun 2015 11:32 AM
%%%-------------------------------------------------------------------
-module(observerweb_sup).
-author("bill@freecnpro.net").

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link() ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%%
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
  {ok, {SupFlags :: {RestartStrategy :: supervisor:strategy(),
    MaxR :: non_neg_integer(), MaxT :: non_neg_integer()},
    [ChildSpec :: supervisor:child_spec()]
  }} |
  ignore |
  {error, Reason :: term()}).
init([]) ->
  RestartStrategy = one_for_one,
  MaxRestarts = 10,
  MaxSecondsBetweenRestarts = 10,

  SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

  Restart = permanent,
  Shutdown = 2000,
  Type = worker,

  AChild = {'observerweb_pro', {'observerweb_pro', start_link, []},
    Restart, Shutdown, Type, ['observerweb_pro']},

  {ok, {SupFlags, [AChild]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
