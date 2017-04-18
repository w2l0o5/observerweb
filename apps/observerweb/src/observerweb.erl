%%%-------------------------------------------------------------------
%%% @author Bill Wang
%%% @copyright (C) 2017, Freecnpro
%%% @doc
%%%
%%% @end
%%% Created : 2017-04-17
%%%-------------------------------------------------------------------
-module(observerweb).
-author("bill@freecnpro.net").

%% API
-export([start/0, stop/0]).
-export([try_rpc/4, env/2]).

-define(APP, ?MODULE).

start() ->
  application:ensure_all_started(observerweb).

stop() ->
  application:stop(observerweb).

try_rpc(Node, Mod, Func, Args) ->
  case rpc:call(Node, Mod, Func, Args) of
    {badrpc, Reason} ->
      error_logger:error_report([{node, Node},
        {call, {Mod, Func, Args}},
        {reason, {badrpc, Reason}}]),
      error({badrpc, Reason});
    Res ->
      Res
  end.

env(Key, Default) -> application:get_env(?APP, Key, Default).
