%%%-------------------------------------------------------------------
%%% @author Bill Wang <bill@freecnpro.net>
%%% @copyright (C) 2015, Freecnpro.net
%%% @doc
%%%
%%% @end
%%% Created : 17. Jun 2015 11:34 AM
%%%-------------------------------------------------------------------
-module(observerweb).
-author("bill@freecnpro.net").

%% API
-export([start/0, stop/0]).
-export([try_rpc/4]).

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
