%%%-------------------------------------------------------------------
%%% @author Bill Wang <bill@freecnpro.net>
%%% @copyright (C) 2015, Freecnpro.net
%%% @doc
%%%
%%% @end
%%% Created : 17. Jun 2015 11:36 AM
%%%-------------------------------------------------------------------
-module(observerweb_perf).
-author("bill@freecnpro.net").

%% API
-export([perf_info/2]).

perf_info(Node, PerfType) ->
  'observerweb':try_rpc(Node, erlang, system_flag, [scheduler_wall_time, true]),
  case PerfType of
    scheduler ->
      lists:sort('observerweb':try_rpc(Node, erlang, statistics, [scheduler_wall_time]));
    memory ->
      MemT = mem_types(),
      MemInfo = 'observerweb':try_rpc(Node, erlang, memory, []),
      [{Type, Value} || {Type, Value} <- MemInfo, lists:member(Type, MemT)];
    io ->
      {{input, Input},{output, Output}} = 'observerweb':try_rpc(Node, erlang, statistics, [io]),
      [{input, Input},{output, Output}]
  end.

mem_types() ->
  [total, processes, atom, binary, code, ets].