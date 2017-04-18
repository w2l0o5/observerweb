%%%-------------------------------------------------------------------
%%% @author Bill Wang
%%% @copyright (C) 2017, Freecnpro
%%% @doc
%%%
%%% @end
%%% Created : 2017-04-17
%%%-------------------------------------------------------------------
-module(observerweb_perf).
-author("bill@freecnpro.net").

%% API
-export([perf_info/2]).

perf_info(Node, PerfType) ->
    observerweb:try_rpc(Node, erlang, system_flag, [scheduler_wall_time, true]),
    case PerfType of
        scheduler ->
            lists:sort(observerweb:try_rpc(Node, erlang, statistics, [scheduler_wall_time]));
        memory ->
            MemT = mem_types(),
            MemInfo = observerweb:try_rpc(Node, erlang, memory, []),
            [{Type, Value} || {Type, Value} <- MemInfo, lists:member(Type, MemT)];
        io ->
            {{input, Input},{output, Output}} = observerweb:try_rpc(Node, erlang, statistics, [io]),
            [{input, Input},{output, Output}]
    end.

mem_types() ->
    [total, processes, atom, binary, code, ets].