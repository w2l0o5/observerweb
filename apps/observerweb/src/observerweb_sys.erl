%%%-------------------------------------------------------------------
%%% @author Bill Wang
%%% @copyright (C) 2017, Freecnpro
%%% @doc
%%%
%%% @end
%%% Created : 2017-04-17
%%%-------------------------------------------------------------------
-module(observerweb_sys).
-author("bill@freecnpro.net").

%% API
-export([sys_info/1]).

sys_info(Node) ->
    SysInfo = observerweb:try_rpc(Node, observer_backend, sys_info, []),
    {Info, Stat} = info_fields(),
    InfoFields = observerweb_lib:fill_info(Info, SysInfo),
    StatFields = observerweb_lib:fill_info(Stat, SysInfo),
    {InfoFields, StatFields}.

info_fields() ->
    Info = [
        {"System and Architecture",
            [
                {"System Version", otp_release},
                {"Erts Version", version},
                {"Compiled for", system_architecture},
                {"Emulator Wordsize", wordsize_external},
                {"Process Wordsize", wordsize_internal},
                {"Smp Support",  smp_support},
                {"Thread Support",  threads},
                {"Async thread pool size",  thread_pool_size}
            ]},
        {"CPU's and Threads",
            [
                {"Logical CPU's", logical_processors},
                {"Online Logical CPU's", logical_processors_online},
                {"Available Logical CPU's", logical_processors_available},
                {"Schedulers", schedulers},
                {"Online schedulers", schedulers_online},
                {"Available schedulers", schedulers_available}
            ]}
    ],

    Stat = [
        {"Memory Usage",
            [
                {"Total", {bytes, total}},
                {"Processes", {bytes, processes}},
                {"Atoms", {bytes, atom}},
                {"Binaries", {bytes, binary}},
                {"Code", {bytes, code}},
                {"Ets", {bytes, ets}}
            ]},
        {"Statistics",
            [
                {"Up time", {time_ms, uptime}},
                {"Max Processes", process_limit},
                {"Processes", process_count},
                {"Run Queue", run_queue},
                {"IO Input",  {bytes, io_input}},
                {"IO Output", {bytes, io_output}}
            ]}
    ],
    {Info, Stat}.