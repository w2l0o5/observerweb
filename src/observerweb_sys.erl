%%%-------------------------------------------------------------------
%%% @author Bill Wang <freecnpro@gmail.com>
%%% @copyright (C) 2015, Freecnpro.net
%%% @doc
%%%
%%% @end
%%% Created : 17. Jun 2015 11:40 AM
%%%-------------------------------------------------------------------
-module(observerweb_sys).
-author("freecnpro@gmail.com").

%% API
-export([sys_info/1]).

sys_info(Node) ->
  SysInfo = 'observerweb':try_rpc(Node, observer_backend, sys_info, []),
  AllocInfo = proplists:get_value(alloc_info, SysInfo, []),
  AllocFields = alloc_info(AllocInfo, [], 0, 0, true),
  {Info, Stat} = info_fields(),
  InfoFields = 'observerweb_lib':fill_info(Info, SysInfo),
  StatFields =  'observerweb_lib':fill_info(Stat, SysInfo),
  {InfoFields, StatFields, AllocFields}.


alloc_info([{Type,Instances}|Allocators],TypeAcc,TotalBS,TotalCS,IncludeTotal) ->
  {BS,CS,NewTotalBS,NewTotalCS,NewIncludeTotal} =
    sum_alloc_instances(Instances,0,0,TotalBS,TotalCS),
  alloc_info(Allocators,[{Type,BS,CS}|TypeAcc],NewTotalBS,NewTotalCS,
    IncludeTotal andalso NewIncludeTotal);
alloc_info([],TypeAcc,TotalBS,TotalCS,IncludeTotal) ->
  Types = [X || X={_,BS,CS} <- TypeAcc, (BS>0 orelse CS>0)],
  case IncludeTotal of
    true ->
      [{total,TotalBS,TotalCS} | lists:reverse(Types)];
    false ->
      lists:reverse(Types)
  end.

sum_alloc_instances(false,BS,CS,TotalBS,TotalCS) ->
  {BS,CS,TotalBS,TotalCS,false};
sum_alloc_instances([{_,_,Data}|Instances],BS,CS,TotalBS,TotalCS) ->
  {NewBS,NewCS,NewTotalBS,NewTotalCS} =
    sum_alloc_one_instance(Data,BS,CS,TotalBS,TotalCS),
  sum_alloc_instances(Instances,NewBS,NewCS,NewTotalBS,NewTotalCS);
sum_alloc_instances([],BS,CS,TotalBS,TotalCS) ->
  {BS,CS,TotalBS,TotalCS,true}.

sum_alloc_one_instance([{sbmbcs,[{blocks_size,BS,_,_},{carriers_size,CS,_,_}]}|
  Rest],OldBS,OldCS,TotalBS,TotalCS) ->
  sum_alloc_one_instance(Rest,OldBS+BS,OldCS+CS,TotalBS,TotalCS);
sum_alloc_one_instance([{_,[{blocks_size,BS,_,_},{carriers_size,CS,_,_}]}|
  Rest],OldBS,OldCS,TotalBS,TotalCS) ->
  sum_alloc_one_instance(Rest,OldBS+BS,OldCS+CS,TotalBS+BS,TotalCS+CS);
sum_alloc_one_instance([{_,[{blocks_size,BS},{carriers_size,CS}]}|
  Rest],OldBS,OldCS,TotalBS,TotalCS) ->
  sum_alloc_one_instance(Rest,OldBS+BS,OldCS+CS,TotalBS+BS,TotalCS+CS);
sum_alloc_one_instance([_|Rest],BS,CS,TotalBS,TotalCS) ->
  sum_alloc_one_instance(Rest,BS,CS,TotalBS,TotalCS);
sum_alloc_one_instance([],BS,CS,TotalBS,TotalCS) ->
  {BS,CS,TotalBS,TotalCS}.

info_fields() ->
  Info = [{"System and Architecture",
    [{"System Version", otp_release},
      {"Erts Version", version},
      {"Compiled for", system_architecture},
      {"Emulator Wordsize", wordsize_external},
      {"Process Wordsize", wordsize_internal},
      {"Smp Support",  smp_support},
      {"Thread Support",  threads},
      {"Async thread pool size",  thread_pool_size}
    ]},
    {"CPU's and Threads",
      [{"Logical CPU's", logical_processors},
        {"Online Logical CPU's", logical_processors_online},
        {"Available Logical CPU's", logical_processors_available},
        {"Schedulers", schedulers},
        {"Online schedulers", schedulers_online},
        {"Available schedulers", schedulers_available}
      ]}
  ],
  Stat = [{"Memory Usage",
    [{"Total", {bytes, total}},
      {"Processes", {bytes, processes}},
      {"Atoms", {bytes, atom}},
      {"Binaries", {bytes, binary}},
      {"Code", {bytes, code}},
      {"Ets", {bytes, ets}}
    ]},
    {"Statistics",
      [{"Up time", {time_ms, uptime}},
        {"Max Processes", process_limit},
        {"Processes", process_count},
        {"Run Queue", run_queue},
        {"IO Input",  {bytes, io_input}},
        {"IO Output", {bytes, io_output}}
      ]}
  ],
  {Info, Stat}.