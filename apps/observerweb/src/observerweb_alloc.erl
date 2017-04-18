%%%-------------------------------------------------------------------
%%% @author Bill Wang
%%% @copyright (C) 2017, Freecnpro
%%% @doc
%%%
%%% @end
%%% Created : 2017-04-17
%%%-------------------------------------------------------------------
-module(observerweb_alloc).
-author("bill@freecnpro.net").

-export([memory_alloc_info/1]).

memory_alloc_info(Node) ->
	SysInfo = observerweb:try_rpc(Node, observer_backend, sys_info, []),
  	AllocFields = alloc_info(SysInfo),
  	AllocFields.

%%====================================================================
alloc_info(SysInfo) ->
    AllocInfo = proplists:get_value(alloc_info, SysInfo, []),
    alloc_info(AllocInfo, [], 0, 0, true).

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