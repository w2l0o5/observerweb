%%%-------------------------------------------------------------------
%%% @author Bill Wang
%%% @copyright (C) 2017, Freecnpro
%%% @doc
%%%
%%% @end
%%% Created : 2017-04-17
%%%-------------------------------------------------------------------
-module(observerweb_lib).
-author("bill@freecnpro.net").

%% API
-export([fill_info/2, to_str/1, escape/1]).

fill_info([{dynamic, Key}|Rest], Data) when is_atom(Key); is_function(Key) ->

    %% Special case used by crashdump_viewer when the value decides
    %% which header to use
    case get_value(Key, Data) of
        undefined -> [undefined | fill_info(Rest, Data)];
        {Str,Value} -> [{Str, Value} | fill_info(Rest, Data)]
    end;
fill_info([{Str, Key}|Rest], Data) when is_atom(Key); is_function(Key) ->
    case get_value(Key, Data) of
        undefined -> [undefined | fill_info(Rest, Data)];
        Value -> [{Str, Value} | fill_info(Rest, Data)]
    end;
fill_info([{Str,Attrib,Key}|Rest], Data) when is_atom(Key); is_function(Key) ->
    case get_value(Key, Data) of
        undefined -> [undefined | fill_info(Rest, Data)];
        Value -> [{Str,Attrib,Value} | fill_info(Rest, Data)]
    end;
fill_info([{Str, {Format, Key}}|Rest], Data) when is_atom(Key); is_function(Key), is_atom(Format) ->
    case get_value(Key, Data) of
        undefined -> [undefined | fill_info(Rest, Data)];
        Value -> [{Str, {Format, Value}} | fill_info(Rest, Data)]
    end;
fill_info([{Str, Attrib, {Format, Key}}|Rest], Data) when is_atom(Key); is_function(Key), is_atom(Format) ->
    case get_value(Key, Data) of
        undefined -> [undefined | fill_info(Rest, Data)];
        Value -> [{Str, Attrib, {Format, Value}} | fill_info(Rest, Data)]
    end;
fill_info([{Str,SubStructure}|Rest], Data) when is_list(SubStructure) ->
    [{Str, fill_info(SubStructure, Data)}|fill_info(Rest,Data)];
fill_info([{Str,Attrib,SubStructure}|Rest], Data) ->
    [{Str, Attrib, fill_info(SubStructure, Data)}|fill_info(Rest,Data)];
fill_info([], _) -> [].

get_value(Key, Data) when is_atom(Key) ->
    proplists:get_value(Key,Data);
get_value(Fun, Data) when is_function(Fun) ->
    Fun(Data).

to_str(Value) when is_atom(Value) ->
    atom_to_list(Value);
to_str({Unit, X}) when (Unit==bytes orelse Unit==time_ms) andalso is_list(X) ->
    try list_to_integer(X) of
        B -> to_str({Unit,B})
    catch error:badarg -> X
    end;
to_str({bytes, B}) ->
    KB = B div 1024,
    MB = KB div 1024,
    GB = MB div 1024,
    if
        GB > 10 -> integer_to_list(GB) ++ " GB";
        MB > 10 -> integer_to_list(MB) ++ " MB";
        KB >  0 -> integer_to_list(KB) ++ " kB";
        true -> integer_to_list(B) ++ " B"
    end;
to_str({time_ms, MS}) ->
    S = MS div 1000,
    Min = S div 60,
    Hours = Min div 60,
    Days = Hours div 24,
    if
        Days > 0 -> integer_to_list(Days) ++ " Days";
        Hours > 0 -> integer_to_list(Hours) ++ " Hours";
        Min > 0 -> integer_to_list(Min) ++ " Mins";
        true -> integer_to_list(S) ++ " Secs"
    end;

to_str({func, {F,A}}) when is_atom(F), is_integer(A) ->
    lists:concat([F, "/", A]);
to_str({func, {F,'_'}}) when is_atom(F) ->
    atom_to_list(F);
to_str({{format,Fun},Value}) when is_function(Fun) ->
    Fun(Value);
to_str({A, B}) when is_atom(A), is_atom(B) ->
    lists:concat([A, ":", B]);
to_str({M,F,A}) when is_atom(M), is_atom(F), is_integer(A) ->
    lists:concat([M, ":", F, "/", A]);
to_str(Value) when is_list(Value) ->
    case lists:all(fun(X) -> is_integer(X) end, Value) of
        true -> Value;
        false ->
            lists:foldl(fun(X, Acc) -> to_str(X) ++ " " ++ Acc end, "", Value)
    end;
to_str(Port) when is_port(Port) ->
    erlang:port_to_list(Port);
to_str(Pid) when is_pid(Pid) ->
    pid_to_list(Pid);
to_str(No) when is_integer(No) ->
    integer_to_list(No);
to_str(Term) ->
    io_lib:format("~w", [Term]).

escape(Str) when is_list(Str) ->
    re:replace(Str, "'", "\\\\'", [global, {return, list}]).