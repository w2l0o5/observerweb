%%%-------------------------------------------------------------------
%%% @author Bill Wang
%%% @copyright (C) 2016, Freecnpro.net
%%% @doc
%%%
%%% @end
%%% Created : 2016-01-02
%%%-------------------------------------------------------------------
-author("bill@freecnpro.net").

%% ------------------------------------
%% Logging mechanism
%% ------------------------------------

-define(PRINT(Format, Args),
  io:format(Format, Args)).

-define(PRINT_MSG(Msg),
  io:format(Msg)).

-define(DEBUG(Format, Args),
  lager:debug(Format, Args)).

-define(INFO_MSG(Format, Args),
  lager:info(Format, Args)).

-define(WARNING_MSG(Format, Args),
  lager:warning(Format, Args)).

-define(ERROR_MSG(Format, Args),
  lager:error(Format, Args)).

-define(CRITICAL_MSG(Format, Args),
  lager:critical(Format, Args)).
