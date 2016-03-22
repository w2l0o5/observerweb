%%%-------------------------------------------------------------------
%%% @author Bill Wang <bill@freecnpro.net>
%%% @copyright (C) 2015, Freecnpro.net
%%% @doc
%%%
%%% @end
%%% Created : 17. Jun 2015 11:27 AM
%%%-------------------------------------------------------------------
-module(observerweb_app).
-author("bill@freecnpro.net").

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%%===================================================================
%%% Application callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called whenever an application is started using
%% application:start/[1,2], and should start the processes of the
%% application. If the application is structured according to the OTP
%% design principles as a supervision tree, this means starting the
%% top supervisor of the tree.
%%
%% @end
%%--------------------------------------------------------------------
-spec(start(StartType :: normal | {takeover, node()} | {failover, node()},
    StartArgs :: term()) ->
  {ok, pid()} |
  {ok, pid(), State :: term()} |
  {error, Reason :: term()}).
start(_StartType, _StartArgs) ->
  Dispatch = cowboy_router:compile([
    {'_', [
      {"/", cowboy_static, {priv_file, observerweb, "index.html"}},
      {"/css/[...]", cowboy_static, {priv_dir, observerweb, "css"}},
      {"/js/[...]", cowboy_static, {priv_dir, observerweb, "js"}},
      {"/img/[...]", cowboy_static, {priv_dir, observerweb, "img"}},
      {"/info", observerweb_handler, []}
    ]}
  ]),
  {ok, _} = cowboy:start_http(http, 100, [{port, 8080}], [
    {env, [{dispatch, Dispatch}]}
  ]),
  dets:open_file(observer_table, [{type, set}, {file, "observer_table"}]),
  dets:close(observer_table),
  observerweb_sup:start_link().

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called whenever an application has stopped. It
%% is intended to be the opposite of Module:start/2 and should do
%% any necessary cleaning up. The return value is ignored.
%%
%% @end
%%--------------------------------------------------------------------
-spec(stop(State :: term()) -> term()).
stop(_State) ->
  {ok, Ref} = dets:open_file("observer_table"),
  dets:delete_all_objects(Ref),
  dets:close(Ref),
  ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================
