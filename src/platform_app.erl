%% @doc Callbacks for the platform application.

-module(platform_app).
-author('Ilshad Khabibullin <astoon.net@gmail.com>').

-behaviour(application).
-export([start/2,stop/1]).


%% @spec start(_Type, _StartArgs) -> ServerRet
%% @doc application start callback for platform.
start(_Type, _StartArgs) ->
    platform_sup:start_link().

%% @spec stop(_State) -> ServerRet
%% @doc application stop callback for platform.
stop(_State) ->
    ok.
