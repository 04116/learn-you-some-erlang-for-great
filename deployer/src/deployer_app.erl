-module(deployer_app).
-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    % Ensure required applications are started
    application:ensure_all_started(inets),
    application:ensure_all_started(ssl),

    deployer_sup:start_link().

stop(_State) ->
    ok.
