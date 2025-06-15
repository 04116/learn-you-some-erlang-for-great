-module(deployer).

%% escript entry point
-export([main/1]).

%% ===================================================================
%% escript entry point
%% ===================================================================

main(Args) ->
    deployer_cli:main(Args).
