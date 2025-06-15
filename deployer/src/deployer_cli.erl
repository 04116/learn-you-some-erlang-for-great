-module(deployer_cli).

-export([main/1, start_deployment/1]).

%% ===================================================================
%% API functions
%% ===================================================================

main(Args) ->
    application:ensure_all_started(inets),
    application:ensure_all_started(ssl),
    application:ensure_all_started(jsx),

    case parse_args(Args) of
        {ok, Config} ->
            io:format("🚀 Starting concurrent deployment automation...~n"),
            start_deployment(Config);
        {error, Reason} ->
            io:format("Error: ~s~n", [Reason]),
            usage(),
            halt(1)
    end.

start_deployment(Config) ->
    % Check if concurrent mode is enabled (default: true)
    ConcurrentMode = proplists:get_value(concurrent, Config, true),

    case ConcurrentMode of
        true ->
            run_concurrent_deployment(Config);
        false ->
            io:format("Sequential mode not supported in this version. Using concurrent mode.~n"),
            run_concurrent_deployment(Config)
    end.

%% ===================================================================
%% Internal functions
%% ===================================================================

parse_args([ConfigFile]) ->
    deployer_config:parse_config(ConfigFile);
parse_args(_) ->
    {error, "Invalid arguments"}.

run_concurrent_deployment(Config) ->
    io:format("🚀 Starting concurrent deployment mode~n"),
    case deployer_coordinator:start_deployment(Config) of
        {ok, Pid} ->
            monitor_deployment(Pid);
        {error, Reason} ->
            io:format("Failed to start concurrent deployment: ~p~n", [Reason])
    end.

monitor_deployment(Pid) ->
    Ref = monitor(process, Pid),
    receive
        {'DOWN', Ref, process, Pid, normal} ->
            io:format("🎉 All deployments completed successfully~n");
        {'DOWN', Ref, process, Pid, Reason} ->
            io:format("❌ Deployment failed: ~p~n", [Reason])
    end.

usage() ->
    io:format("Usage: rebar3 escriptize && ./_build/default/bin/deployer <config.json>~n"),
    io:format("~n"),
    io:format("Example config.json:~n"),
    io:format("{~n"),
    io:format("  \"github_api_key\": \"github_pat_...\",~n"),
    io:format("  \"slack_bot_token\": \"xoxb-...\",~n"),
    io:format("  \"slack_channel_id\": \"C...\",~n"),
    io:format("  \"concurrent\": true,~n"),
    io:format("  \"repos\": [~n"),
    io:format("    {\"repo\": \"owner/repo\", \"workflow\": \"deploy.yml\"}~n"),
    io:format("  ]~n"),
    io:format("}~n").
