%% Debug script to check metrics collection
-module(debug_metrics).
-export([check_ets/0, test_metrics/0]).

check_ets() ->
    %% Check if ETS table exists
    case ets:info(cookie_metrics_table) of
        undefined ->
            io:format("ETS table does not exist~n");
        Info ->
            io:format("ETS table exists: ~p~n", [Info]),
            %% Print all contents
            io:format("Contents:~n"),
            ets:foldl(fun(Entry, _) ->
                io:format("  ~p~n", [Entry])
            end, ok, cookie_metrics_table)
    end.

test_metrics() ->
    %% Test metrics collection manually
    io:format("Testing metrics collection...~n"),
    
    %% Initialize metrics
    cookie_metrics:init(),
    
    %% Simulate some operations
    cookie_metrics:increment_http_requests(<<"GET">>, 200),
    cookie_metrics:increment_http_requests(<<"POST">>, 201),
    cookie_metrics:increment_db_operations(get_cookie),
    cookie_metrics:observe_http_duration(<<"GET">>, 0.05),
    
    %% Check results
    check_ets(),
    
    %% Get formatted metrics
    Metrics = cookie_metrics:get_metrics(),
    io:format("Formatted metrics:~n~s~n", [Metrics]).