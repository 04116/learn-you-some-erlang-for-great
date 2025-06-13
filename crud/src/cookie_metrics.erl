%% =============================================================================
%% Cookie CRUD API - Prometheus Metrics Module
%% =============================================================================
%% This module provides basic Prometheus-compatible metrics for monitoring
%% the Cookie CRUD API. For Go developers, this is similar to implementing
%% the /metrics endpoint that prometheus/client_golang provides.
%% 
%% Key concepts for Go developers:
%% - ETS (Erlang Term Storage): Like sync.Map or atomic counters in Go
%% - gen_server: Like a goroutine with mailbox pattern
%% - Binary formatting: Like fmt.Sprintf but for binary data
%% =============================================================================

-module(cookie_metrics).

%% API exports
-export([
    init/0,
    increment_http_requests/2,
    observe_http_duration/2,
    increment_db_operations/1,
    get_metrics/0
]).

%% Internal state tracking
-record(metrics_state, {
    http_requests = #{},      %% Map of {method, status} -> count
    db_operations = #{},      %% Map of operation -> count
    start_time = undefined    %% Service start timestamp
}).

%% ETS table for storing metrics (like a global atomic map)
-define(METRICS_TABLE, cookie_metrics_table).

%% =============================================================================
%% Public API Functions
%% =============================================================================

%% Initialize metrics collection
%% Go equivalent: prometheus.NewCounterVec(...)
-spec init() -> ok.
init() ->
    %% Create ETS table for storing metrics (thread-safe global storage)
    %% Go equivalent: var metricsMap sync.Map
    %% Use try/catch to handle race condition in concurrent creation
    try
        ets:new(?METRICS_TABLE, [named_table, public, set]),
        %% Initialize with empty state
        ets:insert(?METRICS_TABLE, {state, #metrics_state{start_time = erlang:system_time(second)}}),
        ok
    catch
        error:badarg ->
            %% Table already exists (race condition)
            ok
    end.

%% Increment HTTP request counter
%% Go equivalent: httpRequestsTotal.WithLabelValues(method, status).Inc()
-spec increment_http_requests(binary(), integer()) -> ok.
increment_http_requests(Method, StatusCode) ->
    Key = {Method, StatusCode},
    
    %% Ensure table exists before operations (defensive programming)
    %% Go equivalent: sync.Once or checking if prometheus is initialized
    try
        %% Atomic increment operation with better error handling
        case ets:lookup(?METRICS_TABLE, {http_requests, Key}) of
            [{_, Count}] ->
                ets:insert(?METRICS_TABLE, {{http_requests, Key}, Count + 1});
            [] ->
                ets:insert(?METRICS_TABLE, {{http_requests, Key}, 1})
        end,
        ok
    catch
        error:badarg ->
            %% Table doesn't exist - initialize and retry once
            init(),
            try
                case ets:lookup(?METRICS_TABLE, {http_requests, Key}) of
                    [{_, ExistingCount}] ->
                        ets:insert(?METRICS_TABLE, {{http_requests, Key}, ExistingCount + 1});
                    [] ->
                        ets:insert(?METRICS_TABLE, {{http_requests, Key}, 1})
                end,
                ok
            catch
                _:_ ->
                    %% Log error for debugging but don't crash the request
                    io:format("Warning: Failed to record HTTP metrics for ~p ~p~n", [Method, StatusCode]),
                    ok
            end;
        _:Error ->
            %% Log unexpected errors but don't crash
            io:format("Warning: Unexpected error in HTTP metrics: ~p~n", [Error]),
            ok
    end.

%% Record HTTP request duration
%% Go equivalent: httpDurationHistogram.WithLabelValues(method).Observe(duration)
-spec observe_http_duration(binary(), float()) -> ok.
observe_http_duration(Method, DurationSeconds) ->
    %% For simplicity, we'll track as counters. In production, use proper histograms
    Key = {Method, duration},
    
    try
        case ets:lookup(?METRICS_TABLE, {http_duration, Key}) of
            [{_, {TotalTime, Count}}] ->
                ets:insert(?METRICS_TABLE, {{http_duration, Key}, {TotalTime + DurationSeconds, Count + 1}});
            [] ->
                ets:insert(?METRICS_TABLE, {{http_duration, Key}, {DurationSeconds, 1}})
        end,
        ok
    catch
        error:badarg ->
            %% Table doesn't exist - initialize and retry once
            init(),
            try
                case ets:lookup(?METRICS_TABLE, {http_duration, Key}) of
                    [{_, {ExistingTotalTime, ExistingCount}}] ->
                        ets:insert(?METRICS_TABLE, {{http_duration, Key}, {ExistingTotalTime + DurationSeconds, ExistingCount + 1}});
                    [] ->
                        ets:insert(?METRICS_TABLE, {{http_duration, Key}, {DurationSeconds, 1}})
                end,
                ok
            catch
                _:_ ->
                    io:format("Warning: Failed to record HTTP duration metrics for ~p~n", [Method]),
                    ok
            end;
        _:Error ->
            io:format("Warning: Unexpected error in HTTP duration metrics: ~p~n", [Error]),
            ok
    end.

%% Increment database operation counter
%% Go equivalent: dbOperationsTotal.WithLabelValues(operation).Inc()
-spec increment_db_operations(atom()) -> ok.
increment_db_operations(Operation) ->
    Key = Operation,
    
    try
        case ets:lookup(?METRICS_TABLE, {db_operations, Key}) of
            [{_, Count}] ->
                ets:insert(?METRICS_TABLE, {{db_operations, Key}, Count + 1});
            [] ->
                ets:insert(?METRICS_TABLE, {{db_operations, Key}, 1})
        end,
        ok
    catch
        error:badarg ->
            %% Table doesn't exist - initialize and retry once
            init(),
            try
                case ets:lookup(?METRICS_TABLE, {db_operations, Key}) of
                    [{_, ExistingDBCount}] ->
                        ets:insert(?METRICS_TABLE, {{db_operations, Key}, ExistingDBCount + 1});
                    [] ->
                        ets:insert(?METRICS_TABLE, {{db_operations, Key}, 1})
                end,
                ok
            catch
                _:_ ->
                    io:format("Warning: Failed to record DB metrics for ~p~n", [Operation]),
                    ok
            end;
        _:Error ->
            io:format("Warning: Unexpected error in DB metrics: ~p~n", [Error]),
            ok
    end.

%% Generate Prometheus-compatible metrics output
%% Go equivalent: promhttp.Handler() output
-spec get_metrics() -> binary().
get_metrics() ->
    
    %% Collect all metrics from ETS table
    HttpRequests = collect_http_requests(),
    HttpDurations = collect_http_durations(),
    DbOperations = collect_db_operations(),
    UptimeSeconds = get_uptime_seconds(),
    
    %% Format as Prometheus exposition format
    %% Reference: https://prometheus.io/docs/instrumenting/exposition_formats/
    MetricsText = [
        %% HTTP request metrics
        "# HELP http_requests_total Total number of HTTP requests by method and status\n",
        "# TYPE http_requests_total counter\n",
        format_http_requests(HttpRequests),
        
        %% HTTP duration metrics (simplified)
        "# HELP http_request_duration_seconds Average HTTP request duration\n", 
        "# TYPE http_request_duration_seconds gauge\n",
        format_http_durations(HttpDurations),
        
        %% Database operation metrics
        "# HELP db_operations_total Total number of database operations\n",
        "# TYPE db_operations_total counter\n",
        format_db_operations(DbOperations),
        
        %% Service uptime
        "# HELP service_uptime_seconds Time since service started\n",
        "# TYPE service_uptime_seconds gauge\n",
        io_lib:format("service_uptime_seconds ~p~n", [UptimeSeconds])
    ],
    
    %% Convert to binary (like []byte in Go)
    iolist_to_binary(MetricsText).

%% =============================================================================
%% Internal Helper Functions
%% =============================================================================

%% Collect HTTP request metrics from ETS
collect_http_requests() ->
    Pattern = {{http_requests, '_'}, '_'},
    try
        ets:match_object(?METRICS_TABLE, Pattern)
    catch
        error:badarg ->
            %% Table doesn't exist
            []
    end.

%% Collect HTTP duration metrics from ETS
collect_http_durations() ->
    Pattern = {{http_duration, '_'}, '_'},
    try
        ets:match_object(?METRICS_TABLE, Pattern)
    catch
        error:badarg ->
            []
    end.

%% Collect database operation metrics from ETS
collect_db_operations() ->
    Pattern = {{db_operations, '_'}, '_'},
    try
        ets:match_object(?METRICS_TABLE, Pattern)
    catch
        error:badarg ->
            []
    end.

%% Get service uptime in seconds
get_uptime_seconds() ->
    try
        case ets:lookup(?METRICS_TABLE, state) of
            [{state, #metrics_state{start_time = StartTime}}] when StartTime =/= undefined ->
                erlang:system_time(second) - StartTime;
            _ ->
                0
        end
    catch
        error:badarg ->
            0
    end.

%% Format HTTP request metrics for Prometheus
format_http_requests([]) ->
    "";
format_http_requests([{{http_requests, {Method, Status}}, Count} | Rest]) ->
    Line = io_lib:format("http_requests_total{method=\"~s\",status=\"~p\"} ~p~n", 
                        [Method, Status, Count]),
    [Line | format_http_requests(Rest)].

%% Format HTTP duration metrics for Prometheus
format_http_durations([]) ->
    "";
format_http_durations([{{http_duration, {Method, duration}}, {TotalTime, Count}} | Rest]) ->
    AvgDuration = if Count > 0 -> TotalTime / Count; true -> 0 end,
    Line = io_lib:format("http_request_duration_seconds{method=\"~s\"} ~.3f~n", 
                        [Method, AvgDuration]),
    [Line | format_http_durations(Rest)].

%% Format database operation metrics for Prometheus
format_db_operations([]) ->
    "";
format_db_operations([{{db_operations, Operation}, Count} | Rest]) ->
    Line = io_lib:format("db_operations_total{operation=\"~s\"} ~p~n", 
                        [Operation, Count]),
    [Line | format_db_operations(Rest)].