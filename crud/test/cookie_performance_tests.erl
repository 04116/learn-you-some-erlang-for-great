-module(cookie_performance_tests).
-include_lib("eunit/include/eunit.hrl").

%% Test configuration
-define(TEST_DB, "test_performance.db").
-define(LOAD_TEST_DURATION, 10000). % 10 seconds
-define(BENCHMARK_ITERATIONS, 1000).

%%====================================================================
%% Test Fixtures
%%====================================================================

setup() ->
    %% Clean up any existing test database
    file:delete(?TEST_DB),
    
    %% Set test configuration optimized for performance
    application:set_env(cookie_crud, db_file, ?TEST_DB),
    application:set_env(cookie_crud, db_pool_size, 10),
    
    %% Start the application
    {ok, _} = application:ensure_all_started(cookie_crud),
    
    %% Wait for initialization
    timer:sleep(200),
    ok.

cleanup(_) ->
    %% Stop the application
    application:stop(cookie_crud),
    
    %% Clean up test database
    file:delete(?TEST_DB),
    ok.

%%====================================================================
%% Performance Tests
%%====================================================================

performance_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     {timeout, 60, [
         {"Single operation benchmarks", fun test_single_operation_benchmarks/0},
         {"Concurrent read performance", fun test_concurrent_read_performance/0},
         {"Concurrent write performance", fun test_concurrent_write_performance/0},
         {"Mixed workload performance", fun test_mixed_workload_performance/0},
         {"Bulk operations performance", fun test_bulk_operations_performance/0},
         {"Memory usage during load", fun test_memory_usage/0},
         {"Database pool utilization", fun test_pool_utilization/0},
         {"Stress test with high concurrency", fun test_stress_high_concurrency/0}
     ]}}.

%% Benchmark individual operations
test_single_operation_benchmarks() ->
    %% Create benchmark
    CreateTimes = benchmark_operation(fun() ->
        CookieId = generate_unique_cookie_id(),
        Data = generate_cookie_data(CookieId),
        cookie_crud:create_cookie(Data)
    end, ?BENCHMARK_ITERATIONS),
    
    AvgCreateTime = lists:sum(CreateTimes) / length(CreateTimes),
    io:format("Average CREATE time: ~.2f microseconds~n", [AvgCreateTime]),
    
    %% Setup test data for read/update/delete benchmarks
    TestCookies = [begin
        CookieId = generate_unique_cookie_id(),
        Data = generate_cookie_data(CookieId),
        {ok, _} = cookie_crud:create_cookie(Data),
        CookieId
    end || _ <- lists:seq(1, ?BENCHMARK_ITERATIONS)],
    
    %% Read benchmark
    ReadTimes = benchmark_operation(fun() ->
        CookieId = lists:nth(rand:uniform(length(TestCookies)), TestCookies),
        cookie_crud:get_cookie(CookieId)
    end, ?BENCHMARK_ITERATIONS),
    
    AvgReadTime = lists:sum(ReadTimes) / length(ReadTimes),
    io:format("Average READ time: ~.2f microseconds~n", [AvgReadTime]),
    
    %% Update benchmark
    UpdateTimes = benchmark_operation(fun() ->
        CookieId = lists:nth(rand:uniform(length(TestCookies)), TestCookies),
        UpdateData = #{<<"status">> => <<"updated">>, <<"timestamp">> => erlang:system_time()},
        cookie_crud:update_cookie(CookieId, UpdateData)
    end, ?BENCHMARK_ITERATIONS div 2), % Fewer updates to avoid conflicts
    
    AvgUpdateTime = lists:sum(UpdateTimes) / length(UpdateTimes),
    io:format("Average UPDATE time: ~.2f microseconds~n", [AvgUpdateTime]),
    
    %% Performance assertions
    ?assert(AvgCreateTime < 10000), % Should be under 10ms
    ?assert(AvgReadTime < 5000),    % Should be under 5ms
    ?assert(AvgUpdateTime < 10000), % Should be under 10ms
    
    %% Cleanup
    lists:foreach(fun(CookieId) ->
        cookie_crud:delete_cookie(CookieId)
    end, TestCookies).

%% Test concurrent read performance
test_concurrent_read_performance() ->
    %% Setup test data
    NumCookies = 1000,
    TestCookies = [begin
        CookieId = generate_unique_cookie_id(),
        Data = generate_cookie_data(CookieId),
        {ok, _} = cookie_crud:create_cookie(Data),
        CookieId
    end || _ <- lists:seq(1, NumCookies)],
    
    %% Run concurrent reads
    NumReaders = 20,
    ReadsPerReader = 100,
    
    {TotalTime, Results} = timer:tc(fun() ->
        run_concurrent_operations(NumReaders, fun() ->
            lists:map(fun(_) ->
                CookieId = lists:nth(rand:uniform(length(TestCookies)), TestCookies),
                {Time, Result} = timer:tc(cookie_crud, get_cookie, [CookieId]),
                {Time, Result}
            end, lists:seq(1, ReadsPerReader))
        end)
    end),
    
    %% Analyze results
    AllTimes = lists:flatten([[Time || {Time, {ok, _}} <- WorkerResults] || WorkerResults <- Results]),
    TotalOps = length(AllTimes),
    AvgTime = lists:sum(AllTimes) / TotalOps,
    OpsPerSecond = TotalOps / (TotalTime / 1000000),
    
    io:format("Concurrent Read Performance:~n"),
    io:format("  Total operations: ~p~n", [TotalOps]),
    io:format("  Average response time: ~.2f microseconds~n", [AvgTime]),
    io:format("  Operations per second: ~.2f~n", [OpsPerSecond]),
    
    %% Performance assertions
    ?assert(OpsPerSecond > 1000), % Should handle > 1000 reads/sec
    ?assert(AvgTime < 10000),     % Average under 10ms
    
    %% Cleanup
    lists:foreach(fun(CookieId) ->
        cookie_crud:delete_cookie(CookieId)
    end, TestCookies).

%% Test concurrent write performance
test_concurrent_write_performance() ->
    NumWriters = 10,
    WritesPerWriter = 50,
    
    {TotalTime, Results} = timer:tc(fun() ->
        run_concurrent_operations(NumWriters, fun() ->
            lists:map(fun(_N) ->
                CookieId = generate_unique_cookie_id(),
                Data = generate_cookie_data(CookieId),
                {Time, Result} = timer:tc(cookie_crud, create_cookie, [Data]),
                {Time, Result, CookieId}
            end, lists:seq(1, WritesPerWriter))
        end)
    end),
    
    %% Analyze results
    AllResults = lists:flatten(Results),
    SuccessTimes = [Time || {Time, {ok, _}, _} <- AllResults],
    CreatedCookies = [CookieId || {_, {ok, _}, CookieId} <- AllResults],
    
    TotalSuccessOps = length(SuccessTimes),
    AvgTime = lists:sum(SuccessTimes) / TotalSuccessOps,
    OpsPerSecond = TotalSuccessOps / (TotalTime / 1000000),
    
    io:format("Concurrent Write Performance:~n"),
    io:format("  Successful operations: ~p~n", [TotalSuccessOps]),
    io:format("  Average response time: ~.2f microseconds~n", [AvgTime]),
    io:format("  Operations per second: ~.2f~n", [OpsPerSecond]),
    
    %% Performance assertions
    ?assert(OpsPerSecond > 100),  % Should handle > 100 writes/sec
    ?assert(AvgTime < 50000),     % Average under 50ms
    ?assert(TotalSuccessOps =:= NumWriters * WritesPerWriter), % All should succeed
    
    %% Cleanup
    lists:foreach(fun(CookieId) ->
        cookie_crud:delete_cookie(CookieId)
    end, CreatedCookies).

%% Test mixed workload performance (realistic scenario)
test_mixed_workload_performance() ->
    %% Setup initial data
    InitialCookies = 500,
    TestCookies = [begin
        CookieId = generate_unique_cookie_id(),
        Data = generate_cookie_data(CookieId),
        {ok, _} = cookie_crud:create_cookie(Data),
        CookieId
    end || _ <- lists:seq(1, InitialCookies)],
    
    %% Mixed workload: 60% reads, 25% writes, 10% updates, 5% deletes
    NumWorkers = 15,
    OpsPerWorker = 50,
    
    {TotalTime, Results} = timer:tc(fun() ->
        run_concurrent_operations(NumWorkers, fun() ->
            lists:map(fun(_) ->
                Operation = weighted_operation_choice(),
                execute_operation(Operation, TestCookies)
            end, lists:seq(1, OpsPerWorker))
        end)
    end),
    
    %% Analyze results
    AllResults = lists:flatten(Results),
    SuccessfulOps = [Time || {Time, {ok, _}} <- AllResults] ++ 
                    [Time || {Time, ok} <- AllResults],
    
    TotalSuccessOps = length(SuccessfulOps),
    AvgTime = lists:sum(SuccessfulOps) / TotalSuccessOps,
    OpsPerSecond = TotalSuccessOps / (TotalTime / 1000000),
    
    %% Categorize operations
    OpCounts = count_operations(AllResults),
    
    io:format("Mixed Workload Performance:~n"),
    io:format("  Total successful operations: ~p~n", [TotalSuccessOps]),
    io:format("  Average response time: ~.2f microseconds~n", [AvgTime]),
    io:format("  Operations per second: ~.2f~n", [OpsPerSecond]),
    io:format("  Operation breakdown: ~p~n", [OpCounts]),
    
    %% Performance assertions
    ?assert(OpsPerSecond > 200),  % Should handle > 200 mixed ops/sec
    ?assert(AvgTime < 25000),     % Average under 25ms
    
    %% Get current cookies and cleanup
    {ok, CurrentCookies} = cookie_crud:get_all_cookies(),
    lists:foreach(fun(Cookie) ->
        CookieId = maps:get(<<"cookie">>, Cookie),
        cookie_crud:delete_cookie(CookieId)
    end, CurrentCookies).

%% Test bulk operations performance
test_bulk_operations_performance() ->
    %% Bulk create
    NumCookies = 1000,
    CookieData = [generate_cookie_data(generate_unique_cookie_id()) || _ <- lists:seq(1, NumCookies)],
    
    {CreateTime, CreateResults} = timer:tc(fun() ->
        lists:map(fun(Data) ->
            cookie_crud:create_cookie(Data)
        end, CookieData)
    end),
    
    SuccessfulCreates = length([ok || {ok, _} <- CreateResults]),
    CreateOpsPerSec = SuccessfulCreates / (CreateTime / 1000000),
    
    io:format("Bulk Create Performance:~n"),
    io:format("  Created ~p cookies in ~.2f seconds~n", [SuccessfulCreates, CreateTime / 1000000]),
    io:format("  Create rate: ~.2f cookies/second~n", [CreateOpsPerSec]),
    
    %% Bulk read (get all)
    {ReadAllTime, {ok, AllCookies}} = timer:tc(cookie_crud, get_all_cookies, []),
    
    io:format("Bulk Read Performance:~n"),
    io:format("  Retrieved ~p cookies in ~.2f seconds~n", [length(AllCookies), ReadAllTime / 1000000]),
    
    %% Bulk delete
    CookieIds = [maps:get(<<"cookie">>, Cookie) || Cookie <- AllCookies],
    
    {DeleteTime, _} = timer:tc(fun() ->
        lists:foreach(fun(CookieId) ->
            cookie_crud:delete_cookie(CookieId)
        end, CookieIds)
    end),
    
    DeleteOpsPerSec = length(CookieIds) / (DeleteTime / 1000000),
    
    io:format("Bulk Delete Performance:~n"),
    io:format("  Deleted ~p cookies in ~.2f seconds~n", [length(CookieIds), DeleteTime / 1000000]),
    io:format("  Delete rate: ~.2f cookies/second~n", [DeleteOpsPerSec]),
    
    %% Performance assertions
    ?assert(CreateOpsPerSec > 100),   % Should create > 100 cookies/sec
    ?assert(DeleteOpsPerSec > 200).   % Should delete > 200 cookies/sec

%% Test memory usage during load
test_memory_usage() ->
    %% Get initial memory usage
    InitialMemory = erlang:memory(total),
    
    %% Create a large number of cookies
    NumCookies = 5000,
    CookieIds = [begin
        CookieId = generate_unique_cookie_id(),
        Data = generate_cookie_data(CookieId),
        {ok, _} = cookie_crud:create_cookie(Data),
        CookieId
    end || _ <- lists:seq(1, NumCookies)],
    
    %% Check memory usage after creation
    AfterCreateMemory = erlang:memory(total),
    
    %% Perform operations to stress memory
    lists:foreach(fun(_) ->
        CookieId = lists:nth(rand:uniform(length(CookieIds)), CookieIds),
        cookie_crud:get_cookie(CookieId)
    end, lists:seq(1, 1000)),
    
    %% Check memory usage after operations
    AfterOpsMemory = erlang:memory(total),
    
    %% Clean up
    lists:foreach(fun(CookieId) ->
        cookie_crud:delete_cookie(CookieId)
    end, CookieIds),
    
    %% Force garbage collection
    erlang:garbage_collect(),
    timer:sleep(100),
    
    %% Check final memory usage
    FinalMemory = erlang:memory(total),
    
    io:format("Memory Usage Analysis:~n"),
    io:format("  Initial memory: ~p bytes (~.2f MB)~n", [InitialMemory, InitialMemory / 1024 / 1024]),
    io:format("  After creating ~p cookies: ~p bytes (~.2f MB)~n", 
              [NumCookies, AfterCreateMemory, AfterCreateMemory / 1024 / 1024]),
    io:format("  After operations: ~p bytes (~.2f MB)~n", [AfterOpsMemory, AfterOpsMemory / 1024 / 1024]),
    io:format("  After cleanup: ~p bytes (~.2f MB)~n", [FinalMemory, FinalMemory / 1024 / 1024]),
    
    %% Memory should not grow excessively
    MemoryIncrease = FinalMemory - InitialMemory,
    MaxAcceptableIncrease = 50 * 1024 * 1024, % 50MB
    
    ?assert(MemoryIncrease < MaxAcceptableIncrease).

%% Test database pool utilization under load
test_pool_utilization() ->
    %% Create load that should utilize most/all pool connections
    NumWorkers = 15, % More than pool size to create contention
    OpsPerWorker = 20,
    
    %% Setup test data
    TestCookies = [begin
        CookieId = generate_unique_cookie_id(),
        Data = generate_cookie_data(CookieId),
        {ok, _} = cookie_crud:create_cookie(Data),
        CookieId
    end || _ <- lists:seq(1, 100)],
    
    %% Run load test with deliberate delays to stress pool
    StartTime = erlang:monotonic_time(microsecond),
    Results = run_concurrent_operations(NumWorkers, fun() ->
        lists:map(fun(_) ->
            %% Mix of quick and slow operations
            case rand:uniform(3) of
                1 ->
                    %% Quick read
                    CookieId = lists:nth(rand:uniform(length(TestCookies)), TestCookies),
                    {Time, Result} = timer:tc(cookie_crud, get_cookie, [CookieId]),
                    {quick_read, Time, Result};
                2 ->
                    %% Slow operation (simulate with sleep in with_connection)
                    Result = cookie_db_pool:with_connection(fun(Db) ->
                        timer:sleep(rand:uniform(100)), % 0-100ms delay
                        esqlite3:q(Db, "SELECT 1")
                    end),
                    {slow_op, 0, Result};
                3 ->
                    %% Regular create
                    CookieId = generate_unique_cookie_id(),
                    Data = generate_cookie_data(CookieId),
                    {Time, Result} = timer:tc(cookie_crud, create_cookie, [Data]),
                    {create, Time, Result}
            end
        end, lists:seq(1, OpsPerWorker))
    end),
    EndTime = erlang:monotonic_time(microsecond),
    
    TotalTime = EndTime - StartTime,
    AllResults = lists:flatten(Results),
    TotalOps = length(AllResults),
    
    io:format("Pool Utilization Test:~n"),
    io:format("  Workers: ~p (Pool size: ~p)~n", [NumWorkers, 10]),
    io:format("  Total operations: ~p~n", [TotalOps]),
    io:format("  Total time: ~.2f seconds~n", [TotalTime / 1000000]),
    io:format("  Operations per second: ~.2f~n", [TotalOps / (TotalTime / 1000000)]),
    
    %% Should complete without timeouts despite pool contention
    SuccessfulOps = length([ok || {_, _, {ok, _}} <- AllResults] ++ 
                          [ok || {_, _, ok} <- AllResults]),
    SuccessRate = SuccessfulOps / TotalOps * 100,
    
    io:format("  Success rate: ~.1f%~n", [SuccessRate]),
    
    ?assert(SuccessRate > 95), % Should have > 95% success rate
    
    %% Cleanup
    {ok, AllCookies} = cookie_crud:get_all_cookies(),
    lists:foreach(fun(Cookie) ->
        CookieId = maps:get(<<"cookie">>, Cookie),
        cookie_crud:delete_cookie(CookieId)
    end, AllCookies).

%% Stress test with very high concurrency
test_stress_high_concurrency() ->
    %% High concurrency stress test
    NumWorkers = 50,
    OpsPerWorker = 20,
    
    %% Setup some initial data
    InitialCookies = [begin
        CookieId = generate_unique_cookie_id(),
        Data = generate_cookie_data(CookieId),
        {ok, _} = cookie_crud:create_cookie(Data),
        CookieId
    end || _ <- lists:seq(1, 200)],
    
    io:format("Starting stress test with ~p workers, ~p operations each~n", [NumWorkers, OpsPerWorker]),
    
    {TotalTime, Results} = timer:tc(fun() ->
        run_concurrent_operations(NumWorkers, fun() ->
            lists:map(fun(_) ->
                %% Random operations with higher write percentage
                case rand:uniform(10) of
                    N when N =< 4 -> % 40% reads
                        CookieId = lists:nth(rand:uniform(length(InitialCookies)), InitialCookies),
                        {read, timer:tc(cookie_crud, get_cookie, [CookieId])};
                    N when N =< 7 -> % 30% creates
                        CookieId = generate_unique_cookie_id(),
                        Data = generate_cookie_data(CookieId),
                        {create, timer:tc(cookie_crud, create_cookie, [Data])};
                    N when N =< 9 -> % 20% updates
                        CookieId = lists:nth(rand:uniform(length(InitialCookies)), InitialCookies),
                        UpdateData = #{<<"stress_field">> => rand:uniform(1000)},
                        {update, timer:tc(cookie_crud, update_cookie, [CookieId, UpdateData])};
                    _ -> % 10% get_all
                        {get_all, timer:tc(cookie_crud, get_all_cookies, [])}
                end
            end, lists:seq(1, OpsPerWorker))
        end)
    end),
    
    %% Analyze stress test results
    AllResults = lists:flatten(Results),
    TotalOps = length(AllResults),
    
    %% Count successes by operation type
    ReadSuccesses = length([ok || {read, {_, {ok, _}}} <- AllResults]),
    CreateSuccesses = length([ok || {create, {_, {ok, _}}} <- AllResults]),
    UpdateSuccesses = length([ok || {update, {_, {ok, _}}} <- AllResults]),
    GetAllSuccesses = length([ok || {get_all, {_, {ok, _}}} <- AllResults]),
    
    TotalSuccesses = ReadSuccesses + CreateSuccesses + UpdateSuccesses + GetAllSuccesses,
    SuccessRate = TotalSuccesses / TotalOps * 100,
    OpsPerSecond = TotalOps / (TotalTime / 1000000),
    
    io:format("Stress Test Results:~n"),
    io:format("  Total operations: ~p~n", [TotalOps]),
    io:format("  Total time: ~.2f seconds~n", [TotalTime / 1000000]),
    io:format("  Operations per second: ~.2f~n", [OpsPerSecond]),
    io:format("  Success rate: ~.1f%~n", [SuccessRate]),
    io:format("  Read successes: ~p~n", [ReadSuccesses]),
    io:format("  Create successes: ~p~n", [CreateSuccesses]),
    io:format("  Update successes: ~p~n", [UpdateSuccesses]),
    io:format("  Get-all successes: ~p~n", [GetAllSuccesses]),
    
    %% Stress test assertions
    ?assert(SuccessRate > 90),    % Should maintain > 90% success under stress
    ?assert(OpsPerSecond > 50),   % Should maintain > 50 ops/sec under stress
    
    %% Final cleanup
    {ok, FinalCookies} = cookie_crud:get_all_cookies(),
    lists:foreach(fun(Cookie) ->
        CookieId = maps:get(<<"cookie">>, Cookie),
        cookie_crud:delete_cookie(CookieId)
    end, FinalCookies).

%%====================================================================
%% Helper Functions
%%====================================================================

%% Generate unique cookie ID
generate_unique_cookie_id() ->
    Timestamp = erlang:system_time(microsecond),
    Random = rand:uniform(1000000),
    list_to_binary("perf_test_" ++ integer_to_list(Timestamp) ++ "_" ++ integer_to_list(Random)).

%% Generate cookie data
generate_cookie_data(CookieId) ->
    #{
        <<"cookie">> => CookieId,
        <<"user_id">> => rand:uniform(100000),
        <<"test_field">> => <<"performance_test">>,
        <<"created_by">> => <<"test_suite">>,
        <<"priority">> => rand:uniform(10)
    }.

%% Benchmark a single operation
benchmark_operation(Fun, Iterations) ->
    [begin
        {Time, _Result} = timer:tc(Fun),
        Time
    end || _ <- lists:seq(1, Iterations)].

%% Run concurrent operations
run_concurrent_operations(NumWorkers, WorkerFun) ->
    Parent = self(),
    Workers = [spawn(fun() ->
        Result = WorkerFun(),
        Parent ! {worker_result, self(), Result}
    end) || _ <- lists:seq(1, NumWorkers)],
    
    [receive
        {worker_result, Worker, Result} -> Result
    end || Worker <- Workers].

%% Choose operation based on weights (realistic workload)
weighted_operation_choice() ->
    case rand:uniform(100) of
        N when N =< 60 -> read;    % 60% reads
        N when N =< 85 -> create;  % 25% creates  
        N when N =< 95 -> update;  % 10% updates
        _ -> delete                % 5% deletes
    end.

%% Execute an operation based on type
execute_operation(read, TestCookies) ->
    CookieId = lists:nth(rand:uniform(length(TestCookies)), TestCookies),
    {Time, Result} = timer:tc(cookie_crud, get_cookie, [CookieId]),
    {Time, Result};
execute_operation(create, _TestCookies) ->
    CookieId = generate_unique_cookie_id(),
    Data = generate_cookie_data(CookieId),
    {Time, Result} = timer:tc(cookie_crud, create_cookie, [Data]),
    {Time, Result};
execute_operation(update, TestCookies) ->
    CookieId = lists:nth(rand:uniform(length(TestCookies)), TestCookies),
    UpdateData = #{<<"last_access">> => erlang:system_time()},
    {Time, Result} = timer:tc(cookie_crud, update_cookie, [CookieId, UpdateData]),
    {Time, Result};
execute_operation(delete, TestCookies) ->
    case TestCookies of
        [] -> {0, {error, no_cookies}};
        _ ->
            CookieId = lists:nth(rand:uniform(length(TestCookies)), TestCookies),
            {Time, Result} = timer:tc(cookie_crud, delete_cookie, [CookieId]),
            {Time, Result}
    end.

%% Count operation types in results
count_operations(Results) ->
    lists:foldl(fun(Result, Acc) ->
        case Result of
            {_, {ok, _}} -> maps:update_with(success, fun(X) -> X + 1 end, 1, Acc);
            {_, ok} -> maps:update_with(success, fun(X) -> X + 1 end, 1, Acc);
            {_, {error, _}} -> maps:update_with(error, fun(X) -> X + 1 end, 1, Acc);
            _ -> maps:update_with(other, fun(X) -> X + 1 end, 1, Acc)
        end
    end, #{}, Results).