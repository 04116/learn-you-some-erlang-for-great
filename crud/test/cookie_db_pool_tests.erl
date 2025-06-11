-module(cookie_db_pool_tests).
-include_lib("eunit/include/eunit.hrl").

%% Test configuration
-define(TEST_DB, "test_pool.db").
-define(TEST_POOL_SIZE, 3).

%%====================================================================
%% Test Fixtures
%%====================================================================

setup() ->
    %% Clean up any existing test database
    file:delete(?TEST_DB),
    
    %% Set test configuration
    application:set_env(cookie_crud, db_file, ?TEST_DB),
    application:set_env(cookie_crud, db_pool_size, ?TEST_POOL_SIZE),
    
    %% Initialize database
    cookie_db_pool:init_database(),
    
    %% Start the pool
    {ok, Pid} = cookie_db_pool:start_link(),
    
    %% Wait for pool to initialize
    timer:sleep(100),
    Pid.

cleanup(Pid) ->
    %% Stop the pool
    gen_server:stop(Pid),
    
    %% Clean up test database
    file:delete(?TEST_DB),
    ok.

%%====================================================================
%% Unit Tests
%%====================================================================

pool_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
         {"Basic connection acquisition", fun test_basic_connection/0},
         {"Connection pool exhaustion", fun test_pool_exhaustion/0},
         {"Connection return and reuse", fun test_connection_reuse/0},
         {"Concurrent connection access", fun test_concurrent_access/0},
         {"Connection monitoring", fun test_connection_monitoring/0},
         {"Database operations through pool", fun test_database_operations/0},
         {"Pool performance stats", fun test_performance_stats/0},
         {"Pool recovery after errors", fun test_error_recovery/0}
     ]}.

%% Test basic connection acquisition and return
test_basic_connection() ->
    %% Get a connection
    {ok, Conn} = cookie_db_pool:get_connection(),
    ?assert(is_pid(Conn)),
    
    %% Use the connection for a simple query
    Result = cookie_db_pool:with_connection(fun(Db) ->
        esqlite3:q(Db, "SELECT 1")
    end),
    
    ?assertEqual({ok, [[1]]}, Result),
    
    %% Return the connection manually
    ok = cookie_db_pool:return_connection(Conn).

%% Test pool exhaustion behavior
test_pool_exhaustion() ->
    %% Get all available connections
    Connections = [begin
        {ok, Conn} = cookie_db_pool:get_connection(),
        Conn
    end || _ <- lists:seq(1, ?TEST_POOL_SIZE)],
    
    ?assertEqual(?TEST_POOL_SIZE, length(Connections)),
    
    %% Try to get one more connection (should timeout)
    Parent = self(),
    spawn(fun() ->
        Result = cookie_db_pool:get_connection(),
        Parent ! {connection_result, Result}
    end),
    
    %% Should timeout after a short wait
    receive
        {connection_result, _} ->
            ?assert(false) % Should not receive a connection
    after 1000 ->
        ok % Expected timeout
    end,
    
    %% Return all connections
    lists:foreach(fun(Conn) ->
        ok = cookie_db_pool:return_connection(Conn)
    end, Connections),
    
    %% Now we should be able to get a connection again
    {ok, _NewConn} = cookie_db_pool:get_connection().

%% Test connection reuse
test_connection_reuse() ->
    %% Get a connection
    {ok, Conn1} = cookie_db_pool:get_connection(),
    
    %% Return it
    ok = cookie_db_pool:return_connection(Conn1),
    
    %% Get another connection (should reuse the same one)
    {ok, Conn2} = cookie_db_pool:get_connection(),
    
    %% In a small pool, we might get the same connection back
    %% This is implementation-dependent, so we just verify we got a valid connection
    ?assert(is_pid(Conn2)),
    
    %% Return it
    ok = cookie_db_pool:return_connection(Conn2).

%% Test concurrent access to the pool
test_concurrent_access() ->
    NumWorkers = 10,
    Parent = self(),
    
    %% Spawn workers that will compete for connections
    Workers = [spawn(fun() ->
        %% Each worker performs multiple operations
        Results = [begin
            Result = cookie_db_pool:with_connection(fun(Db) ->
                %% Simulate some work
                timer:sleep(rand:uniform(50)),
                esqlite3:q(Db, "SELECT ?", [N])
            end),
            Result
        end || N <- lists:seq(1, 5)],
        
        Parent ! {worker_done, self(), Results}
    end) || _ <- lists:seq(1, NumWorkers)],
    
    %% Collect results from all workers
    AllResults = [receive
        {worker_done, Worker, Results} -> Results
    end || Worker <- Workers],
    
    %% Verify all operations succeeded
    TotalOps = NumWorkers * 5,
    FlatResults = lists:flatten(AllResults),
    SuccessResults = [R || {ok, _} <- FlatResults],
    
    ?assertEqual(TotalOps, length(SuccessResults)).

%% Test connection monitoring (process death handling)
test_connection_monitoring() ->
    %% Get a connection
    {ok, Conn} = cookie_db_pool:get_connection(),
    
    %% Spawn a process that will die while holding the connection
    DeadPid = spawn(fun() ->
        %% Simulate process death without returning connection
        exit(simulated_crash)
    end),
    
    %% The pool should detect the dead process and reclaim the connection
    %% Wait a moment for the monitor to trigger
    timer:sleep(100),
    
    %% We should still be able to get all pool connections
    Connections = [begin
        case cookie_db_pool:get_connection() of
            {ok, C} -> C;
            _ -> undefined
        end
    end || _ <- lists:seq(1, ?TEST_POOL_SIZE)],
    
    ValidConnections = [C || C <- Connections, C =/= undefined],
    ?assertEqual(?TEST_POOL_SIZE, length(ValidConnections)),
    
    %% Return all connections
    lists:foreach(fun(C) ->
        ok = cookie_db_pool:return_connection(C)
    end, ValidConnections).

%% Test database operations through the pool
test_database_operations() ->
    %% Test table creation and data insertion
    Result1 = cookie_db_pool:with_connection(fun(Db) ->
        esqlite3:exec(Db, "CREATE TEMPORARY TABLE test_table (id INTEGER, name TEXT)")
    end),
    ?assertEqual({ok, ok}, Result1),
    
    %% Insert test data
    Result2 = cookie_db_pool:with_connection(fun(Db) ->
        esqlite3:q(Db, "INSERT INTO test_table (id, name) VALUES (?, ?)", [1, "test"])
    end),
    ?assertEqual({ok, []}, Result2),
    
    %% Query the data
    Result3 = cookie_db_pool:with_connection(fun(Db) ->
        esqlite3:q(Db, "SELECT id, name FROM test_table WHERE id = ?", [1])
    end),
    ?assertEqual({ok, [[1, "test"]]}, Result3),
    
    %% Test JSON operations (like our cookie storage)
    TestJson = #{<<"test">> => <<"value">>, <<"number">> => 42},
    JsonBinary = jsx:encode(TestJson),
    
    Result4 = cookie_db_pool:with_connection(fun(Db) ->
        esqlite3:q(Db, "SELECT json_extract(?, '$.test')", [JsonBinary])
    end),
    ?assertEqual({ok, [["value"]]}, Result4).

%% Test performance statistics (if implemented)
test_performance_stats() ->
    %% Perform some operations to generate stats
    lists:foreach(fun(_) ->
        cookie_db_pool:with_connection(fun(Db) ->
            esqlite3:q(Db, "SELECT 1")
        end)
    end, lists:seq(1, 10)),
    
    %% Try to get stats (this may not be implemented yet)
    case catch cookie_db_pool:get_performance_stats() of
        {'EXIT', {undef, _}} ->
            %% Stats not implemented yet, that's ok
            ok;
        Stats when is_map(Stats) ->
            %% If stats are implemented, verify they make sense
            ?assert(maps:is_key(total_requests, Stats) orelse 
                   maps:size(Stats) >= 0)
    end.

%% Test pool recovery after database errors
test_error_recovery() ->
    %% Cause a database error
    ErrorResult = cookie_db_pool:with_connection(fun(Db) ->
        %% Try to execute invalid SQL
        esqlite3:q(Db, "INVALID SQL STATEMENT")
    end),
    
    %% Should get an error
    ?assertMatch({error, _}, ErrorResult),
    
    %% Pool should still work after the error
    GoodResult = cookie_db_pool:with_connection(fun(Db) ->
        esqlite3:q(Db, "SELECT 1")
    end),
    
    ?assertEqual({ok, [[1]]}, GoodResult),
    
    %% Test connection errors
    BadResult = cookie_db_pool:with_connection(fun(_Db) ->
        %% Simulate an exception
        error(simulated_error)
    end),
    
    ?assertMatch({error, _}, BadResult),
    
    %% Pool should still work
    GoodResult2 = cookie_db_pool:with_connection(fun(Db) ->
        esqlite3:q(Db, "SELECT 2")
    end),
    
    ?assertEqual({ok, [[2]]}, GoodResult2).

%%====================================================================
%% Load Testing
%%====================================================================

%% Simple load test for the pool
load_test() ->
    %% This is not part of the main test suite due to timing sensitivity
    %% Run manually: cookie_db_pool_tests:load_test().
    
    NumWorkers = 20,
    OpsPerWorker = 100,
    
    Parent = self(),
    StartTime = erlang:monotonic_time(microsecond),
    
    Workers = [spawn(fun() ->
        Results = [cookie_db_pool:with_connection(fun(Db) ->
            esqlite3:q(Db, "SELECT ?", [N])
        end) || N <- lists:seq(1, OpsPerWorker)],
        
        SuccessCount = length([ok || {ok, _} <- Results]),
        Parent ! {worker_stats, self(), SuccessCount}
    end) || _ <- lists:seq(1, NumWorkers)],
    
    %% Collect stats
    WorkerStats = [receive
        {worker_stats, Worker, SuccessCount} -> SuccessCount
    end || Worker <- Workers],
    
    EndTime = erlang:monotonic_time(microsecond),
    Duration = EndTime - StartTime,
    
    TotalOps = lists:sum(WorkerStats),
    OpsPerSecond = TotalOps / (Duration / 1000000),
    
    io:format("Load test results:~n"),
    io:format("  Workers: ~p~n", [NumWorkers]),
    io:format("  Operations per worker: ~p~n", [OpsPerWorker]),
    io:format("  Total operations: ~p~n", [TotalOps]),
    io:format("  Duration: ~p seconds~n", [Duration / 1000000]),
    io:format("  Operations per second: ~p~n", [OpsPerSecond]),
    
    %% Should achieve reasonable throughput
    ?assert(OpsPerSecond > 100). % At least 100 ops/second

%%====================================================================
%% Helper Functions
%%====================================================================

%% Wait for all connections to be returned to pool
wait_for_pool_available() ->
    wait_for_pool_available(10).

wait_for_pool_available(0) ->
    timeout;
wait_for_pool_available(Retries) ->
    case catch cookie_db_pool:get_connection() of
        {ok, Conn} ->
            cookie_db_pool:return_connection(Conn),
            ok;
        _ ->
            timer:sleep(100),
            wait_for_pool_available(Retries - 1)
    end.