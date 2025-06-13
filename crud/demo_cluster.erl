#!/usr/bin/env escript
%% =============================================================================
%% Cookie CRUD Cluster Demo Script
%% =============================================================================
%% This script demonstrates the ETS-based caching and clustering functionality
%% without needing to start full HTTP servers. It shows cache operations,
%% TTL expiration, and basic clustering concepts.
%% =============================================================================

main(_) ->
    %% Add the compiled beam files to the path
    Path = "_build/default/lib/cookie_crud/ebin",
    code:add_path(Path),

    io:format("~n=== Cookie CRUD Cluster Demo ===~n~n"),

    %% Demo 1: ETS Cache Operations
    demo_cache_operations(),

    %% Demo 2: Cache TTL and Expiration
    demo_cache_ttl(),

    %% Demo 3: Cache Statistics
    demo_cache_stats(),

    %% Demo 4: Cache Performance
    demo_cache_performance(),

    io:format("~n=== Demo Complete ===~n").

%% =============================================================================
%% Cache Operations Demo
%% =============================================================================

demo_cache_operations() ->
    io:format("1. Cache Operations Demo~n"),
    io:format("-------------------------~n"),

    %% Initialize the cache
    ok = cookie_cache:init(),
    io:format("✓ Cache initialized~n"),

    %% Test basic put/get operations
    ok = cookie_cache:put(<<"user:123">>, #{name => <<"Alice">>, age => 30}),
    io:format("✓ Stored user data for key 'user:123'~n"),

    case cookie_cache:get(<<"user:123">>) of
        {ok, UserData} ->
            io:format("✓ Retrieved user data: ~p~n", [UserData]);
        miss ->
            io:format("✗ Cache miss for user:123~n")
    end,

    %% Test cache miss
    case cookie_cache:get(<<"nonexistent">>) of
        miss ->
            io:format("✓ Cache miss for non-existent key~n");
        {ok, _} ->
            io:format("✗ Unexpected cache hit~n")
    end,

    %% Test cache deletion
    ok = cookie_cache:delete(<<"user:123">>),
    io:format("✓ Deleted user:123 from cache~n"),

    case cookie_cache:get(<<"user:123">>) of
        miss ->
            io:format("✓ Confirmed deletion - cache miss~n");
        {ok, _} ->
            io:format("✗ Key still exists after deletion~n")
    end,

    io:format("~n").

%% =============================================================================
%% Cache TTL Demo
%% =============================================================================

demo_cache_ttl() ->
    io:format("2. Cache TTL (Time-To-Live) Demo~n"),
    io:format("----------------------------------~n"),

    %% Store with short TTL
    TTL = 2, %% 2 seconds
    ok = cookie_cache:put(<<"temp:data">>, <<"This will expire">>, TTL),
    io:format("✓ Stored temporary data with ~p second TTL~n", [TTL]),

    %% Immediate retrieval should work
    case cookie_cache:get(<<"temp:data">>) of
        {ok, Data} ->
            io:format("✓ Immediate retrieval: ~p~n", [Data]);
        miss ->
            io:format("✗ Unexpected cache miss~n")
    end,

    %% Wait for expiration
    io:format("⏳ Waiting ~p seconds for TTL expiration...~n", [TTL + 1]),
    timer:sleep((TTL + 1) * 1000),

    %% Should be expired now
    case cookie_cache:get(<<"temp:data">>) of
        miss ->
            io:format("✓ Data expired as expected~n");
        {ok, _} ->
            io:format("✗ Data should have expired~n")
    end,

    io:format("~n").

%% =============================================================================
%% Cache Statistics Demo
%% =============================================================================

demo_cache_stats() ->
    io:format("3. Cache Statistics Demo~n"),
    io:format("-------------------------~n"),

    %% Clear cache to reset stats
    ok = cookie_cache:clear(),

    %% Generate some cache activity
    lists:foreach(fun(N) ->
        Key = list_to_binary("key" ++ integer_to_list(N)),
        Value = #{number => N, square => N * N},
        ok = cookie_cache:put(Key, Value, 60)
    end, lists:seq(1, 5)),

    io:format("✓ Stored 5 cache entries~n"),

    %% Generate some hits and misses
    cookie_cache:get(<<"key1">>),  %% Hit
    cookie_cache:get(<<"key3">>),  %% Hit
    cookie_cache:get(<<"key5">>),  %% Hit
    cookie_cache:get(<<"missing1">>),  %% Miss
    cookie_cache:get(<<"missing2">>),  %% Miss

    %% Get statistics
    Stats = cookie_cache:get_stats(),
    io:format("✓ Cache Statistics:~n"),
    io:format("  - Hits: ~p~n", [maps:get(hits, Stats, 0)]),
    io:format("  - Misses: ~p~n", [maps:get(misses, Stats, 0)]),
    io:format("  - Puts: ~p~n", [maps:get(puts, Stats, 0)]),
    io:format("  - Cache Size: ~p entries~n", [maps:get(size, Stats, 0)]),
    io:format("  - Hit Rate: ~.1f%~n", [maps:get(hit_rate, Stats, 0.0)]),

    io:format("~n").

%% =============================================================================
%% Cache Performance Demo
%% =============================================================================

demo_cache_performance() ->
    io:format("4. Cache Performance Demo~n"),
    io:format("--------------------------~n"),

    %% Test performance with larger dataset
    NumEntries = 1000,
    io:format("⏳ Storing ~p entries...~n", [NumEntries]),

    {TimeWrite, _} = timer:tc(fun() ->
        lists:foreach(fun(N) ->
            Key = list_to_binary("perf" ++ integer_to_list(N)),
            Value = #{id => N, data => crypto:strong_rand_bytes(100)},
            ok = cookie_cache:put(Key, Value, 300)
        end, lists:seq(1, NumEntries))
    end),

    io:format("✓ Write performance: ~.2f ms for ~p entries (~.2f entries/ms)~n",
              [TimeWrite/1000, NumEntries, NumEntries/(TimeWrite/1000)]),

    %% Test read performance
    Keys = [list_to_binary("perf" ++ integer_to_list(N)) || N <- lists:seq(1, NumEntries)],

    {TimeRead, Hits} = timer:tc(fun() ->
        lists:foldl(fun(Key, Acc) ->
            case cookie_cache:get(Key) of
                {ok, _} -> Acc + 1;
                miss -> Acc
            end
        end, 0, Keys)
    end),

    io:format("✓ Read performance: ~.2f ms for ~p lookups (~.2f lookups/ms)~n",
              [TimeRead/1000, NumEntries, NumEntries/(TimeRead/1000)]),
    io:format("✓ Cache hits: ~p/~p (~.1f%)~n",
              [Hits, NumEntries, (Hits/NumEntries)*100]),

    %% Final stats
    FinalStats = cookie_cache:get_stats(),
    io:format("✓ Final cache size: ~p entries~n", [maps:get(size, FinalStats, 0)]),

    io:format("~n").
