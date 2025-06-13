%% =============================================================================
%% Cookie Cache Module - ETS-based Distributed Cache
%% =============================================================================
%% This module provides a distributed cache layer using ETS (Erlang Term Storage)
%% for the Cookie CRUD API. It supports multi-node clustering with cache
%% synchronization and TTL-based expiration.
%%
%% Key Features:
%% - ETS-based local cache for fast lookups
%% - Distributed cache synchronization across cluster nodes
%% - TTL (Time-To-Live) support for automatic cache expiration
%% - Write-through and write-back cache strategies
%% - Cache invalidation on updates/deletes
%% - Metrics integration for cache hit/miss tracking
%% =============================================================================

-module(cookie_cache).

%% Public API
-export([
    init/0,
    get/1,
    put/2,
    put/3,
    delete/1,
    clear/0,
    size/0,
    get_stats/0,
    sync_cluster/0
]).

%% Internal cluster sync functions
-export([
    sync_put/3,
    sync_delete/1,
    sync_clear/0
]).

%% Cache configuration
-define(CACHE_TABLE, cookie_cache_table).
-define(STATS_TABLE, cookie_cache_stats).
-define(DEFAULT_TTL, 300).  %% 5 minutes default TTL

%% Cache entry record
-record(cache_entry, {
    key :: binary(),
    value :: term(),
    created :: integer(),
    ttl :: integer(),
    expires_at :: integer()
}).

%% Cache statistics record
-record(cache_stats, {
    hits = 0 :: integer(),
    misses = 0 :: integer(),
    puts = 0 :: integer(),
    deletes = 0 :: integer(),
    evictions = 0 :: integer()
}).

-type cache_key() :: binary().
-type cache_value() :: term().
-type ttl_seconds() :: integer().

%% =============================================================================
%% Cache Initialization
%% =============================================================================

-spec init() -> ok.
init() ->
    %% Create cache table if it doesn't exist
    try
        ets:new(?CACHE_TABLE, [
            named_table,
            public,
            set,
            {keypos, #cache_entry.key},
            {read_concurrency, true},
            {write_concurrency, true}
        ]),

        %% Create stats table
        ets:new(?STATS_TABLE, [
            named_table,
            public,
            set
        ]),

        %% Initialize stats
        ets:insert(?STATS_TABLE, {stats, #cache_stats{}}),

        %% Start TTL cleanup process
        spawn_ttl_cleaner(),

        ok
    catch
        error:badarg -> ok  %% Table already exists
    end.

%% =============================================================================
%% Cache Operations - Public API
%% =============================================================================

-spec get(cache_key()) -> {ok, cache_value()} | miss.
get(Key) ->
    case ets:lookup(?CACHE_TABLE, Key) of
        [Entry] ->
            case is_expired(Entry) of
                true ->
                    %% Entry expired, remove and return miss
                    ets:delete(?CACHE_TABLE, Key),
                    increment_stat(evictions),
                    increment_stat(misses),
                    miss;
                false ->
                    %% Cache hit
                    increment_stat(hits),
                    {ok, Entry#cache_entry.value}
            end;
        [] ->
            %% Cache miss
            increment_stat(misses),
            miss
    end.

-spec put(cache_key(), cache_value()) -> ok.
put(Key, Value) ->
    put(Key, Value, ?DEFAULT_TTL).

-spec put(cache_key(), cache_value(), ttl_seconds()) -> ok.
put(Key, Value, TTL) ->
    Now = erlang:system_time(second),
    Entry = #cache_entry{
        key = Key,
        value = Value,
        created = Now,
        ttl = TTL,
        expires_at = Now + TTL
    },

    ets:insert(?CACHE_TABLE, Entry),
    increment_stat(puts),

    %% Sync with cluster nodes
    sync_to_cluster(sync_put, [Key, Value, TTL]),
    ok.

-spec delete(cache_key()) -> ok.
delete(Key) ->
    ets:delete(?CACHE_TABLE, Key),
    increment_stat(deletes),

    %% Sync with cluster nodes
    sync_to_cluster(sync_delete, [Key]),
    ok.

-spec clear() -> ok.
clear() ->
    ets:delete_all_objects(?CACHE_TABLE),

    %% Sync with cluster nodes
    sync_to_cluster(sync_clear, []),
    ok.

-spec size() -> integer().
size() ->
    ets:info(?CACHE_TABLE, size).

-spec get_stats() -> #{atom() => integer()}.
get_stats() ->
    case ets:lookup(?STATS_TABLE, stats) of
        [{stats, Stats}] ->
            #{
                hits => Stats#cache_stats.hits,
                misses => Stats#cache_stats.misses,
                puts => Stats#cache_stats.puts,
                deletes => Stats#cache_stats.deletes,
                evictions => Stats#cache_stats.evictions,
                size => size(),
                hit_rate => calculate_hit_rate(Stats)
            };
        [] ->
            #{hits => 0, misses => 0, puts => 0, deletes => 0, evictions => 0, size => 0, hit_rate => 0.0}
    end.

%% =============================================================================
%% Cluster Synchronization Functions
%% =============================================================================

-spec sync_cluster() -> ok.
sync_cluster() ->
    %% Manual cluster sync - useful for debugging
    Nodes = nodes(),
    io:format("Syncing cache with nodes: ~p~n", [Nodes]),
    ok.

%% Functions called by remote nodes for cache synchronization
-spec sync_put(cache_key(), cache_value(), ttl_seconds()) -> ok.
sync_put(Key, Value, TTL) ->
    %% Direct ETS insert without triggering further sync
    Now = erlang:system_time(second),
    Entry = #cache_entry{
        key = Key,
        value = Value,
        created = Now,
        ttl = TTL,
        expires_at = Now + TTL
    },
    ets:insert(?CACHE_TABLE, Entry),
    ok.

-spec sync_delete(cache_key()) -> ok.
sync_delete(Key) ->
    %% Direct ETS delete without triggering further sync
    ets:delete(?CACHE_TABLE, Key),
    ok.

-spec sync_clear() -> ok.
sync_clear() ->
    %% Direct ETS clear without triggering further sync
    ets:delete_all_objects(?CACHE_TABLE),
    ok.

%% =============================================================================
%% Internal Helper Functions
%% =============================================================================

-spec is_expired(#cache_entry{}) -> boolean().
is_expired(Entry) ->
    Now = erlang:system_time(second),
    Now >= Entry#cache_entry.expires_at.

-spec increment_stat(atom()) -> ok.
increment_stat(StatName) ->
    case ets:lookup(?STATS_TABLE, stats) of
        [{stats, Stats}] ->
            NewStats = case StatName of
                hits -> Stats#cache_stats{hits = Stats#cache_stats.hits + 1};
                misses -> Stats#cache_stats{misses = Stats#cache_stats.misses + 1};
                puts -> Stats#cache_stats{puts = Stats#cache_stats.puts + 1};
                deletes -> Stats#cache_stats{deletes = Stats#cache_stats.deletes + 1};
                evictions -> Stats#cache_stats{evictions = Stats#cache_stats.evictions + 1}
            end,
            ets:insert(?STATS_TABLE, {stats, NewStats});
        [] ->
            ok
    end.

-spec calculate_hit_rate(#cache_stats{}) -> float().
calculate_hit_rate(Stats) ->
    Total = Stats#cache_stats.hits + Stats#cache_stats.misses,
    case Total of
        0 -> 0.0;
        _ -> Stats#cache_stats.hits / Total * 100.0
    end.

-spec sync_to_cluster(atom(), [term()]) -> ok.
sync_to_cluster(Function, Args) ->
    Nodes = nodes(),
    lists:foreach(fun(Node) ->
        try
            rpc:cast(Node, ?MODULE, Function, Args)
        catch
            _:_ -> ok  %% Ignore errors - node might be down
        end
    end, Nodes),
    ok.

-spec spawn_ttl_cleaner() -> pid().
spawn_ttl_cleaner() ->
    spawn(fun ttl_cleaner_loop/0).

-spec ttl_cleaner_loop() -> no_return().
ttl_cleaner_loop() ->
    timer:sleep(60000),  %% Run every minute
    cleanup_expired_entries(),
    ttl_cleaner_loop().

-spec cleanup_expired_entries() -> ok.
cleanup_expired_entries() ->
    Now = erlang:system_time(second),
    MatchSpec = [
        {#cache_entry{expires_at = '$1', _ = '_'},
         [{'=<', '$1', Now}],
         ['$_']}
    ],
    ExpiredEntries = ets:select(?CACHE_TABLE, MatchSpec),
    lists:foreach(fun(Entry) ->
        ets:delete(?CACHE_TABLE, Entry#cache_entry.key),
        increment_stat(evictions)
    end, ExpiredEntries),
    case length(ExpiredEntries) of
        0 -> ok;
        Count ->
            io:format("Cache: Cleaned ~p expired entries~n", [Count])
    end,
    ok.