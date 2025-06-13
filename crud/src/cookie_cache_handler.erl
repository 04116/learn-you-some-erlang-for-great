%% =============================================================================
%% Cookie Cache Status Handler - Cache and Cluster Monitoring
%% =============================================================================
%% This module handles the /cache endpoint for monitoring cache and cluster status.
%% Provides detailed information about cache performance, cluster health, and
%% distributed state across all nodes.
%% =============================================================================

-module(cookie_cache_handler).
-behaviour(cowboy_handler).

%% Cowboy handler callbacks
-export([init/2, terminate/3]).

%% =============================================================================
%% HTTP Handler Implementation
%% =============================================================================

-spec init(cowboy_req:req(), term()) -> {ok, cowboy_req:req(), term()}.
init(Req, State) ->
    Method = cowboy_req:method(Req),

    Req2 = case Method of
        <<"GET">> ->
            handle_cache_status(Req);
        <<"DELETE">> ->
            handle_cache_clear(Req);
        _ ->
            cowboy_req:reply(405,
                #{<<"content-type">> => <<"application/json">>},
                jsx:encode(#{error => <<"Method Not Allowed">>}),
                Req)
    end,

    {ok, Req2, State}.

-spec terminate(term(), cowboy_req:req(), term()) -> ok.
terminate(_Reason, _Req, _State) ->
    ok.

%% =============================================================================
%% Internal Functions
%% =============================================================================

-spec handle_cache_status(cowboy_req:req()) -> cowboy_req:req().
handle_cache_status(Req) ->
    %% Gather comprehensive cache and cluster status
    CacheStats = cookie_cache:get_stats(),
    ClusterStatus = case whereis(cookie_cluster) of
        undefined -> #{error => <<"Cluster manager not running">>};
        _ -> cookie_cluster:get_cluster_status()
    end,

    %% Collect status from all cluster nodes
    ClusterNodes = maps:get(cluster_nodes, ClusterStatus, []),
    NodeStatuses = collect_node_statuses(ClusterNodes),

    StatusData = #{
        cache_stats => CacheStats,
        cluster_status => ClusterStatus,
        node_statuses => NodeStatuses,
        local_node => node(),
        timestamp => erlang:system_time(second)
    },

    Json = jsx:encode(StatusData),
    Headers = #{<<"content-type">> => <<"application/json">>},
    cowboy_req:reply(200, Headers, Json, Req).

-spec handle_cache_clear(cowboy_req:req()) -> cowboy_req:req().
handle_cache_clear(Req) ->
    %% Clear the local cache
    cookie_cache:clear(),

    Response = #{
        message => <<"Cache cleared successfully">>,
        timestamp => erlang:system_time(second)
    },

    Json = jsx:encode(Response),
    Headers = #{<<"content-type">> => <<"application/json">>},
    cowboy_req:reply(200, Headers, Json, Req).

-spec collect_node_statuses([node()]) -> #{node() => term()}.
collect_node_statuses(Nodes) ->
    %% Collect cache stats from all cluster nodes
    maps:from_list([
        {Node, get_remote_cache_stats(Node)} || Node <- Nodes
    ]).

-spec get_remote_cache_stats(node()) -> term().
get_remote_cache_stats(Node) ->
    try
        case rpc:call(Node, cookie_cache, get_stats, [], 5000) of
            {badrpc, Reason} ->
                #{error => Reason, status => unreachable};
            Stats when is_map(Stats) ->
                maps:put(status, ok, Stats);
            Other ->
                #{error => Other, status => error}
        end
    catch
        _:Error ->
            #{error => Error, status => exception}
    end.
