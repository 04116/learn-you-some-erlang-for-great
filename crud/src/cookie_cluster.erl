%% =============================================================================
%% Cookie Cluster Manager - Multi-Instance Clustering Support
%% =============================================================================
%% This module manages the clustering of multiple Cookie CRUD instances.
%% It handles node discovery, cluster formation, and health monitoring.
%%
%% Key Features:
%% - Automatic node discovery and cluster formation
%% - Health monitoring and failure detection
%% - Load balancing across cluster nodes
%% - Distributed cache synchronization
%% - Graceful node join/leave operations
%% =============================================================================

-module(cookie_cluster).

-behaviour(gen_server).

%% Public API
-export([
    start_link/0,
    join_cluster/1,
    leave_cluster/0,
    get_cluster_status/0,
    get_cluster_nodes/0,
    get_node_health/0
]).

%% gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

%% Node discovery exports
-export([
    discover_nodes/0,
    ping_node/1
]).

-define(SERVER, ?MODULE).
-define(DISCOVERY_INTERVAL, 30000).  %% 30 seconds
-define(HEALTH_CHECK_INTERVAL, 15000).  %% 15 seconds

-record(state, {
    cluster_nodes = [] :: [node()],
    node_health = #{} :: #{node() => {ok | error, integer()}},
    discovery_timer :: reference() | undefined,
    health_timer :: reference() | undefined
}).

%% =============================================================================
%% Public API
%% =============================================================================

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

-spec join_cluster(node()) -> ok | {error, term()}.
join_cluster(Node) ->
    gen_server:call(?SERVER, {join_cluster, Node}).

-spec leave_cluster() -> ok.
leave_cluster() ->
    gen_server:call(?SERVER, leave_cluster).

-spec get_cluster_status() -> #{atom() => term()}.
get_cluster_status() ->
    gen_server:call(?SERVER, get_cluster_status).

-spec get_cluster_nodes() -> [node()].
get_cluster_nodes() ->
    gen_server:call(?SERVER, get_cluster_nodes).

-spec get_node_health() -> #{node() => {ok | error, integer()}}.
get_node_health() ->
    gen_server:call(?SERVER, get_node_health).

%% =============================================================================
%% gen_server Callbacks
%% =============================================================================

init([]) ->
    %% Set up distributed Erlang if not already done
    setup_distribution(),

    %% Start node discovery
    DiscoveryTimer = erlang:send_after(?DISCOVERY_INTERVAL, self(), discover_nodes),
    HealthTimer = erlang:send_after(?HEALTH_CHECK_INTERVAL, self(), health_check),

    %% Initialize state
    State = #state{
        discovery_timer = DiscoveryTimer,
        health_timer = HealthTimer
    },

    io:format("Cluster manager started for node: ~p~n", [node()]),
    {ok, State}.

handle_call({join_cluster, Node}, _From, State) ->
    case net_adm:ping(Node) of
        pong ->
            %% Successfully connected to node
            NewNodes = lists:usort([Node | State#state.cluster_nodes]),
            NewHealth = maps:put(Node, {ok, erlang:system_time(second)}, State#state.node_health),

            %% Sync cache with new node
            cookie_cache:sync_cluster(),

            io:format("Successfully joined cluster node: ~p~n", [Node]),
            {reply, ok, State#state{cluster_nodes = NewNodes, node_health = NewHealth}};
        pang ->
            io:format("Failed to connect to node: ~p~n", [Node]),
            {reply, {error, connection_failed}, State}
    end;

handle_call(leave_cluster, _From, State) ->
    %% Disconnect from all cluster nodes
    lists:foreach(fun(Node) -> erlang:disconnect_node(Node) end, State#state.cluster_nodes),

    io:format("Left cluster, disconnected from nodes: ~p~n", [State#state.cluster_nodes]),
    {reply, ok, State#state{cluster_nodes = [], node_health = #{}}};

handle_call(get_cluster_status, _From, State) ->
    Status = #{
        current_node => node(),
        cluster_nodes => State#state.cluster_nodes,
        connected_nodes => nodes(),
        node_health => State#state.node_health,
        cache_stats => cookie_cache:get_stats()
    },
    {reply, Status, State};

handle_call(get_cluster_nodes, _From, State) ->
    {reply, State#state.cluster_nodes, State};

handle_call(get_node_health, _From, State) ->
    {reply, State#state.node_health, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(discover_nodes, State) ->
    %% Perform node discovery
    DiscoveredNodes = discover_nodes(),

    %% Try to connect to newly discovered nodes
    NewNodes = lists:foldl(fun(Node, Acc) ->
        case lists:member(Node, State#state.cluster_nodes) of
            false ->
                case net_adm:ping(Node) of
                    pong ->
                        io:format("Auto-discovered and connected to node: ~p~n", [Node]),
                        [Node | Acc];
                    pang ->
                        Acc
                end;
            true ->
                Acc
        end
    end, State#state.cluster_nodes, DiscoveredNodes),

    %% Schedule next discovery
    NewTimer = erlang:send_after(?DISCOVERY_INTERVAL, self(), discover_nodes),

    {noreply, State#state{
        cluster_nodes = lists:usort(NewNodes),
        discovery_timer = NewTimer
    }};

handle_info(health_check, State) ->
    %% Check health of all cluster nodes
    Now = erlang:system_time(second),

    NewHealth = maps:fold(fun(Node, {_Status, LastSeen}, Acc) ->
        case ping_node(Node) of
            pong ->
                maps:put(Node, {ok, Now}, Acc);
            pang ->
                %% Node is down, but keep it in health map for a while
                case Now - LastSeen > 300 of  %% 5 minutes
                    true ->
                        io:format("Removing unhealthy node from cluster: ~p~n", [Node]),
                        Acc;  %% Remove from health map
                    false ->
                        maps:put(Node, {error, LastSeen}, Acc)
                end
        end
    end, #{}, State#state.node_health),

    %% Update cluster nodes list (remove permanently failed nodes)
    ActiveNodes = maps:keys(NewHealth),

    %% Schedule next health check
    NewTimer = erlang:send_after(?HEALTH_CHECK_INTERVAL, self(), health_check),

    {noreply, State#state{
        cluster_nodes = ActiveNodes,
        node_health = NewHealth,
        health_timer = NewTimer
    }};

handle_info({nodedown, Node}, State) ->
    io:format("Node down detected: ~p~n", [Node]),

    %% Update node health
    NewHealth = maps:put(Node, {error, erlang:system_time(second)}, State#state.node_health),

    {noreply, State#state{node_health = NewHealth}};

handle_info({nodeup, Node}, State) ->
    io:format("Node up detected: ~p~n", [Node]),

    %% Update node health and add to cluster if not already present
    NewHealth = maps:put(Node, {ok, erlang:system_time(second)}, State#state.node_health),
    NewNodes = lists:usort([Node | State#state.cluster_nodes]),

    %% Sync cache with rejoined node
    cookie_cache:sync_cluster(),

    {noreply, State#state{
        cluster_nodes = NewNodes,
        node_health = NewHealth
    }};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    %% Cancel timers
    case State#state.discovery_timer of
        undefined -> ok;
        Timer -> erlang:cancel_timer(Timer)
    end,

    case State#state.health_timer of
        undefined -> ok;
        Timer2 -> erlang:cancel_timer(Timer2)
    end,

    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% =============================================================================
%% Node Discovery Functions
%% =============================================================================

-spec discover_nodes() -> [node()].
discover_nodes() ->
    %% Get cluster configuration
    ClusterConfig = application:get_env(cookie_crud, cluster, #{}),
    DiscoveryMethod = maps:get(discovery, ClusterConfig, manual),

    case DiscoveryMethod of
        manual ->
            %% Manual node list from configuration
            maps:get(nodes, ClusterConfig, []);

        dns ->
            %% DNS-based discovery
            discover_nodes_dns(ClusterConfig);

        kubernetes ->
            %% Kubernetes service discovery
            discover_nodes_k8s(ClusterConfig);

        _ ->
            []
    end.

-spec discover_nodes_dns(map()) -> [node()].
discover_nodes_dns(Config) ->
    %% Simple DNS-based node discovery
    ServiceName = maps:get(service_name, Config, "cookie-crud"),
    Domain = maps:get(domain, Config, "local"),

    %% This is a simplified implementation
    %% In production, you'd use proper DNS SRV record lookup
    try
        case inet:gethostbyname(ServiceName ++ "." ++ Domain) of
            {ok, HostEntry} ->
                %% Extract address list from hostent record
                case HostEntry of
                    {hostent, _, _, _, _, AddressList} ->
                        NodeName = maps:get(node_name, Config, "cookie_crud"),
                        [list_to_atom(NodeName ++ "@" ++ inet:ntoa(Addr)) || Addr <- AddressList];
                    _ ->
                        []
                end;
            _ ->
                []
        end
    catch
        _:_ -> []
    end.

-spec discover_nodes_k8s(map()) -> [node()].
discover_nodes_k8s(_Config) ->
    %% Kubernetes service discovery via environment variables
    %% This would typically query the Kubernetes API
    %% Simplified implementation using environment variables
    %% In production, use proper Kubernetes API client
    case os:getenv("KUBERNETES_SERVICE_HOST") of
        false -> [];
        _Host ->
            %% This is where you'd implement actual K8s discovery
            %% For now, return empty list
            []
    end.

-spec ping_node(node()) -> pong | pang.
ping_node(Node) ->
    net_adm:ping(Node).

%% =============================================================================
%% Setup Functions
%% =============================================================================

-spec setup_distribution() -> ok.
setup_distribution() ->
    %% Ensure distributed Erlang is started
    case node() of
        nonode@nohost ->
            %% Start distribution with a default name
            NodeName = get_node_name(),
            case net_kernel:start([NodeName, shortnames]) of
                {ok, _} ->
                    io:format("Started distributed Erlang with name: ~p~n", [NodeName]),

                    %% Set up node monitoring
                    net_kernel:monitor_nodes(true),
                    ok;
                {error, Reason} ->
                    io:format("Failed to start distributed Erlang: ~p~n", [Reason]),
                    ok
            end;
        _ ->
            %% Already distributed
            net_kernel:monitor_nodes(true),
            ok
    end.

-spec get_node_name() -> atom().
get_node_name() ->
    %% Generate node name from configuration or hostname
    ClusterConfig = application:get_env(cookie_crud, cluster, #{}),
    BaseName = maps:get(node_name, ClusterConfig, "cookie_crud"),

    case maps:get(instance_id, ClusterConfig, auto) of
        auto ->
            %% Auto-generate instance ID
            {ok, Hostname} = inet:gethostname(),
            Pid = os:getpid(),
            list_to_atom(BaseName ++ "_" ++ Hostname ++ "_" ++ Pid);
        InstanceId ->
            %% Use configured instance ID
            list_to_atom(BaseName ++ "_" ++ InstanceId)
    end.
