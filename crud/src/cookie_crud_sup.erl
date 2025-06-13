%% =============================================================================
%% OTP Supervisor Module - Process Supervision and Fault Tolerance
%% =============================================================================
%% This module implements the OTP supervisor behavior, providing automatic
%% process monitoring, restarting, and fault isolation.
%% 
%% For Go developers, this is a completely new concept! Go doesn't have built-in
%% supervision trees. Here's what this provides:
%% 
%% What Go developers usually handle manually:
%% - Process crashes: manual error handling, panic recovery
%% - Service restart: external process managers (systemd, Docker, k8s)
%% - Health checks: custom monitoring, liveness probes
%% - Circuit breaker: libraries like hystrix-go
%% 
%% What Erlang supervisors provide automatically:
%% - Automatic process restart on crashes
%% - Configurable restart strategies (one_for_one, one_for_all, etc.)
%% - Backoff and rate limiting for restarts
%% - Hierarchical supervision trees
%% - Built-in fault isolation
%% 
%% Think of it like a combination of:
%% - systemd (process management)
%% - Kubernetes (restart policies)
%% - Circuit breakers (fault tolerance)
%% - Health checks (monitoring)
%% =============================================================================

-module(cookie_crud_sup).
-behaviour(supervisor).  %% Implements OTP supervisor interface

%% Public API
-export([start_link/0]).
%% Supervisor behavior callback
-export([init/1]).

%% Supervisor process name (registered name for this supervisor)
%% Like Go's service registration in a service mesh
-define(SERVER, ?MODULE).

%% =============================================================================
%% Supervisor Startup Function
%% =============================================================================
%% Starts the supervisor process and registers it with a local name.
%% This is called by the application module during startup.
%% =============================================================================

-spec start_link() -> {ok, pid()} | ignore | {error, term()}.
start_link() ->
    %% Start supervisor and register it locally with name ?SERVER
    %% {local, Name} registers the process locally (within this node)
    %% ?MODULE refers to this module's init/1 function for configuration
    %% [] is the initial argument passed to init/1
    %% 
    %% Go equivalent concept:
    %% supervisor := NewSupervisor()
    %% serviceRegistry.Register("cookie_crud_sup", supervisor)
    %% return supervisor.Start()
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% =============================================================================
%% Supervisor Configuration Callback
%% =============================================================================
%% This function defines how the supervisor should manage its child processes.
%% It's like defining a Kubernetes deployment or Docker Compose service config.
%% =============================================================================

-spec init([]) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init([]) ->
    %% Supervisor flags - define restart strategy and limits
    %% This is like configuring restart policies in Kubernetes
    SupFlags = #{
        %% Restart strategy: one_for_one means if one child crashes,
        %% only restart that child (not all children)
        %% Go equivalent: isolated failure handling, no cascading failures
        %% Other strategies:
        %% - one_for_all: restart all children if one crashes
        %% - rest_for_one: restart the failed child and all children started after it
        strategy => one_for_one,
        
        %% Intensity: maximum number of restarts allowed in Period
        %% If more than 5 restarts happen in 10 seconds, supervisor gives up
        %% Go equivalent: circuit breaker with failure threshold
        intensity => 5,
        
        %% Period: time window for intensity limit (in seconds)
        %% Like a sliding window for failure rate limiting
        period => 10
    },

    %% Child specifications - define what processes to supervise
    %% Each child spec is like a service definition in Docker Compose
    ChildSpecs = [
        #{
            %% Database connection pool
            id => cookie_db_pool,
            start => {cookie_db_pool, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [cookie_db_pool]
        },
        #{
            %% Cluster manager for multi-instance coordination
            id => cookie_cluster,
            start => {cookie_cluster, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [cookie_cluster]
        }
    ],

    %% Return supervisor configuration
    %% {ok, {SupervisorFlags, ChildSpecifications}}
    {ok, {SupFlags, ChildSpecs}}.