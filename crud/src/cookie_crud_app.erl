%% =============================================================================
%% OTP Application Module - Application Lifecycle Management
%% =============================================================================
%% This module implements the OTP application behavior, similar to Go's main()
%% function but with more sophisticated lifecycle management.
%% 
%% For Go developers, key concepts:
%% - OTP Application: Like a service/daemon with startup, shutdown, and supervision
%% - Behavior: Like Go interfaces, but compile-time contracts
%% - -define: Like Go constants (#define PORT 8080 ~= const PORT = 8080)
%% - start/2, stop/1: Lifecycle callbacks (like main() and cleanup handlers)
%% 
%% Why OTP instead of just main()?
%% - Built-in supervision trees (automatic process restart)
%% - Hot code reloading (update running code without stopping)
%% - Distributed computing support
%% - Standard error handling and logging
%% =============================================================================

-module(cookie_crud_app).
-behaviour(application).  %% Implements OTP application interface

%% Application behavior callbacks
%% Go equivalent: these would be called by your service manager (systemd, etc.)
-export([start/2, stop/1]).

%% Compile-time constant (like Go's const PORT = 8080)
-define(PORT, 8080).

%% =============================================================================
%% Application Start Callback
%% =============================================================================
%% This is called when the application starts, similar to Go's main() function
%% but with more structure. It sets up the HTTP server, database, and supervision tree.
%% 
%% Go equivalent structure:
%% func main() {
%%     initDatabase()
%%     setupRoutes()
%%     startHTTPServer()
%%     startSupervisionTree()
%% }
%% =============================================================================

-spec start(application:start_type(), term()) -> {ok, pid()} | {error, term()}.
start(_StartType, _StartArgs) ->
    %% Initialize metrics collection (must be done before any requests)
    %% Go equivalent: prometheus.MustRegister(httpRequestsCounter, ...)
    cookie_metrics:init(),
    
    %% Initialize ETS cache (must be done before any requests)
    cookie_cache:init(),
    
    %% Initialize SQLite database (create tables if needed)
    %% Go equivalent: db, err := setupDatabase()
    cookie_db_pool:init_database(),
    
    %% Set up HTTP routing (like Go's mux.Router or http.ServeMux)
    %% Dispatch maps URL patterns to handler modules
    %% '_' means "match any host" (like "*" in Go routing)
    Dispatch = cowboy_router:compile([
        {'_', [
            %% Routes: {Pattern, Handler, InitialState}
            %% Go equivalent:
            %% router.HandleFunc("/cookies", handleCookies)
            %% router.HandleFunc("/cookies/{cookie}", handleCookie)
            %% router.HandleFunc("/metrics", handleMetrics)
            %% router.HandleFunc("/cache", handleCacheStatus)
            {"/cookies", cookie_crud, []},
            {"/cookies/:cookie", cookie_crud, []},
            {"/metrics", cookie_metrics_handler, []},
            {"/cache", cookie_cache_handler, []}
        ]}
    ]),
    
    %% Get port from application environment or use default
    Port = application:get_env(cookie_crud, port, ?PORT),
    
    %% Start HTTP server (like Go's http.ListenAndServe)
    %% start_clear = HTTP (not HTTPS)
    %% Returns {ok, ListenerPid} or {error, Reason}
    {ok, _} = cowboy:start_clear(http_listener,
        [{port, Port}],  %% Server options
        #{env => #{dispatch => Dispatch}}  %% Handler options
    ),
    
    %% Log server startup (like Go's log.Printf)
    io:format("Cookie CRUD API server started on port ~p~n", [Port]),
    
    %% Start the supervision tree (this is the main difference from Go)
    %% The supervisor will monitor and restart crashed processes
    %% Returns {ok, SupervisorPid} to indicate successful application start
    cookie_crud_sup:start_link().

%% =============================================================================
%% Application Stop Callback
%% =============================================================================
%% Called when the application is being shut down (like cleanup in Go's defer 
%% statements or signal handlers). Ensures graceful shutdown of all resources.
%% =============================================================================

-spec stop(term()) -> ok.
stop(_State) ->
    %% Stop the HTTP listener gracefully
    %% Go equivalent: server.Shutdown(context.Background())
    ok = cowboy:stop_listener(http_listener),
    %% Return ok to indicate successful shutdown
    %% The supervision tree will automatically stop its child processes
    ok.