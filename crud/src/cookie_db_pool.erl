-module(cookie_db_pool).
-behaviour(gen_server).

%% API
-export([start_link/0, get_connection/0, return_connection/1, with_connection/1]).
-export([init_database/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(POOL_SIZE, 10).
-define(DB_FILE, "cookies.db").
-define(TIMEOUT, 5000).

-record(state, {
    available = [] :: [esqlite3:esqlite3()],
    in_use = #{} :: #{esqlite3:esqlite3() => reference()},
    waiting = [] :: [{from, reference()}]
}).

-type db_connection() :: esqlite3:esqlite3().

%%%===================================================================
%%% API
%%%===================================================================

-spec start_link() -> {ok, pid()} | ignore | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec get_connection() -> {ok, db_connection()} | {error, timeout}.
get_connection() ->
    gen_server:call(?MODULE, get_connection, ?TIMEOUT).

-spec return_connection(db_connection()) -> ok.
return_connection(Conn) ->
    gen_server:cast(?MODULE, {return_connection, Conn}).

-spec with_connection(fun((db_connection()) -> T)) -> {ok, T} | {error, term()}.
with_connection(Fun) ->
    case get_connection() of
        {ok, Conn} ->
            try
                Result = Fun(Conn),
                return_connection(Conn),
                {ok, Result}
            catch
                Class:Reason:Stacktrace ->
                    return_connection(Conn),
                    {error, {Class, Reason, Stacktrace}}
            end;
        Error ->
            Error
    end.

-spec init_database() -> ok.
init_database() ->
    {ok, Db} = esqlite3:open(?DB_FILE),
    
    %% Enable WAL mode for better concurrency
    ok = esqlite3:exec(Db, "PRAGMA journal_mode = WAL;"),
    
    %% Optimize SQLite for server workloads
    OptimizationPragmas = [
        %% WAL mode optimizations
        "PRAGMA synchronous = NORMAL;",     % Faster than FULL, safer than OFF
        "PRAGMA wal_autocheckpoint = 1000;", % Checkpoint every 1000 pages
        "PRAGMA wal_checkpoint(TRUNCATE);",  % Clean up WAL file
        
        %% Performance optimizations
        "PRAGMA cache_size = -64000;",      % 64MB cache (negative = KB)
        "PRAGMA temp_store = MEMORY;",      % Store temp tables in memory
        "PRAGMA mmap_size = 134217728;",    % 128MB memory-mapped I/O
        
        %% Concurrency optimizations
        "PRAGMA busy_timeout = 5000;",      % 5 second busy timeout
        "PRAGMA foreign_keys = ON;",        % Enable foreign key constraints
        
        %% Write optimizations
        "PRAGMA optimize;",                 % Analyze and optimize database
        
        %% Checkpoint optimization
        "PRAGMA wal_checkpoint(PASSIVE);"   % Non-blocking checkpoint
    ],
    
    %% Apply all optimizations
    lists:foreach(fun(Pragma) ->
        case esqlite3:exec(Db, Pragma) of
            ok -> ok;
            {error, Reason} ->
                logger:warning("Failed to execute pragma ~s: ~p", [Pragma, Reason])
        end
    end, OptimizationPragmas),
    
    %% Create table with optimized schema
    CreateTable = "CREATE TABLE IF NOT EXISTS Cookie ("
                  "Cookie   TEXT    NOT NULL AS (json_extract(Data, '$.cookie'))  STORED UNIQUE, "
                  "UserID   INTEGER NOT NULL AS (json_extract(Data, '$.user_id')) STORED, "
                  "Created  INTEGER NOT NULL AS (json_extract(Data, '$.created')) STORED, "
                  "LastUsed INTEGER AS (json_extract(Data, '$.last_used')) CHECK (LastUsed>0), "
                  "Data     TEXT    NOT NULL"
                  ");",
    
    case esqlite3:exec(Db, CreateTable) of
        ok -> 
            %% Create indexes for better query performance
            IndexQueries = [
                "CREATE INDEX IF NOT EXISTS idx_cookie_user_id ON Cookie(UserID);",
                "CREATE INDEX IF NOT EXISTS idx_cookie_created ON Cookie(Created DESC);",
                "CREATE INDEX IF NOT EXISTS idx_cookie_last_used ON Cookie(LastUsed DESC);"
            ],
            
            lists:foreach(fun(IndexQuery) ->
                case esqlite3:exec(Db, IndexQuery) of
                    ok -> ok;
                    {error, IndexReason} ->
                        logger:warning("Failed to create index: ~p", [IndexReason])
                end
            end, IndexQueries),
            
            ok = esqlite3:close(Db),
            logger:info("Database initialized successfully with optimizations");
        {error, Reason} ->
            ok = esqlite3:close(Db),
            logger:error("Failed to create table: ~p", [Reason]),
            error(database_init_failed)
    end.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    %% Initialize the connection pool
    Connections = lists:map(fun(_) ->
        {ok, Conn} = esqlite3:open(?DB_FILE),
        
        %% Apply per-connection optimizations
        ConnectionPragmas = [
            "PRAGMA journal_mode = WAL;",
            "PRAGMA synchronous = NORMAL;",
            "PRAGMA cache_size = -8000;",    % 8MB per connection
            "PRAGMA temp_store = MEMORY;",
            "PRAGMA busy_timeout = 5000;"
        ],
        
        lists:foreach(fun(Pragma) ->
            esqlite3:exec(Conn, Pragma)
        end, ConnectionPragmas),
        
        Conn
    end, lists:seq(1, ?POOL_SIZE)),
    
    logger:info("Database connection pool initialized with ~p connections", [?POOL_SIZE]),
    {ok, #state{available = Connections}}.

handle_call(get_connection, From, #state{available = [Conn|Rest]} = State) ->
    Monitor = erlang:monitor(process, element(1, From)),
    NewInUse = maps:put(Conn, Monitor, State#state.in_use),
    {reply, {ok, Conn}, State#state{available = Rest, in_use = NewInUse}};

handle_call(get_connection, From, #state{available = []} = State) ->
    Monitor = erlang:monitor(process, element(1, From)),
    NewWaiting = State#state.waiting ++ [{From, Monitor}],
    {noreply, State#state{waiting = NewWaiting}}.

handle_cast({return_connection, Conn}, State) ->
    case maps:find(Conn, State#state.in_use) of
        {ok, Monitor} ->
            erlang:demonitor(Monitor, [flush]),
            NewInUse = maps:remove(Conn, State#state.in_use),
            case State#state.waiting of
                [{From, WaitMonitor}|RestWaiting] ->
                    erlang:demonitor(WaitMonitor, [flush]),
                    NewMonitor = erlang:monitor(process, element(1, From)),
                    NewInUse2 = maps:put(Conn, NewMonitor, NewInUse),
                    gen_server:reply(From, {ok, Conn}),
                    {noreply, State#state{in_use = NewInUse2, waiting = RestWaiting}};
                [] ->
                    NewAvailable = [Conn|State#state.available],
                    {noreply, State#state{available = NewAvailable, in_use = NewInUse}}
            end;
        error ->
            logger:warning("Attempted to return unknown connection: ~p", [Conn]),
            {noreply, State}
    end.

handle_info({'DOWN', Monitor, process, _Pid, _Reason}, State) ->
    %% Handle process death - return connection to pool or remove from waiting
    case maps:fold(fun(Conn, Mon, Acc) ->
        case Mon =:= Monitor of
            true -> {found, Conn};
            false -> Acc
        end
    end, not_found, State#state.in_use) of
        {found, Conn} ->
            NewInUse = maps:remove(Conn, State#state.in_use),
            NewAvailable = [Conn|State#state.available],
            {noreply, State#state{available = NewAvailable, in_use = NewInUse}};
        not_found ->
            %% Check if it's a waiting process
            NewWaiting = lists:filter(fun({_From, Mon}) ->
                Mon =/= Monitor
            end, State#state.waiting),
            {noreply, State#state{waiting = NewWaiting}}
    end;

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    %% Close all connections
    AllConnections = State#state.available ++ maps:keys(State#state.in_use),
    lists:foreach(fun(Conn) ->
        esqlite3:close(Conn)
    end, AllConnections),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.