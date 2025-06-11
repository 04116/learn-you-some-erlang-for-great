# Erlang for Go Developers: The Complete Production Guide

Welcome, Go developer! This comprehensive guide will help you master Erlang by drawing parallels to Go concepts you already know. We'll use this high-performance Cookie CRUD API project as a practical example, covering everything from basic syntax to production deployment with monitoring and hot code swapping.

## Table of Contents
1. [Philosophy Comparison](#philosophy-comparison)
2. [Complete Syntax Guide](#complete-syntax-guide)
3. [Type System Deep Dive](#type-system-deep-dive)
4. [Function Programming Patterns](#function-programming-patterns)
5. [Concurrency and OTP](#concurrency-and-otp)
6. [Error Handling Strategies](#error-handling-strategies)
7. [Production Monitoring & Tracing](#production-monitoring--tracing)
8. [Hot Code Swapping & Deployment](#hot-code-swapping--deployment)
9. [Performance Optimization](#performance-optimization)
10. [Testing Strategies](#testing-strategies)
11. [Production Best Practices](#production-best-practices)
12. [Tooling and Development](#tooling-and-development)
13. [Migration Guide](#migration-guide)

## Philosophy Comparison

### Go's Philosophy
- "Do one thing well"
- Simple, readable code
- Built-in concurrency with goroutines
- Static typing with interfaces
- Compiled to native binaries

### Erlang's Philosophy
- "Let it crash" - fault tolerance through isolation
- Actor model concurrency
- Hot code swapping in production
- Functional programming with immutable data
- Designed for distributed, fault-tolerant systems

## Complete Syntax Guide

### Variables and Assignment - The Immutability Shift

**Go (Mutable Variables):**
```go
// Variables can be reassigned
var name string = "session123"
name = "updated_session"  // OK

userID := 1001
userID = 1002  // OK

// Constants
const MaxUsers = 100
```

**Erlang (Single Assignment - Immutable):**
```erlang
% Variables are IMMUTABLE - single assignment only!
Name = "session123",
% Name = "updated_session",  % This would CRASH with badmatch error!

% To "update", create new variables
UpdatedName = "updated_session",

% Pattern matching for assignment
{ok, UserID} = {ok, 1001},  % Extracts 1001 into UserID
[First, Second | Rest] = [1, 2, 3, 4, 5],  % First=1, Second=2, Rest=[3,4,5]

% Constants (macros)
-define(MAX_USERS, 100).
-define(DB_TIMEOUT, 5000).
-define(API_VERSION, <<"v1">>).
```

### Pattern Matching - Erlang's Superpower

**Go (Type Assertions and Switch):**
```go
// Type assertion
if val, ok := data["user_id"].(int); ok {
    processUserID(val)
}

// Switch statement
switch response.Status {
case 200:
    handleSuccess(response)
case 404:
    handleNotFound(response)
default:
    handleError(response)
}
```

**Erlang (Pattern Matching Everything):**
```erlang
% Pattern matching in function heads
process_response({ok, Data}) -> handle_success(Data);
process_response({error, not_found}) -> handle_not_found();
process_response({error, {database_error, Reason}}) -> handle_db_error(Reason);
process_response({error, Reason}) -> handle_generic_error(Reason).

% Pattern matching in case expressions
case maps:get(<<"user_id">>, Data, undefined) of
    undefined -> {error, missing_user_id};
    UserID when is_integer(UserID), UserID > 0 -> {ok, UserID};
    _Invalid -> {error, invalid_user_id}
end,

% Pattern matching lists
process_list([]) -> [];
process_list([Head | Tail]) -> [process_item(Head) | process_list(Tail)].

% Pattern matching maps
process_user(#{name := Name, age := Age}) when Age >= 18 ->
    {adult, Name};
process_user(#{name := Name, age := Age}) when Age < 18 ->
    {minor, Name};
process_user(_InvalidUser) ->
    {error, invalid_user_format}.
```

### Data Types - From Go to Erlang

**Go Data Types:**
```go
// Basic types
var (
    str    string = "hello"
    num    int = 42
    flag   bool = true
    slice  []string = []string{"a", "b"}
    m      map[string]int = map[string]int{"key": 1}
)

// Struct
type User struct {
    ID       int       `json:"id"`
    Name     string    `json:"name"`
    Created  time.Time `json:"created"`
    Active   bool      `json:"active"`
}

// Interface
type Handler interface {
    Handle(data interface{}) error
}
```

**Erlang Data Types:**
```erlang
% Basic types (atoms, numbers, binaries)
Atom = ok,                           % atom (immutable constant)
Integer = 42,                        % integer (arbitrary precision)
Float = 3.14159,                     % float
Binary = <<"hello">>,                % binary (bytes/string)
String = "hello",                    % string (list of integers) - avoid!
Boolean = true,                      % atom (true/false)

% Compound types
List = [1, 2, 3, 4],                % list (linked list)
Tuple = {ok, data, 123},             % tuple (fixed size)
Map = #{key => value, count => 42},  % map (like Go map)

% Records (like structs but compile-time)
-record(user, {
    id       :: integer(),
    name     :: binary(),
    created  :: integer(),    % Unix timestamp
    active   :: boolean()
}).

User = #user{
    id = 123,
    name = <<"John Doe">>,
    created = erlang:system_time(second),
    active = true
},

% Accessing record fields
UserName = User#user.name,           % Get field
UpdatedUser = User#user{active = false}, % Update field (creates new record)

% Process identifiers (unique to Erlang)
Pid = spawn(fun worker_process/0),   % process identifier
Ref = make_ref(),                    % unique reference

% Binaries and pattern matching
<<Version:8, Type:8, Length:16, Data:Length/binary>> = Packet,
```

### String Handling - Critical Differences

**Go Strings:**
```go
str := "Hello, World!"
greeting := fmt.Sprintf("Hello, %s!", name)
bytes := []byte(str)
if strings.Contains(str, "World") {
    // ...
}
```

**Erlang Strings (Use Binaries!):**
```erlang
% DON'T use strings (they're lists of integers!)
BadString = "Hello",  % This is [72, 101, 108, 108, 111] - inefficient!

% DO use binaries for text
GoodString = <<"Hello, World!">>,
Greeting = <<"Hello, ", Name/binary, "!">>,

% String operations
case binary:match(GoodString, <<"World">>) of
    nomatch -> not_found;
    {Pos, Len} -> {found, Pos, Len}
end,

% String interpolation (using io_lib or binary)
io_lib:format("User ~s has ~p cookies", [Name, Count]),
<<"User ", Name/binary, " has ", (integer_to_binary(Count))/binary, " cookies">>.
```

## Type System Deep Dive

### Static vs Dynamic with Gradual Typing

**Go's Static Type System:**
```go
// Everything must be declared with types
type CookieService struct {
    db     *sql.DB
    logger *log.Logger
}

func (s *CookieService) CreateCookie(data CreateCookieRequest) (*Cookie, error) {
    // Type safety enforced at compile time
    return &Cookie{ID: data.ID}, nil
}
```

**Erlang's Dynamic Types with Specs:**
```erlang
% Type specifications for static analysis (optional but recommended)
-type cookie_data() :: #{
    cookie := binary(),
    user_id := pos_integer(),
    created => pos_integer(),
    last_used => pos_integer()
}.

-type db_result(T) :: {ok, T} | {error, term()}.
-type http_method() :: get | post | put | delete.

% Function specifications
-spec create_cookie(cookie_data()) -> db_result(cookie_data()).
create_cookie(Data) when is_map(Data) ->
    % Runtime type checking with guards
    case maps:get(cookie, Data, undefined) of
        undefined -> {error, missing_cookie};
        Cookie when is_binary(Cookie), byte_size(Cookie) > 0 ->
            EnrichedData = Data#{
                created => erlang:system_time(second),
                last_used => erlang:system_time(second)
            },
            {ok, EnrichedData};
        _Invalid -> {error, invalid_cookie_format}
    end.

% Complex type specifications
-type connection_pool() :: #{available := [db_connection()],
                           in_use := #{db_connection() => reference()},
                           waiting := [from()]}.

-type server_state() :: #state{
    pool :: connection_pool(),
    config :: map(),
    metrics :: #{atom() => non_neg_integer()}
}.
```

### Guards - Runtime Type Safety

**Go Type Assertions:**
```go
if val, ok := data["count"].(int); ok && val > 0 {
    processCount(val)
} else {
    return errors.New("invalid count")
}
```

**Erlang Guards:**
```erlang
% Built-in guards
process_count(Count) when is_integer(Count), Count > 0 ->
    {ok, Count};
process_count(_Invalid) ->
    {error, invalid_count}.

% Multiple guards
validate_user(User) when is_map(User),
                         map_size(User) > 0 ->
    case {maps:get(name, User, undefined),
          maps:get(age, User, undefined)} of
        {Name, Age} when is_binary(Name), byte_size(Name) > 0,
                         is_integer(Age), Age >= 0, Age =< 150 ->
            {ok, User};
        _ ->
            {error, invalid_user_data}
    end;
validate_user(_) ->
    {error, not_a_map}.

% Custom guard functions (limited)
is_valid_email(Email) when is_binary(Email) ->
    byte_size(Email) > 3 andalso binary:match(Email, <<"@">>) =/= nomatch.
```

## Function Programming Patterns

### Higher-Order Functions - Functional Style

**Go with Function Types:**
```go
type Transformer func(int) int
type Predicate func(int) bool

func Map(slice []int, fn Transformer) []int {
    result := make([]int, len(slice))
    for i, v := range slice {
        result[i] = fn(v)
    }
    return result
}

func Filter(slice []int, pred Predicate) []int {
    var result []int
    for _, v := range slice {
        if pred(v) {
            result = append(result, v)
        }
    }
    return result
}
```

**Erlang's Built-in Functional Arsenal:**
```erlang
% Lists module - your functional programming toolkit
Numbers = [1, 2, 3, 4, 5],

% Map over lists
Squared = lists:map(fun(X) -> X * X end, Numbers),
% Result: [1, 4, 9, 16, 25]

% Filter lists
Evens = lists:filter(fun(X) -> X rem 2 =:= 0 end, Numbers),
% Result: [2, 4]

% Fold (reduce) lists
Sum = lists:foldl(fun(X, Acc) -> X + Acc end, 0, Numbers),
% Result: 15

% Complex data transformations
Users = [
    #{name => <<"Alice">>, age => 30, active => true},
    #{name => <<"Bob">>, age => 25, active => false},
    #{name => <<"Charlie">>, age => 35, active => true}
],

ActiveUserNames = lists:filtermap(
    fun(#{active := true, name := Name}) -> {true, Name};
       (_) -> false
    end,
    Users
),
% Result: [<<"Alice">>, <<"Charlie">>]

% Chaining operations
Result = Users
    |> lists:filter(fun(#{age := Age}) -> Age >= 30 end)
    |> lists:map(fun(#{name := Name}) -> Name end)
    |> lists:sort().
% Note: |> is not built-in, but can be defined as a macro
```

### Recursion Patterns - Erlang's Loops

**Go Iteration:**
```go
func ProcessItems(items []Item) []Result {
    results := make([]Result, 0, len(items))
    for _, item := range items {
        if result := processItem(item); result != nil {
            results = append(results, *result)
        }
    }
    return results
}
```

**Erlang Tail Recursion:**
```erlang
% Basic tail recursion
process_items(Items) ->
    process_items(Items, []).

process_items([], Acc) ->
    lists:reverse(Acc);  % Accumulator pattern
process_items([Item | Rest], Acc) ->
    case process_item(Item) of
        {ok, Result} ->
            process_items(Rest, [Result | Acc]);
        {error, _} ->
            process_items(Rest, Acc)
    end.

% Binary processing with recursion
parse_packet(<<Type:8, Length:16, Data:Length/binary, Rest/binary>>) ->
    Packet = #{type => Type, length => Length, data => Data},
    [Packet | parse_packet(Rest)];
parse_packet(<<>>) ->
    [];
parse_packet(_Incomplete) ->
    {error, incomplete_packet}.

% Tree traversal
traverse_tree(empty) -> [];
traverse_tree({node, Value, Left, Right}) ->
    traverse_tree(Left) ++ [Value] ++ traverse_tree(Right).
```

## Concurrency and OTP

### Concurrency Models - Fundamental Shift

**Go: Shared Memory with Goroutines**
```go
// Goroutines share memory, communicate via channels
type WorkerPool struct {
    jobs    chan Job
    results chan Result
    workers int
}

func (p *WorkerPool) Start() {
    for i := 0; i < p.workers; i++ {
        go func(id int) {
            for job := range p.jobs {
                result := processJob(job)
                p.results <- result
            }
        }(i)
    }
}

// Usage
pool := &WorkerPool{
    jobs:    make(chan Job, 100),
    results: make(chan Result, 100),
    workers: 10,
}
pool.Start()
```

**Erlang: Share-Nothing Actor Model**
```erlang
% Each process has isolated memory, communicate via message passing
-module(worker_pool).
-behaviour(gen_server).

% State is completely isolated per process
-record(state, {
    workers = [] :: [pid()],
    jobs_queue = [] :: [term()],
    worker_count = 0 :: non_neg_integer()
}).

% Start worker pool
start_link(WorkerCount) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [WorkerCount], []).

init([WorkerCount]) ->
    % Spawn isolated worker processes
    Workers = [spawn_link(fun() -> worker_loop() end) || _ <- lists:seq(1, WorkerCount)],
    {ok, #state{workers = Workers, worker_count = WorkerCount}}.

% Worker process loop (completely isolated)
worker_loop() ->
    receive
        {work, Job, From} ->
            Result = process_job(Job),
            From ! {result, Result},
            worker_loop();
        stop ->
            ok
    end.

% Distribute work to available workers
handle_call({submit_job, Job}, From, #state{workers = [Worker|Rest]} = State) ->
    Worker ! {work, Job, From},
    {noreply, State#state{workers = Rest ++ [Worker]}};
handle_call({submit_job, _Job}, _From, #state{workers = []} = State) ->
    {reply, {error, no_workers_available}, State}.
```

### OTP Behaviors - Erlang's Framework

**Gen_Server - Stateful Server Process**
```erlang
-module(cookie_server).
-behaviour(gen_server).

% API
-export([start_link/0, create_cookie/1, get_cookie/1, update_cookie/2]).

% Callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).

-record(state, {
    cookies = #{} :: #{binary() => map()},
    count = 0 :: non_neg_integer()
}).

%% API Functions
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

create_cookie(CookieData) ->
    gen_server:call(?MODULE, {create_cookie, CookieData}).

get_cookie(CookieID) ->
    gen_server:call(?MODULE, {get_cookie, CookieID}).

update_cookie(CookieID, UpdateData) ->
    gen_server:cast(?MODULE, {update_cookie, CookieID, UpdateData}).

%% Callbacks
init([]) ->
    % Initialize state
    {ok, #state{}}.

% Synchronous calls
handle_call({create_cookie, CookieData}, _From, State) ->
    CookieID = maps:get(<<"cookie">>, CookieData),
    case maps:is_key(CookieID, State#state.cookies) of
        true ->
            {reply, {error, cookie_exists}, State};
        false ->
            NewCookies = maps:put(CookieID, CookieData, State#state.cookies),
            NewState = State#state{
                cookies = NewCookies,
                count = State#state.count + 1
            },
            {reply, {ok, CookieData}, NewState}
    end;

handle_call({get_cookie, CookieID}, _From, State) ->
    case maps:find(CookieID, State#state.cookies) of
        {ok, CookieData} -> {reply, {ok, CookieData}, State};
        error -> {reply, {error, not_found}, State}
    end.

% Asynchronous casts
handle_cast({update_cookie, CookieID, UpdateData}, State) ->
    case maps:find(CookieID, State#state.cookies) of
        {ok, ExistingData} ->
            UpdatedData = maps:merge(ExistingData, UpdateData),
            NewCookies = maps:put(CookieID, UpdatedData, State#state.cookies),
            {noreply, State#state{cookies = NewCookies}};
        error ->
            {noreply, State}
    end.

% Handle other messages
handle_info(Info, State) ->
    logger:warning("Unexpected message: ~p", [Info]),
    {noreply, State}.
```

**Supervisor - Fault Tolerance**
```erlang
-module(cookie_sup).
-behaviour(supervisor).

-export([start_link/0, init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    % Supervisor flags
    SupFlags = #{
        strategy => one_for_all,    % Restart strategy
        intensity => 5,             % Max 5 restarts
        period => 10               % In 10 seconds
    },

    % Child specifications
    Children = [
        % Database connection pool
        #{
            id => cookie_db_pool,
            start => {cookie_db_pool, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [cookie_db_pool]
        },
        
        % HTTP server
        #{
            id => cowboy_listener,
            start => {cowboy, start_clear, [
                http_listener,
                [{port, 8080}],
                #{env => #{dispatch => get_dispatch()}}
            ]},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [cowboy]
        }
    ],

    {ok, {SupFlags, Children}}.

get_dispatch() ->
    cowboy_router:compile([
        {'_', [
            {"/cookies", cookie_crud, []},
            {"/cookies/:cookie", cookie_crud, []}
        ]}
    ]).
```

### Process Communication Patterns

**Request-Response Pattern:**
```erlang
% Synchronous request-response
request_response(ServerPid, Request) ->
    Ref = make_ref(),
    ServerPid ! {request, Request, {self(), Ref}},
    receive
        {response, Ref, Response} -> Response
    after 5000 ->
        {error, timeout}
    end.

% Server side
server_loop(State) ->
    receive
        {request, Request, {From, Ref}} ->
            Response = handle_request(Request, State),
            From ! {response, Ref, Response},
            server_loop(State);
        stop ->
            ok
    end.
```

**Publish-Subscribe Pattern:**
```erlang
% Event manager for pub-sub
-module(event_manager).
-behaviour(gen_event).

% Subscribe to events
subscribe(EventType) ->
    gen_event:add_handler(event_manager, {?MODULE, self()}, [EventType]).

% Publish events
publish(Event) ->
    gen_event:notify(event_manager, Event).

% Handle events
handle_event(Event, [EventType] = State) ->
    case Event of
        {EventType, Data} ->
            % Process event
            process_event(Data),
            {ok, State};
        _ ->
            {ok, State}
    end.
```

## Error Handling Strategies

### From Go's Explicit Errors to Erlang's "Let It Crash"

**Go: Defensive Programming**
```go
func ProcessCookies(cookies []Cookie) error {
    if len(cookies) == 0 {
        return errors.New("no cookies to process")
    }

    for _, cookie := range cookies {
        if err := validateCookie(cookie); err != nil {
            return fmt.Errorf("invalid cookie %s: %w", cookie.ID, err)
        }
        
        if err := saveCookie(cookie); err != nil {
            return fmt.Errorf("failed to save cookie %s: %w", cookie.ID, err)
        }
    }
    
    return nil
}
```

**Erlang: "Let It Crash" with Supervision**
```erlang
% Worker process - let it crash on errors
process_cookies([]) ->
    error(no_cookies_to_process);  % Intentional crash
process_cookies(Cookies) ->
    lists:foreach(fun process_single_cookie/1, Cookies).

process_single_cookie(Cookie) ->
    % Don't handle every possible error - let supervisor restart us
    ok = validate_cookie(Cookie),  % Will crash if validation fails
    ok = save_cookie(Cookie).     % Will crash if save fails

% Supervisor handles the crashes
-module(cookie_processor_sup).
-behaviour(supervisor).

init([]) ->
    SupFlags = #{
        strategy => simple_one_for_one,  % Dynamic children
        intensity => 10,                 % Allow 10 crashes
        period => 60                     % Per minute
    },
    
    ChildSpec = #{
        id => cookie_worker,
        start => {cookie_worker, start_link, []},
        restart => temporary,  % Don't restart - just clean up
        shutdown => 5000,
        type => worker
    },
    
    {ok, {SupFlags, [ChildSpec]}}.

% Start a worker for each batch
process_cookie_batch(Cookies) ->
    {ok, WorkerPid} = supervisor:start_child(cookie_processor_sup, []),
    cookie_worker:process_cookies(WorkerPid, Cookies).
```

### Error Handling Patterns

**Tagged Tuples - Erlang's Result Type:**
```erlang
% Always return {ok, Result} or {error, Reason}
parse_json(Data) ->
    try
        Decoded = jsx:decode(Data, [return_maps]),
        {ok, Decoded}
    catch
        error:badarg -> {error, invalid_json};
        error:{badmatch, _} -> {error, malformed_json}
    end.

% Chain operations with case expressions
process_request(JsonData) ->
    case parse_json(JsonData) of
        {ok, Parsed} ->
            case validate_data(Parsed) of
                {ok, Valid} ->
                    case save_data(Valid) of
                        {ok, Saved} -> {ok, Saved};
                        {error, Reason} -> {error, {save_failed, Reason}}
                    end;
                {error, Reason} -> {error, {validation_failed, Reason}}
            end;
        {error, Reason} -> {error, {parse_failed, Reason}}
    end.

% Or use a more functional approach
process_request_functional(JsonData) ->
    Result = parse_json(JsonData)
        |> bind(fun validate_data/1)
        |> bind(fun save_data/1),
    Result.

% Helper function for monadic composition
bind({ok, Value}, Fun) -> Fun(Value);
bind({error, Reason}, _Fun) -> {error, Reason}.
```

## Production Monitoring & Tracing

### Built-in Observability Tools

**Go Monitoring (External Tools Required):**
```go
// Typically use Prometheus, Jaeger, etc.
import (
    "github.com/prometheus/client_golang/prometheus"
    "go.opentelemetry.io/otel/trace"
)

var (
    requestCounter = prometheus.NewCounterVec(
        prometheus.CounterOpts{
            Name: "http_requests_total",
            Help: "Total HTTP requests",
        },
        []string{"method", "status"},
    )
)

func handleRequest(w http.ResponseWriter, r *http.Request) {
    requestCounter.WithLabelValues(r.Method, "200").Inc()
    // ...
}
```

**Erlang Built-in Monitoring:**
```erlang
% BUILT-IN: Observer - Real-time system monitoring
% Start with: observer:start().

% Built-in tracing
-module(cookie_tracer).
-export([start_tracing/0, stop_tracing/0]).

start_tracing() ->
    % Trace all calls to cookie_crud module
    dbg:tracer(),
    dbg:p(all, c),  % Trace all processes for calls
    dbg:tpl(cookie_crud, []),  % Trace all functions in cookie_crud
    ok.

stop_tracing() ->
    dbg:stop_clear().

% Custom metrics collection
-module(metrics_collector).
-behaviour(gen_server).

-record(state, {
    request_count = 0 :: non_neg_integer(),
    error_count = 0 :: non_neg_integer(),
    response_times = [] :: [non_neg_integer()],
    start_time :: integer()
}).

% Record metrics
record_request(ResponseTime, Success) ->
    gen_server:cast(?MODULE, {record_request, ResponseTime, Success}).

handle_cast({record_request, ResponseTime, true}, State) ->
    NewState = State#state{
        request_count = State#state.request_count + 1,
        response_times = [ResponseTime | State#state.response_times]
    },
    {noreply, NewState};
handle_cast({record_request, ResponseTime, false}, State) ->
    NewState = State#state{
        request_count = State#state.request_count + 1,
        error_count = State#state.error_count + 1,
        response_times = [ResponseTime | State#state.response_times]
    },
    {noreply, NewState}.

% Get current metrics
get_metrics() ->
    gen_server:call(?MODULE, get_metrics).

handle_call(get_metrics, _From, State) ->
    Uptime = erlang:system_time(second) - State#state.start_time,
    AvgResponseTime = case State#state.response_times of
        [] -> 0;
        Times -> lists:sum(Times) div length(Times)
    end,
    
    Metrics = #{
        uptime => Uptime,
        total_requests => State#state.request_count,
        error_count => State#state.error_count,
        success_rate => case State#state.request_count of
            0 -> 0.0;
            Total -> (Total - State#state.error_count) / Total * 100
        end,
        avg_response_time => AvgResponseTime,
        requests_per_second => State#state.request_count / max(1, Uptime)
    },
    
    {reply, Metrics, State}.
```

### Advanced Tracing and Debugging

**Process Inspection:**
```erlang
% Live system inspection (no restart required!)

% 1. Find processes
DbPoolPid = whereis(cookie_db_pool),
info(DbPoolPid).  % Get process info

% 2. Inspect process state
sys:get_state(DbPoolPid).  % Get current state

% 3. Trace specific process
dbg:tracer(),
dbg:p(DbPoolPid, [c, m]),  % Trace calls and messages
dbg:tpl(cookie_db_pool, handle_call, []).  % Trace specific function

% 4. Memory inspection
erlang:process_info(DbPoolPid, memory).  % Process memory usage
erlang:memory().  % System memory usage

% 5. Message queue inspection
erlang:process_info(DbPoolPid, message_queue_len).
erlang:process_info(DbPoolPid, messages).

% 6. Live code tracing
-module(live_tracer).
-export([trace_slow_requests/0]).

trace_slow_requests() ->
    % Trace requests that take longer than 100ms
    dbg:tracer(),
    dbg:p(all, c),
    dbg:tpl(cookie_crud, handle_get, 
        [{[], [], [{message, {"Slow request detected: ", {process_dump}}}]}]).
```

### Performance Monitoring

**Real-time Performance Dashboard:**
```erlang
-module(perf_monitor).
-behaviour(gen_server).

% Start monitoring
start_monitor() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    % Schedule regular performance checks
    timer:send_interval(5000, self(), collect_metrics),
    {ok, #{}}.

handle_info(collect_metrics, State) ->
    Metrics = collect_system_metrics(),
    report_metrics(Metrics),
    check_alerts(Metrics),
    {noreply, State}.

collect_system_metrics() ->
    #{
        % System metrics
        total_memory => erlang:memory(total),
        process_count => erlang:system_info(process_count),
        run_queue => erlang:statistics(run_queue),
        io_input => element(2, erlang:statistics(io)),
        io_output => element(1, erlang:statistics(io)),
        
        % Application metrics
        db_pool_size => get_pool_size(),
        active_connections => get_active_connections(),
        request_rate => get_request_rate(),
        
        % Response time percentiles
        response_times => get_response_time_percentiles()
    }.

check_alerts(Metrics) ->
    MaxMemory = 1024 * 1024 * 1024,  % 1GB
    case maps:get(total_memory, Metrics) of
        Memory when Memory > MaxMemory ->
            logger:alert("High memory usage: ~p MB", [Memory div (1024*1024)]);
        _ -> ok
    end,
    
    case maps:get(process_count, Metrics) of
        Processes when Processes > 10000 ->
            logger:alert("High process count: ~p", [Processes]);
        _ -> ok
    end.
```

## Hot Code Swapping & Deployment

### Zero-Downtime Deployment

**Go Deployment (Restart Required):**
```bash
# Traditional approach - service interruption
sudo systemctl stop myapp
sudo cp new_binary /usr/local/bin/myapp
sudo systemctl start myapp
```

**Erlang Hot Code Swapping (Zero Downtime):**
```erlang
% 1. Code replacement without stopping the system
% Module versioning system built-in

% Old version running
-module(cookie_crud).
-export([handle_request/1]).

handle_request(Req) ->
    % Old implementation
    {ok, process_old_way(Req)}.

% Deploy new version (both versions co-exist temporarily)
-module(cookie_crud).
-export([handle_request/1, handle_request_v2/1]).

handle_request(Req) ->
    % New implementation
    {ok, process_new_way(Req)}.

handle_request_v2(Req) ->
    % Even newer implementation
    {ok, process_newer_way(Req)}.

% Hot deployment script
-module(deployment).
-export([deploy_new_version/1]).

deploy_new_version(ModuleName) ->
    % 1. Compile new code
    {ok, ModuleName} = compile:file(ModuleName),
    
    % 2. Load new code (soft upgrade)
    code:soft_purge(ModuleName),
    code:load_file(ModuleName),
    
    % 3. Optionally migrate state
    migrate_running_processes(ModuleName),
    
    % 4. Remove old code when safe
    timer:apply_after(60000, code, purge, [ModuleName]).

migrate_running_processes(ModuleName) ->
    % Find all processes running old code
    Processes = processes(),
    lists:foreach(fun(Pid) ->
        case erlang:process_info(Pid, current_function) of
            {current_function, {ModuleName, _, _}} ->
                % Send upgrade message to process
                Pid ! {upgrade_code, ModuleName};
            _ -> ok
        end
    end, Processes).
```

### Release Management

**Production Release Strategy:**
```erlang
% rebar.config for releases
{relx, [
    {release, {cookie_crud, "1.0.0"}, [
        cookie_crud,
        sasl,     % System Architecture Support Libraries
        runtime_tools  % For hot upgrades
    ]},
    
    {dev_mode, false},
    {include_erts, true},  % Include Erlang runtime
    
    % Hot upgrade configuration
    {extended_start_script, true},
    {sys_config, "config/sys.config"},
    {vm_args, "config/vm.args"}
]}.

% Deployment commands
% rebar3 as prod release      # Build release
% rebar3 as prod tar          # Create deployment package

% Production deployment
% 1. Upload new release
% 2. Install: ./bin/cookie_crud install
% 3. Upgrade: ./bin/cookie_crud upgrade
% 4. No service interruption!
```

### State Migration During Upgrades

**Gen_Server State Migration:**
```erlang
-module(cookie_server).
-behaviour(gen_server).

% Version 1 state
-record(state_v1, {
    cookies = #{} :: map(),
    count = 0 :: integer()
}).

% Version 2 state (added metrics)
-record(state_v2, {
    cookies = #{} :: map(),
    count = 0 :: integer(),
    metrics = #{} :: map(),
    start_time :: integer()
}).

% Handle code change during hot upgrade
code_change("1.0.0", StateV1, _Extra) when is_record(StateV1, state_v1) ->
    % Migrate from v1 to v2
    StateV2 = #state_v2{
        cookies = StateV1#state_v1.cookies,
        count = StateV1#state_v1.count,
        metrics = #{},
        start_time = erlang:system_time(second)
    },
    {ok, StateV2};
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

% Handle upgrade message
handle_info({upgrade_code, _Module}, State) ->
    % Perform any necessary state cleanup or migration
    logger:info("Code upgraded successfully"),
    {noreply, State}.
```

## Performance Optimization

### Memory Management Strategies

**Go Memory Management:**
```go
// Manual memory optimization
func ProcessLargeDataset(data []LargeStruct) {
    // Pool objects to reduce GC pressure
    pool := sync.Pool{
        New: func() interface{} {
            return make([]ProcessedData, 0, 1000)
        },
    }
    
    buffer := pool.Get().([]ProcessedData)
    defer pool.Put(buffer[:0])
    
    // Process in chunks to control memory
    for i := 0; i < len(data); i += 1000 {
        end := min(i+1000, len(data))
        processChunk(data[i:end], buffer)
    }
}
```

**Erlang Memory Optimization:**
```erlang
% Per-process garbage collection - no global GC pauses!
% Memory optimization strategies:

% 1. Process hibernation for inactive processes
idle_worker() ->
    receive
        {work, Data} ->
            Result = process_data(Data),
            send_result(Result),
            idle_worker();
        hibernate ->
            erlang:hibernate(?MODULE, idle_worker, [])  % Minimize memory
    after 30000 ->  % Hibernate after 30 seconds of inactivity
        erlang:hibernate(?MODULE, idle_worker, [])
    end.

% 2. Binary data handling (shared between processes)
handle_large_binary(LargeBinary) ->
    % Split work across processes, binary is shared (not copied)
    ChunkSize = byte_size(LargeBinary) div 4,
    
    Workers = [
        spawn(fun() -> process_chunk(binary:part(LargeBinary, I*ChunkSize, ChunkSize)) end)
        || I <- lists:seq(0, 3)
    ],
    
    collect_results(Workers, []).

% 3. ETS tables for large datasets
-module(cache_manager).
-export([init_cache/0, store_data/2, get_data/1]).

init_cache() ->
    % ETS table shared between processes, no copying
    ets:new(cache, [named_table, public, {write_concurrency, true}, {read_concurrency, true}]).

store_data(Key, Data) ->
    ets:insert(cache, {Key, Data}).

get_data(Key) ->
    case ets:lookup(cache, Key) of
        [{Key, Data}] -> {ok, Data};
        [] -> {error, not_found}
    end.

% 4. Process pool for heavy computations
-module(compute_pool).
-behaviour(gen_server).

-record(state, {
    workers = [] :: [pid()],
    tasks = queue:new() :: queue:queue()
}).

handle_call({compute, Task}, From, State) ->
    case State#state.workers of
        [Worker | RestWorkers] ->
            Worker ! {task, Task, From},
            {noreply, State#state{workers = RestWorkers}};
        [] ->
            NewTasks = queue:in({Task, From}, State#state.tasks),
            {noreply, State#state{tasks = NewTasks}}
    end.

% Worker returns to pool after task
worker_done(WorkerPid, #state{tasks = Tasks} = State) ->
    case queue:out(Tasks) of
        {{value, {Task, From}}, RestTasks} ->
            WorkerPid ! {task, Task, From},
            State#state{tasks = RestTasks};
        {empty, _} ->
            State#state{workers = [WorkerPid | State#state.workers]}
    end.
```

### Profiling and Performance Analysis

**Built-in Profiling Tools:**
```erlang
% 1. fprof - Function profiling
start_profiling() ->
    fprof:start(),
    fprof:trace(start, "/tmp/cookie_crud_profile.trace"),
    % Run your code here
    run_performance_test(),
    fprof:trace(stop),
    fprof:profile("/tmp/cookie_crud_profile.trace"),
    fprof:analyse([{dest, "/tmp/cookie_crud_analysis.txt"}]).

% 2. eprof - Time profiling
profile_function() ->
    eprof:start(),
    eprof:start_profiling([self()]),
    % Run code to profile
    Results = cookie_crud:get_all_cookies(),
    eprof:stop_profiling(),
    eprof:analyze(total).

% 3. percept - Concurrency profiling
profile_concurrency() ->
    percept:start_webserver(),  % Start web interface
    percept:profile("/tmp/percept.dat", [procs]),
    % Run concurrent code
    run_concurrent_test(),
    percept:stop_profile(),
    % View results at http://localhost:8888
    ok.

% 4. Custom performance monitoring
-module(perf_counter).
-export([time_function/1, benchmark/3]).

time_function(Fun) ->
    Start = erlang:monotonic_time(microsecond),
    Result = Fun(),
    End = erlang:monotonic_time(microsecond),
    Duration = End - Start,
    {Result, Duration}.

benchmark(Fun, Iterations, Concurrency) ->
    % Run benchmark with multiple processes
    Parent = self(),
    
    Workers = [
        spawn(fun() ->
            Times = [element(2, time_function(Fun)) || _ <- lists:seq(1, Iterations div Concurrency)],
            Parent ! {results, Times}
        end)
        || _ <- lists:seq(1, Concurrency)
    ],
    
    AllTimes = collect_benchmark_results(Workers, []),
    
    #{
        total_ops => length(AllTimes),
        avg_time => lists:sum(AllTimes) / length(AllTimes),
        min_time => lists:min(AllTimes),
        max_time => lists:max(AllTimes),
        ops_per_second => 1000000 / (lists:sum(AllTimes) / length(AllTimes))
    }.
```

### Database Performance Optimization

**Connection Pooling with Backpressure:**
```erlang
% Enhanced connection pool with performance monitoring
-module(enhanced_db_pool).
-behaviour(gen_server).

-record(state, {
    available = [] :: [db_connection()],
    in_use = #{} :: #{db_connection() => {pid(), integer()}},  % {Pid, StartTime}
    waiting = queue:new() :: queue:queue(),
    stats = #{} :: #{
        total_requests => non_neg_integer(),
        active_connections => non_neg_integer(),
        avg_wait_time => float(),
        slow_queries => non_neg_integer()
    }
}).

% Enhanced connection acquisition with metrics
handle_call(get_connection, From, State) ->
    StartTime = erlang:monotonic_time(microsecond),
    
    case State#state.available of
        [Conn | Rest] ->
            Monitor = erlang:monitor(process, element(1, From)),
            NewInUse = maps:put(Conn, {element(1, From), StartTime}, State#state.in_use),
            
            % Update stats
            NewStats = maps:update_with(total_requests, fun(X) -> X + 1 end, 1, State#state.stats),
            
            {reply, {ok, Conn}, State#state{
                available = Rest,
                in_use = NewInUse,
                stats = NewStats
            }};
        [] ->
            % Track waiting time
            NewWaiting = queue:in({From, StartTime}, State#state.waiting),
            {noreply, State#state{waiting = NewWaiting}}
    end.

% Connection return with performance tracking
handle_cast({return_connection, Conn}, State) ->
    case maps:find(Conn, State#state.in_use) of
        {ok, {_Pid, StartTime}} ->
            Duration = erlang:monotonic_time(microsecond) - StartTime,
            
            % Track slow queries
            NewStats = case Duration > 100000 of  % 100ms threshold
                true -> maps:update_with(slow_queries, fun(X) -> X + 1 end, 1, State#state.stats);
                false -> State#state.stats
            end,
            
            NewInUse = maps:remove(Conn, State#state.in_use),
            
            case queue:out(State#state.waiting) of
                {{value, {From, WaitStart}}, RestWaiting} ->
                    % Calculate wait time
                    WaitTime = erlang:monotonic_time(microsecond) - WaitStart,
                    UpdatedStats = update_avg_wait_time(NewStats, WaitTime),
                    
                    Monitor = erlang:monitor(process, element(1, From)),
                    NewInUse2 = maps:put(Conn, {element(1, From), erlang:monotonic_time(microsecond)}, NewInUse),
                    gen_server:reply(From, {ok, Conn}),
                    
                    {noreply, State#state{
                        in_use = NewInUse2,
                        waiting = RestWaiting,
                        stats = UpdatedStats
                    }};
                {empty, _} ->
                    {noreply, State#state{
                        available = [Conn | State#state.available],
                        in_use = NewInUse,
                        stats = NewStats
                    }}
            end;
        error ->
            {noreply, State}
    end.

get_performance_stats() ->
    gen_server:call(?MODULE, get_stats).

handle_call(get_stats, _From, State) ->
    Stats = State#state.stats#{
        active_connections => maps:size(State#state.in_use),
        waiting_requests => queue:len(State#state.waiting),
        available_connections => length(State#state.available)
    },
    {reply, Stats, State}.
```

## Testing Strategies

### Comprehensive Testing Approaches

**Go Testing (Table-Driven Tests):**
```go
func TestCreateCookie(t *testing.T) {
    tests := []struct {
        name    string
        input   map[string]interface{}
        want    *Cookie
        wantErr bool
    }{
        {
            name: "valid cookie",
            input: map[string]interface{}{
                "cookie":  "session123",
                "user_id": 1001,
            },
            want: &Cookie{ID: "session123", UserID: 1001},
            wantErr: false,
        },
        {
            name: "missing cookie",
            input: map[string]interface{}{
                "user_id": 1001,
            },
            want: nil,
            wantErr: true,
        },
    }

    for _, tt := range tests {
        t.Run(tt.name, func(t *testing.T) {
            got, err := createCookie(tt.input)
            if (err != nil) != tt.wantErr {
                t.Errorf("createCookie() error = %v, wantErr %v", err, tt.wantErr)
                return
            }
            if !reflect.DeepEqual(got, tt.want) {
                t.Errorf("createCookie() = %v, want %v", got, tt.want)
            }
        })
    }
}
```

**Erlang Testing (EUnit + Property-Based Testing):**
```erlang
% EUnit tests
-module(cookie_crud_tests).
-include_lib("eunit/include/eunit.hrl").

% Test generators
create_cookie_test_() ->
    [
        {"Valid cookie creation", fun test_valid_cookie_creation/0},
        {"Missing cookie field", fun test_missing_cookie_field/0},
        {"Invalid user ID", fun test_invalid_user_id/0}
    ].

test_valid_cookie_creation() ->
    Data = #{<<"cookie">> => <<"session123">>, <<"user_id">> => 1001},
    {ok, Result} = cookie_crud:create_cookie(Data),
    ?assertEqual(<<"session123">>, maps:get(<<"cookie">>, Result)),
    ?assertEqual(1001, maps:get(<<"user_id">>, Result)),
    ?assert(maps:is_key(<<"created">>, Result)).

test_missing_cookie_field() ->
    Data = #{<<"user_id">> => 1001},
    ?assertEqual({error, cookie_required}, cookie_crud:create_cookie(Data)).

% Property-based testing with PropEr
-ifdef(PROPER).
-include_lib("proper/include/proper.hrl").

% Generate valid cookie data
cookie_data() ->
    ?LET({Cookie, UserID}, {binary(), pos_integer()},
         #{<<"cookie">> => Cookie, <<"user_id">> => UserID}).

% Property: creating and then retrieving a cookie should return the same data
prop_cookie_roundtrip() ->
    ?FORALL(CookieData, cookie_data(),
        begin
            CookieID = maps:get(<<"cookie">>, CookieData),
            {ok, Created} = cookie_crud:create_cookie(CookieData),
            {ok, Retrieved} = cookie_crud:get_cookie(CookieID),
            maps:get(<<"cookie">>, Created) =:= maps:get(<<"cookie">>, Retrieved)
        end).

% Property: concurrent operations should be safe
prop_concurrent_safety() ->
    ?FORALL(Operations, list(cookie_operation()),
        begin
            % Run operations concurrently
            Parent = self(),
            Pids = [spawn(fun() -> 
                Result = execute_operation(Op),
                Parent ! {result, self(), Result}
            end) || Op <- Operations],
            
            % Collect results
            Results = [receive {result, Pid, Result} -> Result end || Pid <- Pids],
            
            % Check that all operations completed successfully or failed gracefully
            lists:all(fun(Result) -> 
                case Result of
                    {ok, _} -> true;
                    {error, _} -> true;
                    _ -> false
                end
            end, Results)
        end).

cookie_operation() ->
    oneof([
        {create, cookie_data()},
        {get, binary()},
        {update, binary(), #{binary() => term()}},
        {delete, binary()}
    ]).

-endif.

% Integration tests
integration_test_() ->
    {setup,
     fun setup_test_env/0,
     fun cleanup_test_env/1,
     fun(SetupData) ->
         [
             {"Full CRUD workflow", fun() -> test_full_crud_workflow(SetupData) end},
             {"Concurrent access", fun() -> test_concurrent_access(SetupData) end},
             {"Database failure recovery", fun() -> test_db_failure_recovery(SetupData) end}
         ]
     end}.

setup_test_env() ->
    % Start test database
    TestDbFile = "/tmp/test_cookies.db",
    file:delete(TestDbFile),
    application:set_env(cookie_crud, db_file, TestDbFile),
    {ok, _} = application:ensure_all_started(cookie_crud),
    TestDbFile.

cleanup_test_env(TestDbFile) ->
    application:stop(cookie_crud),
    file:delete(TestDbFile).

test_full_crud_workflow(_SetupData) ->
    % Create
    Data = #{<<"cookie">> => <<"test123">>, <<"user_id">> => 1001},
    {ok, Created} = cookie_crud:create_cookie(Data),
    
    % Read
    {ok, Retrieved} = cookie_crud:get_cookie(<<"test123">>),
    ?assertEqual(maps:get(<<"user_id">>, Created),
                 maps:get(<<"user_id">>, Retrieved)),
    
    % Update
    UpdateData = #{<<"user_id">> => 1002},
    {ok, Updated} = cookie_crud:update_cookie(<<"test123">>, UpdateData),
    ?assertEqual(1002, maps:get(<<"user_id">>, Updated)),
    
    % Delete
    ok = cookie_crud:delete_cookie(<<"test123">>),
    ?assertEqual({error, not_found}, cookie_crud:get_cookie(<<"test123">>)).

test_concurrent_access(_SetupData) ->
    % Test that concurrent operations don't cause race conditions
    NumProcesses = 100,
    Parent = self(),
    
    Pids = [spawn(fun() ->
        CookieID = list_to_binary("cookie_" ++ integer_to_list(N)),
        Data = #{<<"cookie">> => CookieID, <<"user_id">> => N},
        Result = cookie_crud:create_cookie(Data),
        Parent ! {done, self(), Result}
    end) || N <- lists:seq(1, NumProcesses)],
    
    % Wait for all processes to complete
    Results = [receive {done, Pid, Result} -> Result end || Pid <- Pids],
    
    % All should succeed (no race conditions)
    SuccessCount = length([R || {ok, _} <- Results]),
    ?assertEqual(NumProcesses, SuccessCount).
```

### Load Testing and Benchmarking

**Erlang Load Testing Framework:**
```erlang
-module(load_tester).
-export([run_load_test/3, benchmark_function/3]).

% Run load test with specified number of processes and requests
run_load_test(NumProcesses, RequestsPerProcess, TargetFunction) ->
    StartTime = erlang:monotonic_time(millisecond),
    Parent = self(),
    
    % Spawn load testing processes
    Pids = [spawn(fun() ->
        Results = run_requests(RequestsPerProcess, TargetFunction, []),
        Parent ! {results, self(), Results}
    end) || _ <- lists:seq(1, NumProcesses)],
    
    % Collect all results
    AllResults = collect_results(Pids, []),
    EndTime = erlang:monotonic_time(millisecond),
    
    % Calculate statistics
    TotalRequests = NumProcesses * RequestsPerProcess,
    TotalTime = EndTime - StartTime,
    RequestsPerSecond = TotalRequests / (TotalTime / 1000),
    
    {SuccessCount, FailureCount, ResponseTimes} = analyze_results(AllResults),
    
    #{
        total_requests => TotalRequests,
        success_count => SuccessCount,
        failure_count => FailureCount,
        success_rate => SuccessCount / TotalRequests * 100,
        requests_per_second => RequestsPerSecond,
        avg_response_time => lists:sum(ResponseTimes) / length(ResponseTimes),
        min_response_time => lists:min(ResponseTimes),
        max_response_time => lists:max(ResponseTimes),
        percentile_95 => percentile(ResponseTimes, 0.95),
        percentile_99 => percentile(ResponseTimes, 0.99)
    }.

run_requests(0, _Function, Acc) -> Acc;
run_requests(N, Function, Acc) ->
    StartTime = erlang:monotonic_time(microsecond),
    Result = Function(),
    EndTime = erlang:monotonic_time(microsecond),
    ResponseTime = EndTime - StartTime,
    
    run_requests(N - 1, Function, [{Result, ResponseTime} | Acc]).

% Benchmark specific cookie operations
benchmark_cookie_operations() ->
    % Benchmark different operations
    CreateBenchmark = run_load_test(10, 1000, fun() ->
        Data = #{<<"cookie">> => generate_cookie_id(), <<"user_id">> => rand:uniform(10000)},
        cookie_crud:create_cookie(Data)
    end),
    
    GetBenchmark = run_load_test(20, 2000, fun() ->
        cookie_crud:get_all_cookies()
    end),
    
    #{
        create_performance => CreateBenchmark,
        get_all_performance => GetBenchmark
    }.

generate_cookie_id() ->
    list_to_binary("cookie_" ++ integer_to_list(rand:uniform(1000000))).
```

## Production Best Practices

### Configuration Management

**Go Configuration (Environment Variables):**
```go
type Config struct {
    Port     int    `env:"PORT" envDefault:"8080"`
    DBFile   string `env:"DB_FILE" envDefault:"cookies.db"`
    LogLevel string `env:"LOG_LEVEL" envDefault:"info"`
}

func LoadConfig() (*Config, error) {
    cfg := &Config{}
    if err := env.Parse(cfg); err != nil {
        return nil, err
    }
    return cfg, nil
}
```

**Erlang Configuration (sys.config + vm.args):**
```erlang
% config/sys.config - Application configuration
[
    {cookie_crud, [
        {port, 8080},
        {db_file, "cookies.db"},
        {db_pool_size, 10},
        {request_timeout, 30000},
        {max_connections, 1000}
    ]},
    
    {lager, [
        {handlers, [
            {lager_console_backend, [{level, info}]},
            {lager_file_backend, [
                {file, "log/error.log"}, {level, error}
            ]},
            {lager_file_backend, [
                {file, "log/info.log"}, {level, info}
            ]}
        ]}
    ]},
    
    {sasl, [
        {sasl_error_logger, {file, "log/sasl-error.log"}},
        {errlog_type, error},
        {error_logger_mf_dir, "log/sasl"},
        {error_logger_mf_maxbytes, 10485760},
        {error_logger_mf_maxfiles, 5}
    ]}
].

% config/vm.args - VM configuration
-name cookie_crud@127.0.0.1
-setcookie cookie_crud_secret_cookie
-heart                              % Automatic restart on VM crash
+K true                            % Enable kernel poll
+A30                              % 30 async threads
+P 1048576                        % Max processes
+Q 1048576                        % Max ports
+zdbbl 32768                      % Distribution buffer size
+sbwt very_long                   % Scheduler busy wait
+swt very_low                     % Scheduler wakeup threshold
+MMmcs 30                         % Multiblock carrier size

% Environment-specific configuration
-module(config).
-export([get/1, get/2, get_env/0]).

get(Key) ->
    get(Key, undefined).

get(Key, Default) ->
    application:get_env(cookie_crud, Key, Default).

get_env() ->
    case os:getenv("COOKIE_CRUD_ENV") of
        "production" -> production;
        "staging" -> staging;
        "development" -> development;
        _ -> development
    end.

% Dynamic configuration reloading
-module(config_reloader).
-behaviour(gen_server).

init([]) ->
    % Watch config file for changes
    {ok, Watcher} = file_watcher:watch("config/sys.config"),
    {ok, #{watcher => Watcher}}.

handle_info({file_changed, "config/sys.config"}, State) ->
    logger:info("Configuration file changed, reloading..."),
    reload_config(),
    {noreply, State}.

reload_config() ->
    % Hot reload configuration without restart
    {ok, NewConfig} = file:consult("config/sys.config"),
    lists:foreach(fun({App, Config}) ->
        lists:foreach(fun({Key, Value}) ->
            application:set_env(App, Key, Value)
        end, Config)
    end, NewConfig).
```

### Logging and Observability

**Structured Logging:**
```erlang
% Enhanced logging with structured data
-module(structured_logger).
-export([log/3, log/4]).

log(Level, Message, Metadata) ->
    log(Level, Message, Metadata, #{}).

log(Level, Message, Metadata, Context) ->
    Timestamp = erlang:system_time(millisecond),
    LogEntry = #{
        timestamp => Timestamp,
        level => Level,
        message => Message,
        metadata => Metadata,
        context => Context,
        node => node(),
        pid => self(),
        module => get_calling_module()
    },
    
    % Send to multiple destinations
    send_to_console(LogEntry),
    send_to_file(LogEntry),
    send_to_metrics_collector(LogEntry),
    
    % Optional: send to external systems
    case application:get_env(cookie_crud, external_logging, false) of
        true -> send_to_external_system(LogEntry);
        false -> ok
    end.

% Request tracing
-module(request_tracer).
-export([trace_request/2]).

trace_request(RequestId, Fun) ->
    StartTime = erlang:monotonic_time(microsecond),
    
    % Set process dictionary for tracing
    put(request_id, RequestId),
    put(start_time, StartTime),
    
    try
        Result = Fun(),
        EndTime = erlang:monotonic_time(microsecond),
        Duration = EndTime - StartTime,
        
        structured_logger:log(info, "Request completed", #{
            request_id => RequestId,
            duration_us => Duration,
            result => success
        }),
        
        Result
    catch
        Class:Reason:Stacktrace ->
            EndTime = erlang:monotonic_time(microsecond),
            Duration = EndTime - StartTime,
            
            structured_logger:log(error, "Request failed", #{
                request_id => RequestId,
                duration_us => Duration,
                error_class => Class,
                error_reason => Reason,
                stacktrace => Stacktrace
            }),
            
            error({Class, Reason, Stacktrace})
    after
        erase(request_id),
        erase(start_time)
    end.
```

### Health Checks and Monitoring

**Health Check Endpoints:**
```erlang
-module(health_check).
-export([check_health/0, check_readiness/0]).

% Kubernetes-style health checks
check_health() ->
    Checks = [
        {database, check_database()},
        {memory, check_memory()},
        {processes, check_process_count()},
        {disk_space, check_disk_space()}
    ],
    
    case lists:all(fun({_Name, Status}) -> Status =:= ok end, Checks) of
        true -> {ok, #{status => healthy, checks => Checks}};
        false -> {error, #{status => unhealthy, checks => Checks}}
    end.

check_readiness() ->
    % Check if application is ready to serve traffic
    Checks = [
        {database_pool, check_db_pool()},
        {http_server, check_http_server()},
        {configuration, check_configuration()}
    ],
    
    case lists:all(fun({_Name, Status}) -> Status =:= ok end, Checks) of
        true -> {ok, #{status => ready, checks => Checks}};
        false -> {error, #{status => not_ready, checks => Checks}}
    end.

check_database() ->
    try
        {ok, _} = cookie_db_pool:with_connection(fun(Db) ->
            esqlite3:q(Db, "SELECT 1")
        end),
        ok
    catch
        _:_ -> error
    end.

check_memory() ->
    MemoryUsage = erlang:memory(total),
    MaxMemory = 1024 * 1024 * 1024,  % 1GB
    case MemoryUsage < MaxMemory of
        true -> ok;
        false -> error
    end.

check_process_count() ->
    ProcessCount = erlang:system_info(process_count),
    MaxProcesses = 100000,
    case ProcessCount < MaxProcesses of
        true -> ok;
        false -> error
    end.

check_disk_space() ->
    % Check available disk space
    {ok, Stats} = file:read_file_info("."),
    % Simplified check - in production, use proper disk space monitoring
    ok.
```

### Security Best Practices

**Input Validation and Sanitization:**
```erlang
-module(input_validator).
-export([validate_cookie_data/1, sanitize_input/1]).

validate_cookie_data(Data) when is_map(Data) ->
    try
        ValidatedData = #{
            cookie => validate_cookie_id(maps:get(<<"cookie">>, Data)),
            user_id => validate_user_id(maps:get(<<"user_id">>, Data))
        },
        {ok, ValidatedData}
    catch
        error:{validation_error, Reason} -> {error, Reason};
        error:{badkey, Key} -> {error, {missing_field, Key}}
    end;
validate_cookie_data(_) ->
    {error, invalid_data_format}.

validate_cookie_id(CookieId) when is_binary(CookieId) ->
    case re:run(CookieId, "^[a-zA-Z0-9_-]{1,64}$") of
        {match, _} -> CookieId;
        nomatch -> error({validation_error, invalid_cookie_format})
    end;
validate_cookie_id(_) ->
    error({validation_error, cookie_must_be_binary}).

validate_user_id(UserId) when is_integer(UserId), UserId > 0 ->
    UserId;
validate_user_id(_) ->
    error({validation_error, invalid_user_id}).

sanitize_input(Data) when is_map(Data) ->
    maps:map(fun(_Key, Value) -> sanitize_value(Value) end, Data);
sanitize_input(Data) ->
    Data.

sanitize_value(Value) when is_binary(Value) ->
    % Remove potentially harmful characters
    re:replace(Value, "[<>\"'&]", "", [global, {return, binary}]);
sanitize_value(Value) ->
    Value.

% Rate limiting
-module(rate_limiter).
-behaviour(gen_server).

-record(state, {
    requests = #{} :: #{inet:ip_address() => {Count :: non_neg_integer(), LastReset :: integer()}}
}).

check_rate_limit(IpAddress) ->
    gen_server:call(?MODULE, {check_rate_limit, IpAddress}).

handle_call({check_rate_limit, IpAddress}, _From, State) ->
    Now = erlang:system_time(second),
    MaxRequests = 100,  % 100 requests
    WindowSize = 60,    % per minute
    
    case maps:get(IpAddress, State#state.requests, {0, Now}) of
        {Count, LastReset} when Now - LastReset >= WindowSize ->
            % Reset window
            NewRequests = maps:put(IpAddress, {1, Now}, State#state.requests),
            {reply, {ok, allowed}, State#state{requests = NewRequests}};
        {Count, LastReset} when Count < MaxRequests ->
            % Increment counter
            NewRequests = maps:put(IpAddress, {Count + 1, LastReset}, State#state.requests),
            {reply, {ok, allowed}, State#state{requests = NewRequests}};
        {Count, LastReset} ->
            % Rate limit exceeded
            TimeRemaining = WindowSize - (Now - LastReset),
            {reply, {error, {rate_limited, TimeRemaining}}, State}
    end.
```

### Deployment and Operations

**Blue-Green Deployment Script:**
```bash
#!/bin/bash
# blue_green_deploy.sh

set -e

NEW_VERSION=$1
CURRENT_VERSION=$(./bin/cookie_crud eval 'application:get_key(cookie_crud, vsn).')

echo "Deploying version $NEW_VERSION (current: $CURRENT_VERSION)"

# 1. Build new release
rebar3 as prod release
rebar3 as prod tar

# 2. Create new deployment directory
mkdir -p "/opt/cookie_crud_$NEW_VERSION"
tar -xzf "_build/prod/rel/cookie_crud/cookie_crud-$NEW_VERSION.tar.gz" -C "/opt/cookie_crud_$NEW_VERSION"

# 3. Hot upgrade (zero downtime)
cp "_build/prod/rel/cookie_crud/releases/$NEW_VERSION/cookie_crud.tar.gz" "/opt/cookie_crud/releases/"
./bin/cookie_crud upgrade "$NEW_VERSION"

if [ $? -eq 0 ]; then
    echo "✅ Hot upgrade successful"
    
    # 4. Health check
    sleep 5
    curl -f http://localhost:8080/health || {
        echo "❌ Health check failed, rolling back"
        ./bin/cookie_crud downgrade "$CURRENT_VERSION"
        exit 1
    }
    
    echo "✅ Deployment successful"
else
    echo "❌ Hot upgrade failed, rolling back"
    ./bin/cookie_crud downgrade "$CURRENT_VERSION"
    exit 1
fi
```

**Production Monitoring Dashboard:**
```erlang
-module(monitoring_dashboard).
-export([start_dashboard/0, get_dashboard_data/0]).

start_dashboard() ->
    % Start web dashboard on separate port
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/", dashboard_handler, []},
            {"/metrics", metrics_handler, []},
            {"/health", health_handler, []}
        ]}
    ]),
    
    cowboy:start_clear(dashboard_listener,
        [{port, 9090}],
        #{env => #{dispatch => Dispatch}}).

get_dashboard_data() ->
    #{
        system_info => get_system_info(),
        application_metrics => get_app_metrics(),
        database_stats => get_db_stats(),
        recent_errors => get_recent_errors()
    }.

get_system_info() ->
    #{
        node => node(),
        uptime => element(1, erlang:statistics(wall_clock)) div 1000,
        memory_usage => erlang:memory(),
        process_count => erlang:system_info(process_count),
        run_queue => erlang:statistics(run_queue),
        schedulers => erlang:system_info(schedulers_online)
    }.

get_app_metrics() ->
    case catch metrics_collector:get_metrics() of
        {'EXIT', _} -> #{error => metrics_not_available};
        Metrics -> Metrics
    end.

get_db_stats() ->
    case catch cookie_db_pool:get_performance_stats() of
        {'EXIT', _} -> #{error => db_stats_not_available};
        Stats -> Stats
    end.
```

## Tooling and Development

### Development Environment Setup

**Go Development Environment:**
```bash
# Install tools
go install golang.org/x/tools/gopls@latest      # Language server
go install github.com/golangci/golangci-lint@latest  # Linter
go install honnef.co/go/tools/cmd/staticcheck@latest # Static analysis

# VS Code extensions
code --install-extension golang.go
```

**Erlang Development Environment:**
```bash
# Install Erlang and tools
brew install erlang rebar3

# Install language server
git clone https://github.com/erlang-ls/erlang_ls.git
cd erlang_ls
make

# Install formatter
rebar3 new plugin erlfmt

# VS Code extensions
code --install-extension pgourlain.erlang
code --install-extension erlang-ls.erlang-ls

# Emacs (many Erlang developers prefer Emacs)
# Add to .emacs:
# (require 'erlang-start)
# (setq erlang-root-dir "/usr/local/lib/erlang")
```

**Development Workflow:**
```erlang
% .erlang file - Erlang shell startup configuration
% Place in your home directory for automatic loading

% Custom shell commands
c(Module).              % Compile module
l(Module).              % Load module
rr("*").                % Load all record definitions
f(VarName).             % Forget variable binding
f().                    % Forget all variable bindings

% Debug helpers
dbg:tracer().           % Start tracer
dbg:p(all, c).          % Trace all processes for calls
dbg:tpl(Module, []).    % Trace all functions in module

% Performance helpers
fprof:start().          % Start function profiler
eprof:start().          % Start time profiler
percept:start().        % Start concurrency profiler

% Observer (system monitor GUI)
observer:start().

% Custom development helpers
-module(dev_utils).
-export([reload_all/0, hot_reload/1, restart_app/1]).

reload_all() ->
    % Reload all modified modules
    [c:l(M) || M <- [cookie_crud, cookie_db_pool, cookie_crud_app]],
    ok.

hot_reload(Module) ->
    % Hot reload specific module
    code:purge(Module),
    c:l(Module),
    logger:info("Module ~p reloaded", [Module]).

restart_app(App) ->
    % Restart application (useful during development)
    application:stop(App),
    application:start(App).
```

### Advanced Tooling

**Static Analysis Pipeline:**
```bash
#!/bin/bash
# scripts/analyze.sh

echo "🔍 Running static analysis pipeline..."

# 1. Compile with warnings
echo "📦 Compiling with warnings..."
rebar3 compile

# 2. Run Dialyzer
echo "🔬 Running Dialyzer..."
rebar3 dialyzer

# 3. Run cross-reference analysis
echo "🔗 Running xref analysis..."
rebar3 xref

# 4. Check code formatting
echo "✨ Checking code formatting..."
rebar3 fmt --check

# 5. Run tests
echo "🧪 Running tests..."
rebar3 eunit
rebar3 ct

# 6. Generate documentation
echo "📚 Generating documentation..."
rebar3 edoc

# 7. Check for unused dependencies
echo "📋 Checking unused dependencies..."
rebar3 deps tree

echo "✅ Analysis complete!"
```

**Custom Rebar3 Plugin for Development:**
```erlang
% plugins/dev_tools.erl
-module(dev_tools).
-export([init/1, do/1, format_error/1]).

init(State) ->
    Provider = providers:create([
        {name, dev_server},
        {module, ?MODULE},
        {bare, true},
        {deps, [compile]},
        {example, "rebar3 dev_server"},
        {short_desc, "Start development server with hot reloading"},
        {desc, "Start development server with automatic code reloading"}
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.

do(State) ->
    % Start application with file watching
    start_file_watcher(),
    application:ensure_all_started(cookie_crud),
    
    io:format("🚀 Development server started~n"),
    io:format("📁 Watching for file changes...~n"),
    io:format("🌐 API available at http://localhost:8080~n"),
    
    % Keep running
    receive
        stop -> ok
    end,
    
    {ok, State}.

start_file_watcher() ->
    spawn(fun() ->
        file_watcher:watch("src/", fun(File) ->
            io:format("🔄 Reloading ~s~n", [File]),
            Module = filename:basename(File, ".erl"),
            c:l(list_to_atom(Module))
        end)
    end).
```

## Migration Guide

### Step-by-Step Migration from Go to Erlang

**Phase 1: Understanding the Paradigm Shift**

1. **Mindset Change:**
   ```erlang
   % From Go's "goroutines + channels" thinking:
   % "How do I share data between goroutines?"
   
   % To Erlang's "processes + messages" thinking:
   % "How do I pass messages between isolated processes?"
   
   % Go approach (shared memory):
   % type Data struct { mu sync.Mutex; value int }
   
   % Erlang approach (message passing):
   % Process holds state, accepts messages to modify it
   ```

2. **Error Handling Philosophy:**
   ```erlang
   % Go: Defensive programming
   % if err != nil { handle error }
   
   % Erlang: "Let it crash" + supervision
   % Don't handle every error - let supervisor restart
   ```

**Phase 2: Syntax Translation**

| **Concept** | **Go** | **Erlang** |
|-------------|--------|------------|
| **Variables** | `var x int = 5` | `X = 5,` |
| **Functions** | `func add(a, b int) int { return a + b }` | `add(A, B) -> A + B.` |
| **Conditionals** | `if x > 0 { ... }` | `case X > 0 of true -> ...; false -> ... end` |
| **Loops** | `for i := 0; i < 10; i++ { ... }` | `lists:foreach(fun(I) -> ... end, lists:seq(1, 10))` |
| **Structs** | `type User struct { Name string }` | `-record(user, {name}).` |
| **Maps** | `map[string]int{"key": 1}` | `#{<<"key">> => 1}` |
| **Arrays/Slices** | `[]string{"a", "b"}` | `[<<"a">>, <<"b">>]` |
| **Error Handling** | `result, err := func()` | `case func() of {ok, Result} -> ...; {error, Reason} -> ... end` |
| **Concurrency** | `go func() { ... }()` | `spawn(fun() -> ... end)` |
| **Communication** | `ch <- data` | `Pid ! Data` |
| **Receiving** | `data := <-ch` | `receive Data -> ... end` |

**Phase 3: Architecture Migration**

```erlang
% 1. Identify Go components
% main.go -> cookie_crud_app.erl (application)
% handlers/ -> individual handler modules
% models/ -> record definitions
% services/ -> gen_server behaviors

% 2. Map Go packages to Erlang modules
% package database -> cookie_db.erl, cookie_db_pool.erl
% package handlers -> cookie_handler.erl
% package models -> cookie_models.hrl (header file)

% 3. Convert shared state to processes
% Go: global variables with mutexes
% Erlang: gen_server processes with state

% Before (Go):
% var globalCache = make(map[string]interface{})
% var mu sync.RWMutex

% After (Erlang):
-module(cache_server).
-behaviour(gen_server).

-record(state, {
    cache = #{} :: map()
}).

get(Key) -> gen_server:call(?MODULE, {get, Key}).
put(Key, Value) -> gen_server:cast(?MODULE, {put, Key, Value}).
```

**Phase 4: Testing Migration**

```erlang
% Convert Go table-driven tests to Erlang test generators

% Go:
% func TestCreateUser(t *testing.T) {
%     tests := []struct {
%         name string
%         input User
%         want error
%     }{
%         {"valid user", User{Name: "John"}, nil},
%         {"empty name", User{Name: ""}, ErrEmptyName},
%     }
%     for _, tt := range tests {
%         got := CreateUser(tt.input)
%         assert.Equal(t, tt.want, got)
%     }
% }

% Erlang:
create_user_test_() ->
    Tests = [
        {"valid user", #{name => <<"John">>}, ok},
        {"empty name", #{name => <<>>}, {error, empty_name}}
    ],
    
    [{
        TestName,
        fun() ->
            Result = create_user(Input),
            ?assertEqual(Expected, Result)
        end
    } || {TestName, Input, Expected} <- Tests].
```

**Phase 5: Performance Considerations**

```erlang
% Go performance patterns -> Erlang equivalents

% 1. Object pooling (Go) -> Process pools (Erlang)
% 2. Memory reuse (Go) -> Binary matching (Erlang)
% 3. Goroutine pools (Go) -> Supervisor trees (Erlang)
% 4. Channels (Go) -> Message queues (Erlang)

% Example: Worker pool migration
% Go worker pool with channels -> Erlang gen_server pool

-module(worker_pool).
-behaviour(gen_server).

% Pool of worker processes instead of channel-based workers
init([PoolSize]) ->
    Workers = [spawn_worker() || _ <- lists:seq(1, PoolSize)],
    {ok, #{workers => Workers, queue => queue:new()}}.

spawn_worker() ->
    spawn_link(fun worker_loop/0).

worker_loop() ->
    receive
        {work, Task, From} ->
            Result = process_task(Task),
            From ! {result, Result},
            worker_loop();
        stop ->
            ok
    end.
```

### Quick Reference Card

```erlang
% Erlang Cheat Sheet for Go Developers

% VARIABLES (immutable!)
X = 42,                    % Assignment (once only!)
{Y, Z} = {1, 2},          % Pattern matching assignment

% FUNCTIONS
Double = fun(X) -> X * 2 end,        % Anonymous function
Double(5).                           % Call function -> 10

% LISTS
Numbers = [1, 2, 3, 4, 5],
[Head | Tail] = Numbers,             % Head = 1, Tail = [2,3,4,5]
Doubled = [X*2 || X <- Numbers],     % List comprehension -> [2,4,6,8,10]

% MAPS
User = #{name => <<"Alice">>, age => 30},
Name = maps:get(name, User),                    % Get value
Updated = User#{age => 31},                     % Update (creates new map)

% PATTERN MATCHING
case User of
    #{name := <<"Alice">>, age := Age} when Age >= 18 ->
        adult;
    #{name := Name} ->
        {minor, Name};
    _ ->
        unknown
end,

% PROCESSES
Pid = spawn(fun() -> io:format("Hello from process!~n") end),
Pid ! {message, data},               % Send message
receive
    {message, Data} -> process_data(Data)
after 5000 ->
    timeout
end,

% ERROR HANDLING
case risky_operation() of
    {ok, Result} -> Result;
    {error, Reason} -> handle_error(Reason)
end,

% COMMON PATTERNS
lists:map(fun(X) -> X * 2 end, [1,2,3]),        % Map
lists:filter(fun(X) -> X > 0 end, [-1,0,1,2]),  % Filter  
lists:foldl(fun(X, Acc) -> X + Acc end, 0, [1,2,3,4]). % Reduce
```

### Learning Resources for Go Developers

**Essential Reading:**
1. **"Learn You Some Erlang for Great Good!"** - Free online, perfect for developers
2. **"Programming Erlang (2nd Edition)"** - By Joe Armstrong (creator of Erlang)
3. **"Designing for Scalability with Erlang/OTP"** - Production-focused
4. **"Erlang and OTP in Action"** - Practical examples
5. **"Property-Based Testing with PropEr, Erlang, and Elixir"** - Advanced testing

**Online Resources:**
- **Erlang.org** - Official documentation
- **Erlang Central** - Blog and tutorials
- **GitHub: erlang/otp** - Source code and examples
- **Erlang Forums** - Community discussion
- **Learn X in Y Minutes: Erlang** - Quick syntax reference

**Practice Projects:**
1. **Chat Server** - Classic Erlang project
2. **URL Shortener** - Database + HTTP
3. **Distributed Key-Value Store** - Multi-node system
4. **WebSocket Server** - Real-time communication
5. **IoT Data Collector** - Sensor simulation

**Community:**
- **#erlang** on Libera.Chat IRC
- **Erlang Slack** (erlanger.slack.com)
- **Reddit: r/erlang**
- **Stack Overflow: [erlang] tag**
- **Erlang User Conference (EUC)** - Annual conference

### When to Choose Erlang Over Go

**✅ Choose Erlang for:**
- **Distributed systems** (multi-node, clustering)
- **High availability** (99.99%+ uptime requirements)
- **Massive concurrency** (millions of lightweight processes)
- **Fault tolerance** (critical systems that can't fail)
- **Hot code swapping** (zero-downtime deployments)
- **Telecom/IoT/Financial** systems
- **Real-time systems** (soft real-time guarantees)
- **Long-running server processes**

**✅ Choose Go for:**
- **System tools and utilities**
- **Single-binary deployment** simplicity
- **CPU-intensive computations**
- **Container/Docker ecosystems**
- **Kubernetes operators**
- **Team familiar with C-like syntax**
- **Quick prototyping and startups**
- **CLI applications**

**🎯 The Sweet Spot for Erlang:**
Erlang excels when you need **reliable, concurrent, distributed systems** that must **stay running**. If your Go application uses many goroutines, has complex error handling, needs graceful degradation, or requires zero-downtime deployments, Erlang might be a better choice.

---

## Final Thoughts

Moving from Go to Erlang isn't just learning new syntax—it's adopting a different philosophy of building systems. Go focuses on simplicity and performance; Erlang focuses on reliability and fault tolerance. Both are excellent tools, but Erlang shines when building systems that must **never stop running**.

The learning curve is steep, but the concepts you'll learn (actor model, fault tolerance, distributed computing) will make you a better systems architect regardless of the language you ultimately use.

**Start small:** Build this Cookie CRUD API, experiment with the shell, crash some processes, and watch supervisors restart them. Once you experience the "let it crash" philosophy in action, you'll understand why Erlang has powered telecom systems for decades.

**Happy coding, and welcome to the world of fault-tolerant systems!** 🚀📡

---

*This comprehensive guide covers everything from basic syntax to production deployment with hot code swapping and comprehensive monitoring. Each section builds upon the previous one, taking you from Go developer to Erlang production expert. Use this Cookie CRUD API project as your playground to experiment with these concepts!*