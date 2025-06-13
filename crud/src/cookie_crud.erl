%% =============================================================================
%% Cookie CRUD HTTP Handler - Main Business Logic Module
%% =============================================================================
%% This module implements the HTTP request handler for the Cookie CRUD API.
%%
%% For Go developers: This is similar to your http.Handler interface, but uses
%% Erlang's pattern matching and functional programming approach instead of
%% object-oriented methods.
%%
%% Key Erlang concepts for Go developers:
%% - Atoms: Like Go's string constants but more efficient (e.g., <<"GET">>)
%% - Pattern matching: Instead of switch/case, we match on function parameters
%% - Tagged tuples: {ok, Result} | {error, Reason} instead of (Result, error)
%% - Immutable data: Variables can't be reassigned (like Go's const)
%% - Maps: Similar to Go's map[string]interface{} but with different syntax
%% =============================================================================

-module(cookie_crud).

%% Cowboy HTTP handler callbacks
%% In Go, this would be like implementing http.Handler interface
-export([init/2, terminate/3]).

%% Public API for direct function calls (used by tests)
-export([get_all_cookies/0, get_cookie/1, create_cookie/1, update_cookie/2, delete_cookie/1]).

%% Type definitions (similar to Go's type aliases)
%% cookie_data() is like: type CookieData map[string]interface{}
-type cookie_data() :: #{binary() => term()}.
%% http_status() is like: type HTTPStatus int with range constraint
-type http_status() :: 200..599.

%% =============================================================================
%% HTTP Handler Entry Point
%% =============================================================================
%% This is the main entry point for all HTTP requests, similar to Go's:
%% func (h *Handler) ServeHTTP(w http.ResponseWriter, r *http.Request)
%%
%% Key differences from Go:
%% - Function specification (-spec) declares types upfront
%% - Pattern matching on Method instead of r.Method string comparison
%% - Immutable request object (Req2) instead of mutating ResponseWriter
%% - Returns tuple {ok, NewReq, State} instead of void
%% =============================================================================

-spec init(cowboy_req:req(), term()) -> {ok, cowboy_req:req(), term()}.
init(Req, _State) ->
    %% Record start time for metrics
    StartTime = erlang:monotonic_time(microsecond),

    %% Extract HTTP method (like r.Method in Go)
    %% Returns binary (<<"GET">>) instead of string
    Method = cowboy_req:method(Req),

    %% Extract URL parameter :cookie from route (like mux.Vars(r)["cookie"] in Gorilla)
    %% Returns 'undefined' if no parameter (Go would return "", false)
    Cookie = cowboy_req:binding(cookie, Req),

    %% Route requests using pattern matching instead of if/switch
    %% In Go: switch r.Method { case "GET": ... }
    %% In Erlang: case Method of <<"GET">> -> ... end
    %% Each handler returns a new Req object (immutable updates)
    Req2 = case Method of
        <<"GET">> ->
            handle_get(Req, Cookie);  %% Handle both /cookies and /cookies/:cookie
        <<"POST">> ->
            handle_post(Req);
        <<"PUT">> when Cookie =/= undefined ->
            handle_put(Req, Cookie);
        <<"DELETE">> when Cookie =/= undefined ->
            handle_delete(Req, Cookie);
        _ ->
            %% Default case for unsupported methods or missing required parameters
            reply_error(Req, 405, <<"Method Not Allowed">>)
    end,

    %% Track metrics after request processing
    EndTime = erlang:monotonic_time(microsecond),
    Duration = (EndTime - StartTime) / 1_000_000,  %% Convert to seconds

    %% Record metrics with default status (will be tracked in reply functions)
    %% Go equivalent: prometheus middleware automatically tracks status codes
    cookie_metrics:observe_http_duration(Method, Duration),

    %% Return success tuple with updated request
    %% Go equivalent: return (no return value, but response written to w)
    {ok, Req2, _State}.

%% =============================================================================
%% Cleanup Callback
%% =============================================================================
%% Called when request processing is complete (similar to Go's defer statements)
%% All parameters prefixed with _ are ignored (like Go's _ blank identifier)
%% =============================================================================

-spec terminate(term(), cowboy_req:req(), term()) -> ok.
terminate(_Reason, _Req, _State) ->
    %% No cleanup needed for stateless handler
    %% In Go, this would be like: defer func() { /* cleanup */ }()
    ok.

%% =============================================================================
%% GET Request Handler - Multiple Function Clauses
%% =============================================================================
%% Erlang uses multiple function clauses instead of if/else or method overloading
%% Pattern matching on the second parameter (Cookie) determines which clause runs
%%
%% Go equivalent would be:
%% func handleGet(w http.ResponseWriter, r *http.Request, cookie string) {
%%     if cookie == "" {
%%         // handle list all
%%     } else {
%%         // handle get specific
%%     }
%% }
%% =============================================================================

-spec handle_get(cowboy_req:req(), binary() | undefined) -> cowboy_req:req().
%% First clause: Handle GET /cookies (list all cookies)
%% Pattern matches when Cookie parameter is 'undefined'
handle_get(Req, undefined) ->
    %% Call database function and pattern match on result
    %% Go equivalent: cookies, err := getAllCookies()
    case get_all_cookies() of
        {ok, Cookies} ->
            %% Success case: wrap in response format
            %% Go: json.Marshal(map[string]interface{}{"cookies": cookies})
            reply_json(Req, 200, #{cookies => Cookies});
        {error, Reason} ->
            %% Error case: format error message
            %% Go: fmt.Sprintf("Database error: %v", err)
            reply_error(Req, 500, list_to_binary(io_lib:format("Database error: ~p", [Reason])))
    end;

%% Second clause: Handle GET /cookies/:cookie (get specific cookie)
%% Pattern matches when Cookie parameter has a value
handle_get(Req, Cookie) ->
    %% Pattern matching on database result
    case get_cookie(Cookie) of
        {ok, CookieData} ->
            %% Found: return the cookie data
            reply_json(Req, 200, CookieData);
        {error, <<"Cookie not found">>} ->
            %% Not found: return 404
            reply_error(Req, 404, <<"Cookie not found">>);
        {error, Reason} ->
            %% Other database error: return 500
            reply_error(Req, 500, list_to_binary(io_lib:format("Database error: ~p", [Reason])))
    end.

%% =============================================================================
%% POST Request Handler - Create New Cookie
%% =============================================================================
%% Handles POST /cookies - creates a new cookie from JSON body
%%
%% Go equivalent:
%% func handlePost(w http.ResponseWriter, r *http.Request) {
%%     body, err := io.ReadAll(r.Body)
%%     if err != nil { /* handle error */ }
%%     var data map[string]interface{}
%%     if err := json.Unmarshal(body, &data); err != nil { /* handle error */ }
%% }
%% =============================================================================

-spec handle_post(cowboy_req:req()) -> cowboy_req:req().
handle_post(Req) ->
    %% Check Content-Type header first
    case cowboy_req:header(<<"content-type">>, Req) of
        <<"application/json">> ->
            %% Correct content type, proceed with processing
            handle_post_with_json(Req);
        <<"application/json", _/binary>> ->
            %% Content type starts with application/json (may have charset)
            handle_post_with_json(Req);
        undefined ->
            %% Missing Content-Type header
            reply_error(Req, 400, <<"Content-Type header is required">>);
        _Other ->
            %% Wrong Content-Type
            reply_error(Req, 400, <<"Content-Type must be application/json">>)
    end.

-spec handle_post_with_json(cowboy_req:req()) -> cowboy_req:req().
handle_post_with_json(Req) ->
    %% Read request body (like io.ReadAll(r.Body) in Go)
    %% Returns tuple {ok, Body, UpdatedReq} - notice immutable request handling
    {ok, Body, Req2} = cowboy_req:read_body(Req),

    %% Parse JSON body with nested pattern matching
    case decode_json(Body) of
        {ok, Data} ->
            %% JSON parsing succeeded, now try to create cookie
            case create_cookie(Data) of
                {ok, CreatedData} ->
                    %% Success: return 201 Created with the new cookie data
                    reply_json(Req2, 201, CreatedData);
                {error, duplicate} ->
                    %% Specific error: cookie already exists (UNIQUE constraint)
                    reply_error(Req2, 409, <<"Cookie already exists">>);
                {error, <<"Cookie field is required">>} ->
                    %% Validation error: missing required field
                    reply_error(Req2, 400, <<"Cookie field is required">>);
                {error, <<"Cookie cannot be empty">>} ->
                    %% Validation error: empty field
                    reply_error(Req2, 400, <<"Cookie cannot be empty">>);
                {error, <<"User ID field is required">>} ->
                    %% Validation error: missing user ID
                    reply_error(Req2, 400, <<"User ID field is required">>);
                {error, <<"User ID must be a positive integer">>} ->
                    %% Validation error: invalid user ID
                    reply_error(Req2, 400, <<"User ID must be a positive integer">>);
                {error, Reason} when is_binary(Reason) ->
                    %% Other validation error (anything that's a binary message)
                    reply_error(Req2, 400, Reason);
                {error, Reason} ->
                    %% Database error (complex error terms)
                    reply_error(Req2, 500, list_to_binary(io_lib:format("Database error: ~p", [Reason])))
            end;
        {error, _} ->
            %% JSON parsing failed (malformed JSON)
            %% In Go: if err := json.Unmarshal(...); err != nil
            reply_error(Req2, 400, <<"Invalid JSON">>)
    end.

%% =============================================================================
%% PUT Request Handler - Update Existing Cookie
%% =============================================================================
%% Handles PUT /cookies/:cookie - updates an existing cookie
%% Similar structure to POST but for updates instead of creation
%% =============================================================================

-spec handle_put(cowboy_req:req(), binary()) -> cowboy_req:req().
handle_put(Req, Cookie) ->
    %% Read and parse request body (same pattern as POST)
    {ok, Body, Req2} = cowboy_req:read_body(Req),
    case decode_json(Body) of
        {ok, Data} ->
            %% JSON valid, attempt update
            case update_cookie(Cookie, Data) of
                {ok, UpdatedData} ->
                    %% Success: return updated cookie data
                    reply_json(Req2, 200, UpdatedData);
                {error, <<"Cookie not found">>} ->
                    %% Cookie doesn't exist for update
                    reply_error(Req2, 404, <<"Cookie not found">>);
                {error, Reason} ->
                    %% Database error during update
                    reply_error(Req2, 500, list_to_binary(io_lib:format("Database error: ~p", [Reason])))
            end;
        {error, _} ->
            %% Invalid JSON in request body
            reply_error(Req2, 400, <<"Invalid JSON">>)
    end.

%% =============================================================================
%% DELETE Request Handler - Remove Cookie
%% =============================================================================
%% Handles DELETE /cookies/:cookie - removes a cookie by ID
%% Simpler than POST/PUT since no body parsing required
%% =============================================================================

-spec handle_delete(cowboy_req:req(), binary()) -> cowboy_req:req().
handle_delete(Req, Cookie) ->
    %% Attempt to delete cookie (no body to parse)
    case delete_cookie(Cookie) of
        ok ->
            %% Success: return confirmation message
            reply_json(Req, 200, #{message => <<"Cookie deleted successfully">>});
        {error, Reason} ->
            %% Database error during deletion
            reply_error(Req, 500, list_to_binary(io_lib:format("Database error: ~p", [Reason])))
    end.

%% =============================================================================
%% Database Functions - Data Layer
%% =============================================================================
%% These functions handle database operations using SQLite
%%
%% Key Erlang/Go differences in database handling:
%% - Connection pooling handled by separate module (cookie_db_pool)
%% - List comprehensions instead of for loops
%% - Pattern matching on query results instead of scanning
%% - Immutable data structures throughout
%% =============================================================================

-spec get_all_cookies() -> {ok, [cookie_data()]} | {error, term()}.
get_all_cookies() ->
    %% Check if caching is enabled
    ClusterConfig = application:get_env(cookie_crud, cluster, #{}),
    CacheEnabled = maps:get(cache_enabled, ClusterConfig, true),

    case CacheEnabled of
        true ->
            %% Try cache first
            case cookie_cache:get(<<"all_cookies">>) of
                {ok, Cookies} ->
                    {ok, Cookies};
                miss ->
                    %% Cache miss, fetch from database
                    get_all_cookies_from_db()
            end;
        false ->
            %% Cache disabled, go directly to database
            get_all_cookies_from_db()
    end.

-spec get_all_cookies_from_db() -> {ok, [cookie_data()]} | {error, term()}.
get_all_cookies_from_db() ->
    %% Track database operation for metrics
    cookie_metrics:increment_db_operations(list_cookies),

    Query = "SELECT Data FROM Cookie ORDER BY Created DESC",

    %% Use connection pool pattern (like Go's sql.DB with connection pooling)
    %% cookie_db_pool:with_connection/1 is similar to db.Query() in Go
    case cookie_db_pool:with_connection(fun(Db) ->
        case esqlite3:q(Db, Query) of
            Rows when is_list(Rows) ->
                %% List comprehension: decode each JSON row
                %% Go equivalent:
                %% var cookies []CookieData
                %% for rows.Next() {
                %%     var data string
                %%     rows.Scan(&data)
                %%     cookie := decodeJSON(data)
                %%     cookies = append(cookies, cookie)
                %% }
                Cookies = [decode_json(Data) || [Data] <- Rows],
                %% Filter only successful JSON decodes (remove errors)
                [Cookie || {ok, Cookie} <- Cookies];
            {error, Reason} ->
                %% Database query error - throw exception to be caught by pool
                error(Reason)
        end
    end) of
        {ok, Cookies} ->
            %% Cache the result for future requests (TTL from config)
            ClusterConfig = application:get_env(cookie_crud, cluster, #{}),
            TTL = maps:get(cache_ttl, ClusterConfig, 300),
            cookie_cache:put(<<"all_cookies">>, Cookies, TTL),
            {ok, Cookies};
        {error, {error, Reason, _}} -> {error, Reason};
        {error, Reason} -> {error, Reason}
    end.

-spec get_cookie(binary()) -> {ok, cookie_data()} | {error, term()}.
get_cookie(Cookie) ->
    %% Check if caching is enabled
    ClusterConfig = application:get_env(cookie_crud, cluster, #{}),
    CacheEnabled = maps:get(cache_enabled, ClusterConfig, true),

    case CacheEnabled of
        true ->
            %% Try cache first
            CacheKey = <<"cookie:", Cookie/binary>>,
            case cookie_cache:get(CacheKey) of
                {ok, CookieData} ->
                    {ok, CookieData};
                miss ->
                    %% Cache miss, fetch from database
                    get_cookie_from_db(Cookie, CacheKey)
            end;
        false ->
            %% Cache disabled, go directly to database
            get_cookie_from_db(Cookie, undefined)
    end.

-spec get_cookie_from_db(binary(), binary() | undefined) -> {ok, cookie_data()} | {error, term()}.
get_cookie_from_db(Cookie, CacheKey) ->
    %% Track database operation for metrics
    cookie_metrics:increment_db_operations(get_cookie),

    Query = "SELECT Data FROM Cookie WHERE Cookie = ?",

    case cookie_db_pool:with_connection(fun(Db) ->
        %% Parameterized query (prevents SQL injection like Go's db.Query(query, args...))
        case esqlite3:q(Db, Query, [Cookie]) of
            [[Data]] ->
                %% Single row returned: pattern match on [[Data]] structure
                %% Go: rows.Next(); rows.Scan(&data)
                decode_json(Data);
            [] ->
                %% No rows returned: cookie not found
                %% Go: if !rows.Next() { return nil, ErrNotFound }
                {error, <<"Cookie not found">>};
            {error, Reason} ->
                error(Reason)
        end
    end) of
        %% Complex nested pattern matching on pool result
        {ok, {ok, Data}} ->
            %% Cache the result if caching is enabled
            case CacheKey of
                undefined -> ok;  %% Caching disabled
                _ ->
                    ClusterConfig = application:get_env(cookie_crud, cluster, #{}),
                    TTL = maps:get(cache_ttl, ClusterConfig, 300),
                    cookie_cache:put(CacheKey, Data, TTL)
            end,
            {ok, Data};
        {ok, {error, <<"Cookie not found">>}} -> {error, <<"Cookie not found">>};
        {error, {error, Reason, _}} -> {error, Reason};
        {error, Reason} -> {error, Reason}
    end.

-spec create_cookie(cookie_data()) -> {ok, cookie_data()} | {error, term()}.
create_cookie(Data) ->
    %% Track database operation for metrics
    cookie_metrics:increment_db_operations(create_cookie),

    %% Validate input data first
    case validate_cookie_data(Data) of
        ok ->
            %% Add timestamps to cookie data (like setting CreatedAt/UpdatedAt in Go structs)
            %% maps:merge/2 is like Go's: for k, v := range newFields { data[k] = v }
            DataWithTimestamp = maps:merge(Data, #{
                <<"created">> => format_timestamp(erlang:system_time(second)),
                <<"last_used">> => format_timestamp(erlang:system_time(second))
            }),

            Json = encode_json(DataWithTimestamp),
            Query = "INSERT INTO Cookie (Data) VALUES (?)",

            case cookie_db_pool:with_connection(fun(Db) ->
                %% Try-catch block for handling database constraints
                %% Go equivalent: if _, err := db.Exec(query, args...); err != nil
                try esqlite3:q(Db, Query, [Json]) of
                    [] ->
                        %% Empty result means successful INSERT
                        DataWithTimestamp;
                    {error, Reason} ->
                        error(Reason)
                catch
                    %% Catch specific UNIQUE constraint violation (like Go's duplicate key error)
                    error:{sqlite_error, "UNIQUE constraint failed: Cookie.Cookie"} ->
                        error(duplicate);
                    error:Reason ->
                        error(Reason)
                end
            end) of
                {ok, Result} ->
                    %% Invalidate cache on successful create
                    invalidate_cookie_cache(),
                    {ok, Result};
                {error, {error, duplicate, _}} -> {error, <<"Cookie already exists">>};
                {error, {error, 2067, _}} -> {error, <<"Cookie already exists">>};  %% SQLITE_CONSTRAINT_UNIQUE
                {error, {error, Reason, _}} -> {error, Reason};
                {error, Reason} when Reason =:= 2067 -> {error, <<"Cookie already exists">>};
                {error, Reason} -> {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

-spec update_cookie(binary(), cookie_data()) -> {ok, cookie_data()} | {error, term()}.
update_cookie(Cookie, Data) ->
    %% Track database operation for metrics
    cookie_metrics:increment_db_operations(update_cookie),

    %% First check if cookie exists (read-before-write pattern)
    case get_cookie(Cookie) of
        {ok, ExistingData} ->
            %% Merge existing data with updates, plus new timestamp
            %% Double merge: first add timestamp, then merge with existing
            %% Go: existingData["last_used"] = time.Now(); merge(existingData, newData)
            UpdatedData = maps:merge(ExistingData, maps:merge(Data, #{
                <<"last_used">> => format_timestamp(erlang:system_time(second))
            })),

            Json = encode_json(UpdatedData),
            Query = "UPDATE Cookie SET Data = ? WHERE Cookie = ?",

            case cookie_db_pool:with_connection(fun(Db) ->
                case esqlite3:q(Db, Query, [Json, Cookie]) of
                    [] -> UpdatedData;  %% UPDATE succeeded
                    {error, Reason} -> error(Reason)
                end
            end) of
                {ok, Result} ->
                    %% Invalidate cache on successful update
                    invalidate_cookie_cache(Cookie),
                    {ok, Result};
                {error, {error, Reason, _}} -> {error, Reason};
                {error, Reason} -> {error, Reason}
            end;
        {error, <<"Cookie not found">>} ->
            %% Cookie doesn't exist - can't update what doesn't exist
            {error, <<"Cookie not found">>}
    end.

-spec delete_cookie(binary()) -> ok | {error, term()}.
delete_cookie(Cookie) ->
    %% Track database operation for metrics
    cookie_metrics:increment_db_operations(delete_cookie),

    Query = "DELETE FROM Cookie WHERE Cookie = ?",

    case cookie_db_pool:with_connection(fun(Db) ->
        case esqlite3:q(Db, Query, [Cookie]) of
            [] ->
                %% DELETE query succeeded, check if any rows were affected
                %% Go: result, err := db.Exec(query, args); rowsAffected, _ := result.RowsAffected()
                case esqlite3:changes(Db) of
                    Changes when Changes > 0 -> ok;
                    0 -> ok  %% Even if no rows deleted, return ok (idempotent delete)
                end;
            {error, Reason} ->
                error(Reason)
        end
    end) of
        {ok, ok} ->
            %% Invalidate cache on successful delete
            invalidate_cookie_cache(Cookie),
            ok;
        {error, {error, Reason, _}} -> {error, Reason};
        {error, Reason} -> {error, Reason}
    end.

%% =============================================================================
%% Utility Functions - JSON and Response Helpers
%% =============================================================================
%% These functions handle JSON encoding/decoding and HTTP responses
%% Similar to Go's json.Marshal/Unmarshal and http response helpers
%% =============================================================================

-spec encode_json(term()) -> binary().
encode_json(Data) ->
    %% Simple JSON encoding using jsx library
    %% Go equivalent: json.Marshal(data)
    jsx:encode(Data).

-spec decode_json(binary() | string()) -> {ok, cookie_data()} | {error, term()}.
%% Function clause for binary input (most common)
decode_json(Json) when is_binary(Json) ->
    try
        %% Try to decode JSON with return_maps option (creates maps instead of proplists)
        %% Go equivalent: json.Unmarshal(data, &result)
        case jsx:decode(Json, [return_maps]) of
            Map when is_map(Map) -> {ok, Map};
            _ -> {error, invalid_json}
        end
    catch
        %% Catch any JSON parsing errors
        %% Go equivalent: if err := json.Unmarshal(...); err != nil
        _:_ -> {error, invalid_json}
    end;
%% Function clause for string input (convert to binary first)
decode_json(Json) when is_list(Json) ->
    decode_json(iolist_to_binary(Json)).

-spec format_timestamp(integer()) -> binary().
format_timestamp(UnixTime) ->
    %% Convert Unix timestamp to binary string
    %% Go equivalent: strconv.Itoa(int(unixTime))
    list_to_binary(integer_to_list(UnixTime)).

%% =============================================================================
%% HTTP Response Helpers
%% =============================================================================
%% These functions create HTTP responses with proper headers and JSON content
%% Similar to Go's http.ResponseWriter methods
%% =============================================================================

-spec reply_json(cowboy_req:req(), http_status(), term()) -> cowboy_req:req().
reply_json(Req, Status, Data) ->
    %% Track HTTP request metrics (like prometheus middleware in Go)
    Method = cowboy_req:method(Req),
    cookie_metrics:increment_http_requests(Method, Status),

    Json = encode_json(Data),
    %% Send JSON response with proper Content-Type header
    %% Go equivalent:
    %% w.Header().Set("Content-Type", "application/json")
    %% w.WriteHeader(status)
    %% w.Write(jsonData)
    cowboy_req:reply(Status,
        #{<<"content-type">> => <<"application/json">>},
        Json, Req).

-spec reply_error(cowboy_req:req(), http_status(), binary()) -> cowboy_req:req().
reply_error(Req, Status, Message) ->
    %% Track HTTP request metrics (like prometheus middleware in Go)
    Method = cowboy_req:method(Req),
    cookie_metrics:increment_http_requests(Method, Status),

    %% Wrap error message in JSON object
    %% Go: json.Marshal(map[string]string{"error": message})
    Json = encode_json(#{error => Message}),
    cowboy_req:reply(Status,
        #{<<"content-type">> => <<"application/json">>},
        Json, Req).

%% =============================================================================
%% Validation Functions
%% =============================================================================
%% Input validation for cookie data
%% Similar to Go's validation libraries or manual validation
%% =============================================================================

-spec validate_cookie_data(cookie_data()) -> ok | {error, binary()}.
validate_cookie_data(Data) ->
    %% Check if cookie field exists
    case maps:get(<<"cookie">>, Data, undefined) of
        undefined ->
            {error, <<"Cookie field is required">>};
        <<>> ->
            {error, <<"Cookie cannot be empty">>};
        CookieValue when is_binary(CookieValue) ->
            %% Check if user_id field exists and is valid
            case maps:get(<<"user_id">>, Data, undefined) of
                undefined ->
                    {error, <<"User ID field is required">>};
                UserID when is_integer(UserID), UserID > 0 ->
                    ok;
                UserID when is_integer(UserID) ->
                    {error, <<"User ID must be a positive integer">>};
                _ ->
                    {error, <<"User ID must be a positive integer">>}
            end;
        _ ->
            {error, <<"Cookie must be a string">>}
    end.

%% =============================================================================
%% Cache Management Functions
%% =============================================================================

-spec invalidate_cookie_cache() -> ok.
invalidate_cookie_cache() ->
    %% Invalidate the all_cookies cache entry
    cookie_cache:delete(<<"all_cookies">>),
    ok.

-spec invalidate_cookie_cache(binary()) -> ok.
invalidate_cookie_cache(Cookie) ->
    %% Invalidate specific cookie cache entry
    CacheKey = <<"cookie:", Cookie/binary>>,
    cookie_cache:delete(CacheKey),
    %% Also invalidate the all_cookies cache since the list has changed
    cookie_cache:delete(<<"all_cookies">>),
    ok.
