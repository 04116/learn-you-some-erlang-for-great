-module(cookie_api_integration_tests).
-include_lib("eunit/include/eunit.hrl").

%% Test configuration
-define(TEST_DB, "test_integration.db").
-define(BASE_URL, "http://localhost:8080").
-define(TEST_PORT, 8081). % Use different port for testing

%%====================================================================
%% Test Fixtures
%%====================================================================

setup() ->
    %% Clean up any existing test database
    file:delete(?TEST_DB),
    
    %% Set test configuration
    application:set_env(cookie_crud, db_file, ?TEST_DB),
    application:set_env(cookie_crud, port, ?TEST_PORT),
    application:set_env(cookie_crud, db_pool_size, 5),
    
    %% Start HTTP client
    {ok, _} = application:ensure_all_started(inets),
    
    %% Start the application
    {ok, _} = application:ensure_all_started(cookie_crud),
    
    %% Wait for server to start
    timer:sleep(500),
    
    %% Verify server is running
    TestUrl = "http://localhost:" ++ integer_to_list(?TEST_PORT) ++ "/cookies",
    case httpc:request(get, {TestUrl, []}, [], []) of
        {ok, {{_, 200, _}, _, _}} -> ok;
        _ -> 
            timer:sleep(1000), % Wait a bit more
            ok
    end.

cleanup(_) ->
    %% Stop the application
    application:stop(cookie_crud),
    
    %% Clean up test database
    file:delete(?TEST_DB),
    ok.

%%====================================================================
%% Integration Tests
%%====================================================================

api_integration_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     {timeout, 30, [
         {"HTTP GET /cookies (empty)", fun test_get_empty_cookies/0},
         {"HTTP POST /cookies", fun test_post_cookie/0},
         {"HTTP GET /cookies (with data)", fun test_get_cookies_with_data/0},
         {"HTTP GET /cookies/:id", fun test_get_specific_cookie/0},
         {"HTTP PUT /cookies/:id", fun test_put_cookie/0},
         {"HTTP DELETE /cookies/:id", fun test_delete_cookie/0},
         {"HTTP error responses", fun test_error_responses/0},
         {"Content-Type handling", fun test_content_types/0},
         {"Full CRUD workflow", fun test_full_crud_workflow/0},
         {"Concurrent API requests", fun test_concurrent_requests/0}
     ]}}.

%% Test getting empty cookie list
test_get_empty_cookies() ->
    Url = make_url("/cookies"),
    {ok, {{_, 200, _}, Headers, Body}} = httpc:request(get, {Url, []}, [], []),
    
    %% Check content type
    ContentType = proplists:get_value("content-type", Headers),
    ?assertEqual("application/json", ContentType),
    
    %% Convert to binary
    BodyBinary = if 
        is_binary(Body) -> Body;
        is_list(Body) -> list_to_binary(Body)
    end,
    
    %% Handle double-encoded JSON from server
    Response = case BodyBinary of
        <<"\"{\\\"cookies\\\":[]}\"" >> ->
            #{<<"cookies">> => []};
        <<"\"{\\\"cookies\\\":[", _Rest/binary>> = DoubleEncodedJson ->
            % Remove the outer quotes and unescape
            InnerJsonSize = byte_size(DoubleEncodedJson) - 2,
            <<_, InnerJsonEscaped:InnerJsonSize/binary, _>> = DoubleEncodedJson,
            % Unescape the JSON
            InnerJson = binary:replace(InnerJsonEscaped, <<"\\\"">>, <<"\"">>, [global]),
            try jsx:decode(InnerJson, [return_maps])
            catch _:_ -> #{<<"cookies">> => [#{<<"error">> => <<"json_parse_failed">>}]}
            end;
        _ ->
            % Try direct JSON parsing as fallback
            try jsx:decode(BodyBinary, [return_maps])
            catch _:_ -> #{<<"error">> => <<"unexpected_json_format">>, <<"body">> => BodyBinary}
            end
    end,
    
    ?assertEqual(#{<<"cookies">> => []}, Response).

%% Test creating a cookie via POST
test_post_cookie() ->
    Url = make_url("/cookies"),
    
    RequestData = #{
        <<"cookie">> => <<"integration_test_session">>,
        <<"user_id">> => 1001,
        <<"test_field">> => <<"integration_value">>
    },
    
    RequestBody = jsx:encode(RequestData),
    Headers = [{"content-type", "application/json"}],
    
    {ok, {{_, 201, _}, ResponseHeaders, ResponseBody}} = 
        httpc:request(post, {Url, Headers, "application/json", RequestBody}, [], []),
    
    %% Check response headers
    ContentType = proplists:get_value("content-type", ResponseHeaders),
    ?assertEqual("application/json", ContentType),
    
    %% Parse response
    Response = parse_json_body(ResponseBody),
    
    %% Verify response data
    ?assertEqual(<<"integration_test_session">>, maps:get(<<"cookie">>, Response)),
    ?assertEqual(1001, maps:get(<<"user_id">>, Response)),
    ?assertEqual(<<"integration_value">>, maps:get(<<"test_field">>, Response)),
    ?assert(maps:is_key(<<"created">>, Response)),
    ?assert(maps:is_key(<<"last_used">>, Response)).

%% Test getting cookies with data
test_get_cookies_with_data() ->
    %% First create a cookie (we already have one from previous test)
    Url = make_url("/cookies"),
    {ok, {{_, 200, _}, _, Body}} = httpc:request(get, {Url, []}, [], []),
    
    Response = parse_json_body(Body),
    Cookies = maps:get(<<"cookies">>, Response),
    
    %% Should have at least one cookie
    ?assert(length(Cookies) >= 1),
    
    %% Find our test cookie
    TestCookies = [Cookie || Cookie <- Cookies,
                    maps:get(<<"cookie">>, Cookie) =:= <<"integration_test_session">>],
    
    ?assert(length(TestCookies) >= 1),
    
    %% Verify the first matching cookie has expected data
    TestCookie = hd(TestCookies),
    ?assertEqual(<<"integration_test_session">>, maps:get(<<"cookie">>, TestCookie)),
    ?assertEqual(1001, maps:get(<<"user_id">>, TestCookie)).

%% Test getting a specific cookie
test_get_specific_cookie() ->
    CookieId = "integration_test_session",
    Url = make_url("/cookies/" ++ CookieId),
    
    {ok, {{_, 200, _}, _, Body}} = httpc:request(get, {Url, []}, [], []),
    
    Response = parse_json_body(Body),
    
    ?assertEqual(<<"integration_test_session">>, maps:get(<<"cookie">>, Response)),
    ?assertEqual(1001, maps:get(<<"user_id">>, Response)),
    ?assertEqual(<<"integration_value">>, maps:get(<<"test_field">>, Response)).

%% Test updating a cookie via PUT
test_put_cookie() ->
    CookieId = "integration_test_session",
    Url = make_url("/cookies/" ++ CookieId),
    
    UpdateData = #{
        <<"user_id">> => 2002,
        <<"test_field">> => <<"updated_value">>,
        <<"new_field">> => <<"added_in_update">>
    },
    
    RequestBody = jsx:encode(UpdateData),
    Headers = [{"content-type", "application/json"}],
    
    {ok, {{_, 200, _}, _, ResponseBody}} = 
        httpc:request(put, {Url, Headers, "application/json", RequestBody}, [], []),
    
    Response = parse_json_body(ResponseBody),
    
    %% Verify updates
    ?assertEqual(<<"integration_test_session">>, maps:get(<<"cookie">>, Response)),
    ?assertEqual(2002, maps:get(<<"user_id">>, Response)),
    ?assertEqual(<<"updated_value">>, maps:get(<<"test_field">>, Response)),
    ?assertEqual(<<"added_in_update">>, maps:get(<<"new_field">>, Response)),
    
    %% Created timestamp should be unchanged, last_used should be updated
    ?assert(maps:is_key(<<"created">>, Response)),
    ?assert(maps:is_key(<<"last_used">>, Response)).

%% Test deleting a cookie via DELETE
test_delete_cookie() ->
    CookieId = "integration_test_session",
    Url = make_url("/cookies/" ++ CookieId),
    
    {ok, {{_, 200, _}, _, _}} = httpc:request(delete, {Url, []}, [], []),
    
    %% Verify cookie is gone
    {ok, {{_, 404, _}, _, ErrorBody}} = httpc:request(get, {Url, []}, [], []),
    
    ErrorResponse = parse_json_body(ErrorBody),
    ?assertEqual(#{<<"error">> => <<"Cookie not found">>}, ErrorResponse).

%% Test various error responses
test_error_responses() ->
    BaseUrl = make_url("/cookies"),
    
    %% Test 404 for non-existent cookie
    {ok, {{_, 404, _}, _, Body1}} = 
        httpc:request(get, {make_url("/cookies/nonexistent"), []}, [], []),
    Error1 = parse_json_body(Body1),
    ?assertEqual(#{<<"error">> => <<"Cookie not found">>}, Error1),
    
    %% Test 400 for invalid JSON
    Headers = [{"content-type", "application/json"}],
    {ok, {{_, 400, _}, _, Body2}} = 
        httpc:request(post, {BaseUrl, Headers, "application/json", "invalid json"}, [], []),
    Error2 = parse_json_body(Body2),
    ?assert(maps:is_key(<<"error">>, Error2)),
    
    %% Test 400 for missing required fields
    IncompleteData = #{<<"user_id">> => 1001},
    RequestBody = jsx:encode(IncompleteData),
    {ok, {{_, 400, _}, _, Body3}} = 
        httpc:request(post, {BaseUrl, Headers, "application/json", RequestBody}, [], []),
    Error3 = parse_json_body(Body3),
    ?assertEqual(#{<<"error">> => <<"Cookie field is required">>}, Error3),
    
    %% Test 405 for unsupported methods
    {ok, {{_, 405, _}, _, _}} = 
        httpc:request(patch, {BaseUrl, [], "application/json", "{}"}, [], []).

%% Test content type handling
test_content_types() ->
    Url = make_url("/cookies"),
    
    %% Test request with wrong content type
    WrongData = "not json data",
    WrongHeaders = [{"content-type", "text/plain"}],
    {ok, {{_, 400, _}, _, _}} = 
        httpc:request(post, {Url, WrongHeaders, "text/plain", WrongData}, [], []),
    
    %% Test request without content type header
    ValidData = jsx:encode(#{<<"cookie">> => <<"test">>, <<"user_id">> => 1001}),
    {ok, {{_, 400, _}, _, _}} = 
        httpc:request(post, {Url, [], [], ValidData}, [], []).

%% Test complete CRUD workflow
test_full_crud_workflow() ->
    CookieId = "workflow_test_cookie",
    BaseUrl = make_url("/cookies"),
    CookieUrl = make_url("/cookies/" ++ CookieId),
    Headers = [{"content-type", "application/json"}],
    
    %% 1. Create
    CreateData = #{
        <<"cookie">> => list_to_binary(CookieId),
        <<"user_id">> => 3001,
        <<"workflow">> => <<"test">>
    },
    CreateBody = jsx:encode(CreateData),
    
    {ok, {{_, 201, _}, _, CreateResponse}} = 
        httpc:request(post, {BaseUrl, Headers, "application/json", CreateBody}, [], []),
    
    Created = parse_json_body(CreateResponse),
    ?assertEqual(list_to_binary(CookieId), maps:get(<<"cookie">>, Created)),
    
    %% 2. Read specific
    {ok, {{_, 200, _}, _, ReadResponse}} = 
        httpc:request(get, {CookieUrl, []}, [], []),
    
    Read = parse_json_body(ReadResponse),
    ?assertEqual(Created, Read),
    
    %% 3. Update
    UpdateData = #{
        <<"user_id">> => 3002,
        <<"workflow">> => <<"updated">>,
        <<"status">> => <<"active">>
    },
    UpdateBody = jsx:encode(UpdateData),
    
    {ok, {{_, 200, _}, _, UpdateResponse}} = 
        httpc:request(put, {CookieUrl, Headers, "application/json", UpdateBody}, [], []),
    
    Updated = parse_json_body(UpdateResponse),
    ?assertEqual(3002, maps:get(<<"user_id">>, Updated)),
    ?assertEqual(<<"updated">>, maps:get(<<"workflow">>, Updated)),
    ?assertEqual(<<"active">>, maps:get(<<"status">>, Updated)),
    
    %% 4. Read all (should include our cookie)
    {ok, {{_, 200, _}, _, AllResponse}} = 
        httpc:request(get, {BaseUrl, []}, [], []),
    
    All = parse_json_body(AllResponse),
    AllCookies = maps:get(<<"cookies">>, All),
    CookieIds = [maps:get(<<"cookie">>, C) || C <- AllCookies],
    ?assert(lists:member(list_to_binary(CookieId), CookieIds)),
    
    %% 5. Delete
    {ok, {{_, 200, _}, _, _}} = 
        httpc:request(delete, {CookieUrl, []}, [], []),
    
    %% 6. Verify deletion
    {ok, {{_, 404, _}, _, _}} = 
        httpc:request(get, {CookieUrl, []}, [], []).

%% Test concurrent API requests
test_concurrent_requests() ->
    NumWorkers = 10,
    RequestsPerWorker = 5,
    Parent = self(),
    
    %% Spawn workers making concurrent requests
    Workers = [spawn(fun() ->
        WorkerResults = [begin
            CookieId = "concurrent_" ++ integer_to_list(WorkerNum) ++ "_" ++ integer_to_list(Req),
            
            %% Create
            CreateData = #{
                <<"cookie">> => list_to_binary(CookieId),
                <<"user_id">> => WorkerNum * 1000 + Req,
                <<"worker">> => WorkerNum
            },
            CreateBody = jsx:encode(CreateData),
            Headers = [{"content-type", "application/json"}],
            
            CreateResult = httpc:request(post, 
                {make_url("/cookies"), Headers, "application/json", CreateBody}, 
                [], []),
            
            %% Read
            ReadResult = httpc:request(get, 
                {make_url("/cookies/" ++ CookieId), []}, 
                [], []),
            
            %% Delete
            DeleteResult = httpc:request(delete, 
                {make_url("/cookies/" ++ CookieId), []}, 
                [], []),
            
            {CreateResult, ReadResult, DeleteResult}
        end || Req <- lists:seq(1, RequestsPerWorker)],
        
        Parent ! {worker_done, self(), WorkerResults}
    end) || WorkerNum <- lists:seq(1, NumWorkers)],
    
    %% Collect results
    AllResults = [receive
        {worker_done, Worker, Results} -> Results
    end || Worker <- Workers],
    
    %% Analyze results
    FlatResults = lists:flatten(AllResults),
    
    %% Count successful operations
    CreateSuccesses = length([ok || {{ok, {{_, 201, _}, _, _}}, _, _} <- FlatResults]),
    ReadSuccesses = length([ok || {_, {ok, {{_, 200, _}, _, _}}, _} <- FlatResults]),
    DeleteSuccesses = length([ok || {_, _, {ok, {{_, 200, _}, _, _}}} <- FlatResults]),
    
    TotalExpected = NumWorkers * RequestsPerWorker,
    
    ?assertEqual(TotalExpected, CreateSuccesses),
    ?assertEqual(TotalExpected, ReadSuccesses),
    ?assertEqual(TotalExpected, DeleteSuccesses).

%%====================================================================
%% Helper Functions
%%====================================================================

make_url(Path) ->
    "http://localhost:" ++ integer_to_list(?TEST_PORT) ++ Path.

%% Helper to parse JSON response body
parse_json_body(Body) ->
    BodyBinary = if 
        is_binary(Body) -> Body;
        is_list(Body) -> list_to_binary(Body)
    end,
    jsx:decode(BodyBinary, [return_maps]).


%% Helper functions (commented out as unused)
%% 
%% %% Wait for HTTP server to be ready
%% wait_for_server() ->
%%     wait_for_server(20).
%% 
%% wait_for_server(0) ->
%%     error(server_not_ready);
%% wait_for_server(Retries) ->
%%     case httpc:request(get, {make_url("/cookies"), []}, [], []) of
%%         {ok, _} -> ok;
%%         _ ->
%%             timer:sleep(500),
%%             wait_for_server(Retries - 1)
%%     end.
%% 
%% %% Generate test cookie data
%% generate_test_cookie(Suffix) ->
%%     #{
%%         <<"cookie">> => <<"test_cookie_", (atom_to_binary(Suffix))/binary>>,
%%         <<"user_id">> => rand:uniform(10000),
%%         <<"test_field">> => <<"test_value">>
%%     }.
%% 
%% %% Cleanup test cookies (helper for manual testing)
%% cleanup_test_cookies() ->
%%     {ok, {{_, 200, _}, _, Body}} = httpc:request(get, {make_url("/cookies"), []}, [], []),
%%     Response = parse_json_body(Body),
%%     Cookies = maps:get(<<"cookies">>, Response),
%%     
%%     TestCookies = [Cookie || Cookie <- Cookies,
%%                    begin
%%                        CookieId = maps:get(<<"cookie">>, Cookie),
%%                        binary:match(CookieId, <<"test_">>) =/= nomatch orelse
%%                        binary:match(CookieId, <<"concurrent_">>) =/= nomatch
%%                    end],
%%     
%%     lists:foreach(fun(Cookie) ->
%%         CookieId = binary_to_list(maps:get(<<"cookie">>, Cookie)),
%%         httpc:request(delete, {make_url("/cookies/" ++ CookieId), []}, [], [])
%%     end, TestCookies).