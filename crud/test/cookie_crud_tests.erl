-module(cookie_crud_tests).
-include_lib("eunit/include/eunit.hrl").

%% Test setup and teardown
-define(TEST_DB, "test_cookies.db").

%%====================================================================
%% Test Fixtures
%%====================================================================

setup() ->
    %% Clean up any existing test database
    file:delete(?TEST_DB),
    
    %% Set test configuration
    application:set_env(cookie_crud, db_file, ?TEST_DB),
    application:set_env(cookie_crud, db_pool_size, 3),
    
    %% Start the application
    {ok, _} = application:ensure_all_started(cookie_crud),
    
    %% Wait for pool to initialize
    timer:sleep(100),
    ok.

cleanup(_) ->
    %% Stop the application
    application:stop(cookie_crud),
    
    %% Clean up test database
    file:delete(?TEST_DB),
    ok.

%%====================================================================
%% Unit Tests
%%====================================================================

%% Test generators for setup/teardown
cookie_crud_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
         {"Create cookie tests", fun test_create_cookie/0},
         {"Get cookie tests", fun test_get_cookie/0},
         {"Get all cookies tests", fun test_get_all_cookies/0},
         {"Update cookie tests", fun test_update_cookie/0},
         {"Delete cookie tests", fun test_delete_cookie/0},
         {"Validation tests", fun test_validation/0},
         {"Error handling tests", fun test_error_handling/0},
         {"Concurrent operations tests", fun test_concurrent_operations/0}
     ]}.

%% Create cookie tests
test_create_cookie() ->
    %% Test valid cookie creation
    ValidData = #{
        <<"cookie">> => <<"test_session_123">>,
        <<"user_id">> => 1001
    },
    
    {ok, Result} = cookie_crud:create_cookie(ValidData),
    
    %% Verify the result contains all expected fields
    ?assertEqual(<<"test_session_123">>, maps:get(<<"cookie">>, Result)),
    ?assertEqual(1001, maps:get(<<"user_id">>, Result)),
    ?assert(maps:is_key(<<"created">>, Result)),
    ?assert(maps:is_key(<<"last_used">>, Result)),
    
    %% Verify timestamps are reasonable (within last minute)
    Now = erlang:system_time(second),
    Created = binary_to_integer(maps:get(<<"created">>, Result)),
    LastUsed = binary_to_integer(maps:get(<<"last_used">>, Result)),
    
    ?assert(Created =< Now),
    ?assert(Created > Now - 60),
    ?assertEqual(Created, LastUsed),
    
    %% Test duplicate cookie creation should fail
    {error, Reason} = cookie_crud:create_cookie(ValidData),
    ?assertEqual(<<"Cookie already exists">>, Reason).

%% Get cookie tests
test_get_cookie() ->
    %% Create a test cookie first
    CookieData = #{
        <<"cookie">> => <<"get_test_cookie">>,
        <<"user_id">> => 2001,
        <<"extra_field">> => <<"test_value">>
    },
    
    {ok, _} = cookie_crud:create_cookie(CookieData),
    
    %% Test successful retrieval
    {ok, Retrieved} = cookie_crud:get_cookie(<<"get_test_cookie">>),
    
    ?assertEqual(<<"get_test_cookie">>, maps:get(<<"cookie">>, Retrieved)),
    ?assertEqual(2001, maps:get(<<"user_id">>, Retrieved)),
    ?assertEqual(<<"test_value">>, maps:get(<<"extra_field">>, Retrieved)),
    ?assert(maps:is_key(<<"created">>, Retrieved)),
    ?assert(maps:is_key(<<"last_used">>, Retrieved)),
    
    %% Test non-existent cookie
    {error, Reason} = cookie_crud:get_cookie(<<"non_existent">>),
    ?assertEqual(<<"Cookie not found">>, Reason).

%% Get all cookies tests
test_get_all_cookies() ->
    %% Start with empty database (should be empty from cleanup)
    {ok, InitialCookies} = cookie_crud:get_all_cookies(),
    InitialCount = length(InitialCookies),
    
    %% Create multiple test cookies
    TestCookies = [
        #{<<"cookie">> => <<"all_test_1">>, <<"user_id">> => 3001},
        #{<<"cookie">> => <<"all_test_2">>, <<"user_id">> => 3002},
        #{<<"cookie">> => <<"all_test_3">>, <<"user_id">> => 3003}
    ],
    
    %% Create all test cookies
    lists:foreach(fun(Data) ->
        {ok, _} = cookie_crud:create_cookie(Data)
    end, TestCookies),
    
    %% Retrieve all cookies
    {ok, AllCookies} = cookie_crud:get_all_cookies(),
    
    %% Should have original count + 3 new cookies
    ?assertEqual(InitialCount + 3, length(AllCookies)),
    
    %% Verify all our test cookies are present
    CookieNames = [maps:get(<<"cookie">>, Cookie) || Cookie <- AllCookies],
    ?assert(lists:member(<<"all_test_1">>, CookieNames)),
    ?assert(lists:member(<<"all_test_2">>, CookieNames)),
    ?assert(lists:member(<<"all_test_3">>, CookieNames)).

%% Update cookie tests
test_update_cookie() ->
    %% Create a test cookie
    InitialData = #{
        <<"cookie">> => <<"update_test_cookie">>,
        <<"user_id">> => 4001,
        <<"status">> => <<"active">>
    },
    
    {ok, Created} = cookie_crud:create_cookie(InitialData),
    OriginalLastUsed = maps:get(<<"last_used">>, Created),
    
    %% Wait a moment to ensure timestamp difference (using seconds, so need 1+ second)
    timer:sleep(1100),
    
    %% Update the cookie
    UpdateData = #{
        <<"user_id">> => 4002,
        <<"status">> => <<"updated">>,
        <<"new_field">> => <<"added_value">>
    },
    
    {ok, Updated} = cookie_crud:update_cookie(<<"update_test_cookie">>, UpdateData),
    
    %% Verify updates
    ?assertEqual(<<"update_test_cookie">>, maps:get(<<"cookie">>, Updated)),
    ?assertEqual(4002, maps:get(<<"user_id">>, Updated)),
    ?assertEqual(<<"updated">>, maps:get(<<"status">>, Updated)),
    ?assertEqual(<<"added_value">>, maps:get(<<"new_field">>, Updated)),
    
    %% Verify timestamps
    ?assertEqual(maps:get(<<"created">>, Created), maps:get(<<"created">>, Updated)),
    UpdatedLastUsed = maps:get(<<"last_used">>, Updated),
    ?assert(binary_to_integer(UpdatedLastUsed) > binary_to_integer(OriginalLastUsed)),
    
    %% Test updating non-existent cookie
    {error, Reason} = cookie_crud:update_cookie(<<"non_existent">>, UpdateData),
    ?assertEqual(<<"Cookie not found">>, Reason).

%% Delete cookie tests
test_delete_cookie() ->
    %% Create a test cookie
    CookieData = #{
        <<"cookie">> => <<"delete_test_cookie">>,
        <<"user_id">> => 5001
    },
    
    {ok, _} = cookie_crud:create_cookie(CookieData),
    
    %% Verify it exists
    {ok, _} = cookie_crud:get_cookie(<<"delete_test_cookie">>),
    
    %% Delete the cookie
    ok = cookie_crud:delete_cookie(<<"delete_test_cookie">>),
    
    %% Verify it's gone
    {error, Reason} = cookie_crud:get_cookie(<<"delete_test_cookie">>),
    ?assertEqual(<<"Cookie not found">>, Reason),
    
    %% Test deleting non-existent cookie
    DeleteResult = cookie_crud:delete_cookie(<<"non_existent">>),
    ?assertEqual(ok, DeleteResult). % Should not error, just return ok

%% Validation tests
test_validation() ->
    %% Test missing cookie field
    InvalidData1 = #{<<"user_id">> => 1001},
    {error, Reason1} = cookie_crud:create_cookie(InvalidData1),
    ?assertEqual(<<"Cookie field is required">>, Reason1),
    
    %% Test missing user_id field
    InvalidData2 = #{<<"cookie">> => <<"test_cookie">>},
    {error, Reason2} = cookie_crud:create_cookie(InvalidData2),
    ?assertEqual(<<"User ID field is required">>, Reason2),
    
    %% Test empty cookie
    InvalidData3 = #{
        <<"cookie">> => <<>>,
        <<"user_id">> => 1001
    },
    {error, Reason3} = cookie_crud:create_cookie(InvalidData3),
    ?assertEqual(<<"Cookie cannot be empty">>, Reason3),
    
    %% Test invalid user_id type
    InvalidData4 = #{
        <<"cookie">> => <<"test_cookie">>,
        <<"user_id">> => <<"not_a_number">>
    },
    {error, Reason4} = cookie_crud:create_cookie(InvalidData4),
    ?assertEqual(<<"User ID must be a positive integer">>, Reason4),
    
    %% Test negative user_id
    InvalidData5 = #{
        <<"cookie">> => <<"test_cookie">>,
        <<"user_id">> => -1
    },
    {error, Reason5} = cookie_crud:create_cookie(InvalidData5),
    ?assertEqual(<<"User ID must be a positive integer">>, Reason5).

%% Error handling tests
test_error_handling() ->
    %% Test malformed JSON (simulated by passing invalid data)
    InvalidData = not_a_map,
    
    %% This should be caught by our validation (badmap error when trying to access maps:get)
    ?assertError({badmap, not_a_map}, cookie_crud:create_cookie(InvalidData)),
    
    %% Test very large cookie name
    LargeCookie = binary:copy(<<"a">>, 1000),
    LargeData = #{
        <<"cookie">> => LargeCookie,
        <<"user_id">> => 1001
    },
    
    %% Should still work (no size limit in our implementation)
    {ok, _} = cookie_crud:create_cookie(LargeData),
    
    %% Clean up
    ok = cookie_crud:delete_cookie(LargeCookie).

%% Concurrent operations tests
test_concurrent_operations() ->
    %% Test concurrent cookie creation
    NumProcesses = 50,
    Parent = self(),
    
    %% Spawn concurrent processes creating cookies
    Pids = [spawn(fun() ->
        CookieID = list_to_binary("concurrent_" ++ integer_to_list(N)),
        Data = #{
            <<"cookie">> => CookieID,
            <<"user_id">> => N
        },
        Result = cookie_crud:create_cookie(Data),
        Parent ! {result, self(), N, Result}
    end) || N <- lists:seq(1, NumProcesses)],
    
    %% Collect results
    Results = [receive {result, Pid, N, Result} -> {N, Result} end || Pid <- Pids],
    
    %% All should succeed
    SuccessResults = [{N, R} || {N, {ok, R}} <- Results],
    ?assertEqual(NumProcesses, length(SuccessResults)),
    
    %% Verify all cookies were created with correct data
    lists:foreach(fun({N, Cookie}) ->
        ExpectedCookieID = list_to_binary("concurrent_" ++ integer_to_list(N)),
        ?assertEqual(ExpectedCookieID, maps:get(<<"cookie">>, Cookie)),
        ?assertEqual(N, maps:get(<<"user_id">>, Cookie))
    end, SuccessResults),
    
    %% Test concurrent updates on the same cookie
    TestCookie = <<"concurrent_update_test">>,
    {ok, _} = cookie_crud:create_cookie(#{
        <<"cookie">> => TestCookie,
        <<"user_id">> => 1001
    }),
    
    %% Spawn concurrent updates
    UpdatePids = [spawn(fun() ->
        UpdateData = #{<<"update_field_", (integer_to_binary(N))/binary>> => N},
        Result = cookie_crud:update_cookie(TestCookie, UpdateData),
        Parent ! {update_result, self(), N, Result}
    end) || N <- lists:seq(1, 10)],
    
    %% Collect update results
    UpdateResults = [receive {update_result, Pid, N, Result} -> {N, Result} end || Pid <- UpdatePids],
    
    %% All updates should succeed
    SuccessUpdates = [{N, R} || {N, {ok, R}} <- UpdateResults],
    ?assertEqual(10, length(SuccessUpdates)),
    
    %% Clean up concurrent test cookies
    lists:foreach(fun(N) ->
        CookieID = list_to_binary("concurrent_" ++ integer_to_list(N)),
        cookie_crud:delete_cookie(CookieID)
    end, lists:seq(1, NumProcesses)),
    
    cookie_crud:delete_cookie(TestCookie).

%%====================================================================
%% Helper Functions
%%====================================================================

%% Helper functions (commented out as unused)
%% 
%% %% Helper to generate test data
%% generate_test_cookie(Suffix) ->
%%     #{
%%         <<"cookie">> => <<"test_cookie_", (atom_to_binary(Suffix))/binary>>,
%%         <<"user_id">> => rand:uniform(10000),
%%         <<"test_field">> => <<"test_value">>
%%     }.
%% 
%% %% Helper to generate random cookie data
%% generate_random_cookie() ->
%%     CookieID = list_to_binary("random_" ++ integer_to_list(rand:uniform(1000000))),
%%     #{
%%         <<"cookie">> => CookieID,
%%         <<"user_id">> => rand:uniform(10000),
%%         <<"random_field">> => list_to_binary("value_" ++ integer_to_list(rand:uniform(1000)))
%%     }.