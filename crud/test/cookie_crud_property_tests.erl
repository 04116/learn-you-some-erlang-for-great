-module(cookie_crud_property_tests).

%% Only compile property tests if PropEr is available
-ifdef(PROPER).

-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

%% Test configuration
-define(TEST_DB, "test_property.db").
-define(NUMTESTS, 100).

%%====================================================================
%% Property Test Setup
%%====================================================================

setup() ->
    %% Clean up any existing test database
    file:delete(?TEST_DB),
    
    %% Set test configuration
    application:set_env(cookie_crud, db_file, ?TEST_DB),
    application:set_env(cookie_crud, db_pool_size, 5),
    
    %% Start the application
    {ok, _} = application:ensure_all_started(cookie_crud),
    
    %% Wait for initialization
    timer:sleep(100),
    ok.

cleanup(_) ->
    %% Stop the application
    application:stop(cookie_crud),
    
    %% Clean up test database
    file:delete(?TEST_DB),
    ok.

%%====================================================================
%% Property Tests
%%====================================================================

%% Property test runner
property_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
         {"Cookie roundtrip property", 
          ?_assert(proper:quickcheck(prop_cookie_roundtrip(), [{numtests, ?NUMTESTS}]))},
         {"Cookie update property", 
          ?_assert(proper:quickcheck(prop_cookie_update(), [{numtests, ?NUMTESTS}]))},
         {"Cookie deletion property", 
          ?_assert(proper:quickcheck(prop_cookie_deletion(), [{numtests, ?NUMTESTS}]))},
         {"Concurrent operations property", 
          ?_assert(proper:quickcheck(prop_concurrent_operations(), [{numtests, 50}]))},
         {"Data integrity property", 
          ?_assert(proper:quickcheck(prop_data_integrity(), [{numtests, ?NUMTESTS}]))},
         {"Error handling property", 
          ?_assert(proper:quickcheck(prop_error_handling(), [{numtests, ?NUMTESTS}]))}
     ]}.

%%====================================================================
%% Data Generators
%%====================================================================

%% Generate valid cookie IDs
cookie_id() ->
    ?LET(Prefix, elements(["session", "auth", "temp", "user"]),
         ?LET(Suffix, choose(1000, 999999),
              list_to_binary(Prefix ++ "_" ++ integer_to_list(Suffix)))).

%% Generate valid user IDs
user_id() ->
    choose(1, 1000000).

%% Generate valid cookie data
cookie_data() ->
    ?LET({CookieId, UserId}, {cookie_id(), user_id()},
         ?LET(ExtraFields, extra_fields(),
              maps:merge(#{
                  <<"cookie">> => CookieId,
                  <<"user_id">> => UserId
              }, ExtraFields))).

%% Generate extra fields for cookies
extra_fields() ->
    ?LET(Fields, list({binary_field_name(), field_value()}),
         maps:from_list(Fields)).

%% Generate field names
binary_field_name() ->
    ?LET(Name, elements(["status", "type", "metadata", "tag", "category", "note"]),
         list_to_binary(Name)).

%% Generate field values
field_value() ->
    oneof([
        binary_value(),
        integer_value(),
        boolean_value(),
        null
    ]).

binary_value() ->
    ?LET(Value, elements(["active", "inactive", "pending", "verified", "test", "production"]),
         list_to_binary(Value)).

integer_value() ->
    choose(0, 10000).

boolean_value() ->
    elements([true, false]).

%% Generate update data (partial cookie data)
update_data() ->
    ?LET(Fields, non_empty(list({update_field_name(), field_value()})),
         maps:from_list(Fields)).

update_field_name() ->
    oneof([
        <<"user_id">>,
        binary_field_name()
    ]).

%% Generate invalid cookie data for error testing
invalid_cookie_data() ->
    oneof([
        %% Missing cookie field
        #{<<"user_id">> => user_id()},
        %% Missing user_id field
        #{<<"cookie">> => cookie_id()},
        %% Empty cookie
        #{<<"cookie">> => <<>>, <<"user_id">> => user_id()},
        %% Invalid user_id type
        #{<<"cookie">> => cookie_id(), <<"user_id">> => binary_value()},
        %% Negative user_id
        #{<<"cookie">> => cookie_id(), <<"user_id">> => choose(-1000, -1)},
        %% Non-map data
        not_a_map
    ]).

%%====================================================================
%% Properties
%%====================================================================

%% Property: Creating and then retrieving a cookie should return the same data
prop_cookie_roundtrip() ->
    ?FORALL(CookieData, cookie_data(),
        begin
            CookieId = maps:get(<<"cookie">>, CookieData),
            
            %% Clean up any existing cookie with this ID
            cookie_crud:delete_cookie(CookieId),
            
            %% Create the cookie
            case cookie_crud:create_cookie(CookieData) of
                {ok, CreatedCookie} ->
                    %% Retrieve the cookie
                    case cookie_crud:get_cookie(CookieId) of
                        {ok, RetrievedCookie} ->
                            %% Verify core data matches
                            CookieMatches = maps:get(<<"cookie">>, CreatedCookie) =:= 
                                          maps:get(<<"cookie">>, RetrievedCookie),
                            UserIdMatches = maps:get(<<"user_id">>, CreatedCookie) =:= 
                                          maps:get(<<"user_id">>, RetrievedCookie),
                            
                            %% Verify extra fields
                            ExtraFieldsMatch = all_extra_fields_match(CookieData, RetrievedCookie),
                            
                            %% Cleanup
                            cookie_crud:delete_cookie(CookieId),
                            
                            CookieMatches andalso UserIdMatches andalso ExtraFieldsMatch;
                        {error, _} ->
                            %% Cleanup and fail
                            cookie_crud:delete_cookie(CookieId),
                            false
                    end;
                {error, _} ->
                    false
            end
        end).

%% Property: Updating a cookie should preserve the original cookie ID and created timestamp
prop_cookie_update() ->
    ?FORALL({CookieData, UpdateData}, {cookie_data(), update_data()},
        begin
            CookieId = maps:get(<<"cookie">>, CookieData),
            
            %% Clean up any existing cookie
            cookie_crud:delete_cookie(CookieId),
            
            %% Create the cookie
            case cookie_crud:create_cookie(CookieData) of
                {ok, OriginalCookie} ->
                    %% Update the cookie
                    case cookie_crud:update_cookie(CookieId, UpdateData) of
                        {ok, UpdatedCookie} ->
                            %% Verify cookie ID is unchanged
                            CookieIdUnchanged = maps:get(<<"cookie">>, OriginalCookie) =:= 
                                              maps:get(<<"cookie">>, UpdatedCookie),
                            
                            %% Verify created timestamp is unchanged
                            CreatedUnchanged = maps:get(<<"created">>, OriginalCookie) =:= 
                                             maps:get(<<"created">>, UpdatedCookie),
                            
                            %% Verify last_used is updated (should be >= original)
                            OriginalLastUsed = binary_to_integer(maps:get(<<"last_used">>, OriginalCookie)),
                            UpdatedLastUsed = binary_to_integer(maps:get(<<"last_used">>, UpdatedCookie)),
                            LastUsedUpdated = UpdatedLastUsed >= OriginalLastUsed,
                            
                            %% Verify update fields are applied
                            UpdateFieldsApplied = all_update_fields_applied(UpdateData, UpdatedCookie),
                            
                            %% Cleanup
                            cookie_crud:delete_cookie(CookieId),
                            
                            CookieIdUnchanged andalso CreatedUnchanged andalso 
                            LastUsedUpdated andalso UpdateFieldsApplied;
                        {error, _} ->
                            cookie_crud:delete_cookie(CookieId),
                            false
                    end;
                {error, _} ->
                    false
            end
        end).

%% Property: Deleting a cookie should make it unavailable for retrieval
prop_cookie_deletion() ->
    ?FORALL(CookieData, cookie_data(),
        begin
            CookieId = maps:get(<<"cookie">>, CookieData),
            
            %% Clean up any existing cookie
            cookie_crud:delete_cookie(CookieId),
            
            %% Create the cookie
            case cookie_crud:create_cookie(CookieData) of
                {ok, _} ->
                    %% Verify it exists
                    case cookie_crud:get_cookie(CookieId) of
                        {ok, _} ->
                            %% Delete the cookie
                            ok = cookie_crud:delete_cookie(CookieId),
                            
                            %% Verify it's gone
                            case cookie_crud:get_cookie(CookieId) of
                                {error, _} -> true;
                                {ok, _} -> false
                            end;
                        {error, _} ->
                            false
                    end;
                {error, _} ->
                    false
            end
        end).

%% Property: Concurrent operations should not cause data corruption
prop_concurrent_operations() ->
    ?FORALL(CookieDataList, non_empty(list(cookie_data())),
        begin
            %% Ensure unique cookie IDs
            UniqueCookies = lists:ukeysort(1, [{maps:get(<<"cookie">>, CD), CD} || CD <- CookieDataList]),
            CookiesToTest = [CD || {_, CD} <- lists:sublist(UniqueCookies, 10)], % Limit to 10 for performance
            
            %% Clean up any existing cookies
            lists:foreach(fun(CData) ->
                cookie_crud:delete_cookie(maps:get(<<"cookie">>, CData))
            end, CookiesToTest),
            
            %% Create cookies concurrently
            Parent = self(),
            Pids = [spawn(fun() ->
                Result = cookie_crud:create_cookie(CData),
                Parent ! {create_result, self(), maps:get(<<"cookie">>, CData), Result}
            end) || CData <- CookiesToTest],
            
            %% Collect create results
            CreateResults = [receive
                {create_result, Pid, CookieId, Result} -> {CookieId, Result}
            end || Pid <- Pids],
            
            %% All creates should succeed
            AllCreateSucceeded = lists:all(fun({_, {ok, _}}) -> true; (_) -> false end, CreateResults),
            
            %% Verify all cookies exist and have correct data
            VerificationResults = [begin
                case cookie_crud:get_cookie(CookieId) of
                    {ok, Retrieved} ->
                        %% Find original data
                        OriginalData = lists:keyfind(CookieId, 2, 
                            [{maps:get(<<"cookie">>, CD), CD} || CD <- CookiesToTest]),
                        case OriginalData of
                            {CookieId, OrigData} ->
                                maps:get(<<"user_id">>, Retrieved) =:= maps:get(<<"user_id">>, OrigData);
                            false -> false
                        end;
                    {error, _} -> false
                end
            end || {CookieId, {ok, _}} <- CreateResults],
            
            AllVerified = lists:all(fun(X) -> X end, VerificationResults),
            
            %% Cleanup
            lists:foreach(fun({CookieId, _}) ->
                cookie_crud:delete_cookie(CookieId)
            end, CreateResults),
            
            AllCreateSucceeded andalso AllVerified
        end).

%% Property: Data integrity should be maintained across operations
prop_data_integrity() ->
    ?FORALL(Operations, list({operation_type(), cookie_data()}),
        begin
            %% Execute a sequence of operations and verify data integrity
            execute_operations_and_verify(Operations)
        end).

operation_type() ->
    elements([create, get, update, delete]).

%% Property: Invalid data should always result in appropriate errors
prop_error_handling() ->
    ?FORALL(InvalidData, invalid_cookie_data(),
        begin
            %% Attempt to create cookie with invalid data
            case catch cookie_crud:create_cookie(InvalidData) of
                {error, _} -> true; % Expected error
                {'EXIT', _} -> true; % Also acceptable (function_clause, etc.)
                {ok, _} -> false % Should not succeed with invalid data
            end
        end).

%%====================================================================
%% Helper Functions
%%====================================================================

%% Check if all extra fields from original data are preserved
all_extra_fields_match(OriginalData, RetrievedData) ->
    ExtraFields = maps:without([<<"cookie">>, <<"user_id">>], OriginalData),
    maps:fold(fun(Key, Value, Acc) ->
        case maps:get(Key, RetrievedData, undefined) of
            Value -> Acc;
            _ -> false
        end
    end, true, ExtraFields).

%% Check if all update fields are applied
all_update_fields_applied(UpdateData, UpdatedCookie) ->
    maps:fold(fun(Key, Value, Acc) ->
        case maps:get(Key, UpdatedCookie, undefined) of
            Value -> Acc;
            _ -> false
        end
    end, true, UpdateData).

%% Execute a sequence of operations and verify data integrity
execute_operations_and_verify(Operations) ->
    try
        %% Keep track of expected state
        State = execute_operations(Operations, #{}),
        
        %% Verify final state matches expectations
        verify_final_state(State)
    catch
        _:_ -> false
    end.

execute_operations([], State) ->
    State;
execute_operations([{create, CookieData} | Rest], State) ->
    CookieId = maps:get(<<"cookie">>, CookieData),
    case maps:is_key(CookieId, State) of
        true ->
            %% Cookie already exists, skip
            execute_operations(Rest, State);
        false ->
            case cookie_crud:create_cookie(CookieData) of
                {ok, _} ->
                    execute_operations(Rest, State#{CookieId => CookieData});
                {error, _} ->
                    execute_operations(Rest, State)
            end
    end;
execute_operations([{get, CookieData} | Rest], State) ->
    %% Get operation doesn't change state
    execute_operations(Rest, State);
execute_operations([{update, CookieData} | Rest], State) ->
    CookieId = maps:get(<<"cookie">>, CookieData),
    case maps:is_key(CookieId, State) of
        true ->
            %% Update existing cookie
            UpdateData = maps:without([<<"cookie">>], CookieData),
            case cookie_crud:update_cookie(CookieId, UpdateData) of
                {ok, _} ->
                    OriginalData = maps:get(CookieId, State),
                    NewData = maps:merge(OriginalData, UpdateData),
                    execute_operations(Rest, State#{CookieId => NewData});
                {error, _} ->
                    execute_operations(Rest, State)
            end;
        false ->
            %% Cookie doesn't exist, skip
            execute_operations(Rest, State)
    end;
execute_operations([{delete, CookieData} | Rest], State) ->
    CookieId = maps:get(<<"cookie">>, CookieData),
    cookie_crud:delete_cookie(CookieId),
    execute_operations(Rest, maps:remove(CookieId, State)).

verify_final_state(ExpectedState) ->
    %% Get all cookies from the database
    case cookie_crud:get_all_cookies() of
        {ok, ActualCookies} ->
            %% Convert to map for easier comparison
            ActualState = maps:from_list([
                {maps:get(<<"cookie">>, Cookie), Cookie} || Cookie <- ActualCookies
            ]),
            
            %% Check that all expected cookies exist with correct data
            ExpectedExists = maps:fold(fun(CookieId, ExpectedData, Acc) ->
                case maps:get(CookieId, ActualState, undefined) of
                    undefined -> false;
                    ActualData ->
                        %% Check core fields
                        CoreMatch = maps:get(<<"user_id">>, ExpectedData) =:= 
                                   maps:get(<<"user_id">>, ActualData),
                        Acc andalso CoreMatch
                end
            end, true, ExpectedState),
            
            %% Check that no unexpected cookies exist
            NoUnexpected = maps:fold(fun(CookieId, _, Acc) ->
                Acc andalso maps:is_key(CookieId, ExpectedState)
            end, true, ActualState),
            
            %% Cleanup
            maps:foreach(fun(CookieId, _) ->
                cookie_crud:delete_cookie(CookieId)
            end, ActualState),
            
            ExpectedExists andalso NoUnexpected;
        {error, _} ->
            false
    end.

-else.

%% Fallback tests when PropEr is not available
property_test_() ->
    [{"PropEr not available", 
      fun() -> 
          ?debugMsg("PropEr property testing framework not available. Install PropEr to run property tests."),
          ok 
      end}].

-endif.