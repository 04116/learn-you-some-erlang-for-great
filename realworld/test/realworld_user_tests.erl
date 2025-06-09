-module(realworld_user_tests).

-include_lib("eunit/include/eunit.hrl").
-include("../src/models/realworld_user.hrl").

%% Test user validation
validate_email_test() ->
    ?assertEqual(true, realworld_user:validate_email(<<"test@example.com">>)),
    ?assertEqual(false, realworld_user:validate_email(<<"invalid-email">>)),
    ?assertEqual(false, realworld_user:validate_email(<<"">>)).

hash_password_test() ->
    Password = <<"testpassword">>,
    Hash = realworld_user:hash_password(Password),
    ?assert(is_binary(Hash)),
    ?assert(byte_size(Hash) > 20),
    ?assertEqual(true, realworld_user:verify_password(Password, Hash)),
    ?assertEqual(false, realworld_user:verify_password(<<"wrongpassword">>, Hash)).

user_to_json_test() ->
    User = #user{
        id = <<"test-id">>,
        email = <<"test@example.com">>,
        username = <<"testuser">>,
        bio = <<"Test bio">>,
        image = <<"http://example.com/image.jpg">>
    },
    Token = <<"test-token">>,
    Json = realworld_user:user_to_json(User, Token),
    
    ?assertEqual(<<"test@example.com">>, maps:get(<<"email">>, Json)),
    ?assertEqual(<<"test-token">>, maps:get(<<"token">>, Json)),
    ?assertEqual(<<"testuser">>, maps:get(<<"username">>, Json)),
    ?assertEqual(<<"Test bio">>, maps:get(<<"bio">>, Json)),
    ?assertEqual(<<"http://example.com/image.jpg">>, maps:get(<<"image">>, Json)). 