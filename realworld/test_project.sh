#!/bin/bash

echo "=== RealWorld Erlang Project Test ==="
echo ""

echo "1. Project Structure:"
echo "--------------------"
find . -name "*.erl" -o -name "*.hrl" -o -name "*.sql" | head -20

echo ""
echo "2. Compilation Test:"
echo "-------------------"
if rebar3 compile; then
    echo "✓ Project compiles successfully!"
else
    echo "✗ Compilation failed"
    exit 1
fi

echo ""
echo "3. Dependency Check:"
echo "-------------------"
rebar3 tree | head -10

echo ""
echo "4. Testing Module Discovery:"
echo "---------------------------"
erl -noshell -pa _build/default/lib/*/ebin -eval "
    Modules = [realworld_user, realworld_jwt, realworld_auth, realworld_users_handler],
    lists:foreach(fun(M) ->
        case code:which(M) of
            non_existing -> 
                io:format(\"✗ Module ~p not found~n\", [M]);
            Path -> 
                io:format(\"✓ Module ~p found at ~s~n\", [M, Path])
        end
    end, Modules),
    halt().
"

echo ""
echo "5. JWT Module Test:"
echo "------------------"
erl -noshell -pa _build/default/lib/*/ebin -eval "
    application:ensure_all_started(crypto),
    application:ensure_all_started(jwerl),
    UserId = <<\"test-user-123\">>,
    try
        Token = realworld_jwt:generate_token(UserId),
        if 
            byte_size(Token) > 10 ->
                io:format(\"✓ JWT token generation works: ~s...~n\", [binary:part(Token, 0, min(50, byte_size(Token)))]);
            true ->
                io:format(\"✗ JWT token generation failed~n\")
        end
    catch
        Error:Reason ->
            io:format(\"✗ JWT test failed: ~p:~p~n\", [Error, Reason])
    end,
    halt().
"

echo ""
echo "6. Password Hashing Test:"
echo "------------------------"
erl -noshell -pa _build/default/lib/*/ebin -eval "
    try
        Password = <<\"testpassword123\">>,
        Hash = realworld_user:hash_password(Password),
        Verified = realworld_user:verify_password(Password, Hash),
        if 
            Verified ->
                io:format(\"✓ Password hashing and verification works~n\");
            true ->
                io:format(\"✗ Password verification failed~n\")
        end
    catch
        Error:Reason ->
            io:format(\"✗ Password test failed: ~p:~p~n\", [Error, Reason])
    end,
    halt().
"

echo ""
echo "7. Available Make Targets:"
echo "-------------------------"
make help

echo ""
echo "=== Test Complete ==="
echo ""
echo "To start the server (requires PostgreSQL):"
echo "  1. Set up PostgreSQL database"
echo "  2. Run: make db-create && make db-migrate"
echo "  3. Run: make run"
echo "  4. Server will be available at http://localhost:8080"
echo ""
echo "API Endpoints implemented:"
echo "  POST /api/users - User registration"
echo "  POST /api/users/login - User login"
echo "  GET /api/user - Current user (requires auth)"
echo "  PUT /api/user - Update user (requires auth)" 