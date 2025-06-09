-module(realworld_jwt).

-export([
    generate_token/1,
    verify_token/1,
    extract_user_id/1
]).

%% Generate JWT token for user
-spec generate_token(binary()) -> binary().
generate_token(UserId) ->
    JWTConfig = application:get_env(realworld, jwt, []),
    Secret = proplists:get_value(secret, JWTConfig, "default-secret"),
    ExpiryHours = proplists:get_value(expiry_hours, JWTConfig, 24),
    
    % Calculate expiry time
    Now = calendar:datetime_to_gregorian_seconds(calendar:universal_time()),
    ExpirySeconds = Now + (ExpiryHours * 3600),
    
    % Create JWT payload
    Payload = #{
        <<"sub">> => UserId,
        <<"iat">> => Now,
        <<"exp">> => ExpirySeconds
    },
    
    % Generate token
    case jwerl:sign(Payload, hs256, Secret) of
        {ok, Token} ->
            Token;
        {error, _Reason} ->
            <<"">>
    end.

%% Verify JWT token
-spec verify_token(binary()) -> {ok, map()} | {error, any()}.
verify_token(<<>>) ->
    {error, empty_token};
verify_token(Token) ->
    JWTConfig = application:get_env(realworld, jwt, []),
    Secret = proplists:get_value(secret, JWTConfig, "default-secret"),
    
    case jwerl:verify(Token, hs256, Secret) of
        {ok, Claims} ->
            % Check if token is expired
            case maps:get(<<"exp">>, Claims, undefined) of
                undefined ->
                    {error, no_expiry};
                Exp ->
                    Now = calendar:datetime_to_gregorian_seconds(calendar:universal_time()),
                    if
                        Exp > Now ->
                            {ok, Claims};
                        true ->
                            {error, expired}
                    end
            end;
        {error, Reason} ->
            {error, Reason}
    end.

%% Extract user ID from token
-spec extract_user_id(binary()) -> {ok, binary()} | {error, any()}.
extract_user_id(Token) ->
    case verify_token(Token) of
        {ok, Claims} ->
            case maps:get(<<"sub">>, Claims, undefined) of
                undefined ->
                    {error, no_subject};
                UserId ->
                    {ok, UserId}
            end;
        {error, Reason} ->
            {error, Reason}
    end. 