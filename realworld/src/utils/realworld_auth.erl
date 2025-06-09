-module(realworld_auth).

-export([
    get_current_user/1,
    extract_token/1,
    require_auth/1
]).

%% Get current user from request
-spec get_current_user(cowboy_req:req()) -> {ok, any()} | {error, unauthorized}.
get_current_user(Req) ->
    case extract_token(Req) of
        {ok, Token} ->
            case realworld_jwt:extract_user_id(Token) of
                {ok, UserId} ->
                    case realworld_user:find_by_id(UserId) of
                        {ok, User} ->
                            {ok, User};
                        {error, not_found} ->
                            {error, unauthorized};
                        {error, _} ->
                            {error, unauthorized}
                    end;
                {error, _} ->
                    {error, unauthorized}
            end;
        {error, _} ->
            {error, unauthorized}
    end.

%% Extract token from Authorization header
-spec extract_token(cowboy_req:req()) -> {ok, binary()} | {error, no_token}.
extract_token(Req) ->
    case cowboy_req:header(<<"authorization">>, Req) of
        undefined ->
            {error, no_token};
        AuthHeader when is_binary(AuthHeader) ->
            case binary:split(AuthHeader, <<" ">>) of
                [<<"Token">>, Token] when Token =/= <<"">> ->
                    {ok, Token};
                _ ->
                    {error, no_token}
            end;
        _ ->
            {error, no_token}
    end.

%% Require authentication middleware
-spec require_auth(cowboy_req:req()) -> {ok, any(), cowboy_req:req()} | {error, cowboy_req:req()}.
require_auth(Req) ->
    case get_current_user(Req) of
        {ok, User} ->
            {ok, User, Req};
        {error, unauthorized} ->
            ErrorResponse = #{
                <<"errors">> => #{
                    <<"body">> => [<<"Unauthorized">>]
                }
            },
            Body = jsx:encode(ErrorResponse),
            Req2 = cowboy_req:reply(401, #{}, Body, Req),
            {error, Req2}
    end. 