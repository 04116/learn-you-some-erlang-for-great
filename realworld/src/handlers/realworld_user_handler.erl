-module(realworld_user_handler).
-behaviour(cowboy_handler).

-include("../models/realworld_user.hrl").

-export([init/2]).

-spec init(cowboy_req:req(), list()) -> {ok, cowboy_req:req(), any()}.
init(Req0, State) ->
    Method = cowboy_req:method(Req0),
    case Method of
        <<"GET">> ->
            handle_get_current_user(Req0, State);
        <<"PUT">> ->
            handle_update_user(Req0, State);
        _ ->
            method_not_allowed(Req0, State)
    end.

%% Handle GET current user
-spec handle_get_current_user(cowboy_req:req(), list()) -> {ok, cowboy_req:req(), any()}.
handle_get_current_user(Req0, State) ->
    case realworld_auth:require_auth(Req0) of
        {ok, User, Req1} ->
            Token = case realworld_auth:extract_token(Req1) of
                {ok, T} -> T;
                _ -> <<"">>
            end,
            UserJson = realworld_user:user_to_json(User, Token),
            Response = #{<<"user">> => UserJson},
            respond_json(Req1, 200, Response, State);
        {error, Req1} ->
            {ok, Req1, State}
    end.

%% Handle PUT update user
-spec handle_update_user(cowboy_req:req(), list()) -> {ok, cowboy_req:req(), any()}.
handle_update_user(Req0, State) ->
    case realworld_auth:require_auth(Req0) of
        {ok, User, Req1} ->
            {ok, Body, Req2} = cowboy_req:read_body(Req1),
            
            case jsx:decode(Body, [return_maps]) of
                #{<<"user">> := UserData} ->
                    % Build update map from user data
                    UpdateMap = build_update_map(UserData),
                    
                    case realworld_user:update(User#user.id, UpdateMap) of
                        {ok, UpdatedUser} ->
                            Token = case realworld_auth:extract_token(Req2) of
                                {ok, T} -> T;
                                _ -> <<"">>
                            end,
                            UserJson = realworld_user:user_to_json(UpdatedUser, Token),
                            Response = #{<<"user">> => UserJson},
                            respond_json(Req2, 200, Response, State);
                        {error, _} ->
                            ErrorResponse = #{
                                <<"errors">> => #{
                                    <<"body">> => [<<"Update failed">>]
                                }
                            },
                            respond_json(Req2, 422, ErrorResponse, State)
                    end;
                _ ->
                    ErrorResponse = #{
                        <<"errors">> => #{
                            <<"body">> => [<<"Invalid request format">>]
                        }
                    },
                    respond_json(Req2, 422, ErrorResponse, State)
            end;
        {error, Req1} ->
            {ok, Req1, State}
    end.

%% Helper functions

-spec build_update_map(map()) -> map().
build_update_map(UserData) ->
    Fields = [
        {<<"email">>, email},
        {<<"username">>, username},
        {<<"bio">>, bio},
        {<<"image">>, image}
    ],
    
    lists:foldl(fun({Key, Field}, Acc) ->
        case maps:get(Key, UserData, undefined) of
            undefined -> Acc;
            Value -> maps:put(Field, Value, Acc)
        end
    end, #{}, Fields).

-spec method_not_allowed(cowboy_req:req(), list()) -> {ok, cowboy_req:req(), any()}.
method_not_allowed(Req0, State) ->
    ErrorResponse = #{
        <<"errors">> => #{
            <<"body">> => [<<"Method not allowed">>]
        }
    },
    respond_json(Req0, 405, ErrorResponse, State).

-spec respond_json(cowboy_req:req(), integer(), map(), list()) -> {ok, cowboy_req:req(), any()}.
respond_json(Req, StatusCode, Data, State) ->
    Body = jsx:encode(Data),
    Headers = #{<<"content-type">> => <<"application/json">>},
    Req2 = cowboy_req:reply(StatusCode, Headers, Body, Req),
    {ok, Req2, State}. 