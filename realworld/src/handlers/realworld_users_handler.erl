-module(realworld_users_handler).
-behaviour(cowboy_handler).

-include("../models/realworld_user.hrl").

-export([init/2]).

-spec init(cowboy_req:req(), list()) -> {ok, cowboy_req:req(), any()}.
init(Req0, [register] = State) ->
    Method = cowboy_req:method(Req0),
    case Method of
        <<"POST">> ->
            handle_register(Req0, State);
        _ ->
            method_not_allowed(Req0, State)
    end;

init(Req0, [login] = State) ->
    Method = cowboy_req:method(Req0),
    case Method of
        <<"POST">> ->
            handle_login(Req0, State);
        _ ->
            method_not_allowed(Req0, State)
    end.

%% Handle user registration
-spec handle_register(cowboy_req:req(), list()) -> {ok, cowboy_req:req(), any()}.
handle_register(Req0, State) ->
    {ok, Body, Req1} = cowboy_req:read_body(Req0),
    
    case jsx:decode(Body, [return_maps]) of
        #{<<"user">> := UserData} ->
            Email = maps:get(<<"email">>, UserData, <<>>),
            Username = maps:get(<<"username">>, UserData, <<>>),
            Password = maps:get(<<"password">>, UserData, <<>>),
            
            case realworld_user:create(Email, Username, Password) of
                {ok, User} ->
                    Token = realworld_jwt:generate_token(User#user.id),
                    UserJson = realworld_user:user_to_json(User, Token),
                    Response = #{<<"user">> => UserJson},
                    respond_json(Req1, 201, Response, State);
                {error, ErrorMsg} ->
                    ErrorResponse = #{
                        <<"errors">> => #{
                            <<"body">> => [ErrorMsg]
                        }
                    },
                    respond_json(Req1, 422, ErrorResponse, State)
            end;
        _ ->
            ErrorResponse = #{
                <<"errors">> => #{
                    <<"body">> => [<<"Invalid request format">>]
                }
            },
            respond_json(Req1, 422, ErrorResponse, State)
    end.

%% Handle user login
-spec handle_login(cowboy_req:req(), list()) -> {ok, cowboy_req:req(), any()}.
handle_login(Req0, State) ->
    {ok, Body, Req1} = cowboy_req:read_body(Req0),
    
    case jsx:decode(Body, [return_maps]) of
        #{<<"user">> := UserData} ->
            Email = maps:get(<<"email">>, UserData, <<>>),
            Password = maps:get(<<"password">>, UserData, <<>>),
            
            case realworld_user:find_by_email(Email) of
                {ok, User} ->
                    case realworld_user:verify_password(Password, User#user.password_hash) of
                        true ->
                            Token = realworld_jwt:generate_token(User#user.id),
                            UserJson = realworld_user:user_to_json(User, Token),
                            Response = #{<<"user">> => UserJson},
                            respond_json(Req1, 200, Response, State);
                        false ->
                            ErrorResponse = #{
                                <<"errors">> => #{
                                    <<"body">> => [<<"Invalid email or password">>]
                                }
                            },
                            respond_json(Req1, 422, ErrorResponse, State)
                    end;
                {error, not_found} ->
                    ErrorResponse = #{
                        <<"errors">> => #{
                            <<"body">> => [<<"Invalid email or password">>]
                        }
                    },
                    respond_json(Req1, 422, ErrorResponse, State);
                {error, _} ->
                    ErrorResponse = #{
                        <<"errors">> => #{
                            <<"body">> => [<<"Server error">>]
                        }
                    },
                    respond_json(Req1, 500, ErrorResponse, State)
            end;
        _ ->
            ErrorResponse = #{
                <<"errors">> => #{
                    <<"body">> => [<<"Invalid request format">>]
                }
            },
            respond_json(Req1, 422, ErrorResponse, State)
    end.

%% Helper functions

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
    try
        Body = jsx:encode(Data),
        Headers = #{<<"content-type">> => <<"application/json">>},
        Req2 = cowboy_req:reply(StatusCode, Headers, Body, Req),
        {ok, Req2, State}
    catch
        error:badarg ->
            % Fallback to basic error response if JSON encoding fails
            ErrorBody = jsx:encode(#{<<"errors">> => #{<<"body">> => [<<"Internal server error">>]}}),
            ErrorHeaders = #{<<"content-type">> => <<"application/json">>},
            Req3 = cowboy_req:reply(500, ErrorHeaders, ErrorBody, Req),
            {ok, Req3, State}
    end. 