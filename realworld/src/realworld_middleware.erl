-module(realworld_middleware).
-behaviour(cowboy_middleware).

-export([execute/2]).

-spec execute(cowboy_req:req(), any()) -> {ok, cowboy_req:req(), any()} | {stop, cowboy_req:req()}.
execute(Req0, Env) ->
    % Add CORS headers
    Req1 = add_cors_headers(Req0),
    
    % Handle preflight OPTIONS requests
    Method = cowboy_req:method(Req1),
    case Method of
        <<"OPTIONS">> ->
            Req2 = cowboy_req:reply(200, #{}, <<>>, Req1),
            {stop, Req2};
        _ ->
            % Continue with the request
            {ok, Req1, Env}
    end.

%% Internal functions

-spec add_cors_headers(cowboy_req:req()) -> cowboy_req:req().
add_cors_headers(Req) ->
    Headers = #{
        <<"access-control-allow-origin">> => <<"*">>,
        <<"access-control-allow-methods">> => <<"GET, POST, PUT, DELETE, OPTIONS">>,
        <<"access-control-allow-headers">> => <<"Content-Type, Authorization">>,
        <<"access-control-max-age">> => <<"86400">>,
        <<"content-type">> => <<"application/json">>
    },
    cowboy_req:set_resp_headers(Headers, Req). 