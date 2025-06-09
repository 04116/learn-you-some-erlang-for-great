-module(realworld_comments_handler).
-behaviour(cowboy_handler).

-export([init/2]).

init(Req0, State) ->
    % TODO: Implement comments functionality
    ErrorResponse = #{
        <<"errors">> => #{
            <<"body">> => [<<"Not implemented yet">>]
        }
    },
    Body = jsx:encode(ErrorResponse),
    Req1 = cowboy_req:reply(501, #{<<"content-type">> => <<"application/json">>}, Body, Req0),
    {ok, Req1, State}. 