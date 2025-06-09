-module(realworld_tags_handler).
-behaviour(cowboy_handler).

-export([init/2]).

init(Req0, State) ->
    % TODO: Implement tags functionality
    ErrorResponse = #{
        <<"errors">> => #{
            <<"body">> => [<<"Not implemented yet">>]
        }
    },
    Body = jsx:encode(ErrorResponse),
    Req1 = cowboy_req:reply(501, #{<<"content-type">> => <<"application/json">>}, Body, Req0),
    {ok, Req1, State}. 