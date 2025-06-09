-module(realworld_articles_handler).
-behaviour(cowboy_handler).

-include("../models/realworld_article.hrl").
-include("../models/realworld_user.hrl").

-export([init/2]).

init(Req0, State) ->
    % Simplified handler for now - would need proper routing based on path/method
    ErrorResponse = #{
        <<"errors">> => #{
            <<"body">> => [<<"Articles functionality coming soon">>]
        }
    },
    Body = jsx:encode(ErrorResponse),
    Req1 = cowboy_req:reply(200, #{<<"content-type">> => <<"application/json">>}, Body, Req0),
    {ok, Req1, State}. 