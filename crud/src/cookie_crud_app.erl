-module(cookie_crud_app).
-behaviour(application).

-export([start/2, stop/1]).

-define(PORT, 8080).

-spec start(application:start_type(), term()) -> {ok, pid()} | {error, term()}.
start(_StartType, _StartArgs) ->
    cookie_db_pool:init_database(),
    
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/cookies", cookie_crud, []},
            {"/cookies/:cookie", cookie_crud, []}
        ]}
    ]),
    
    {ok, _} = cowboy:start_clear(http_listener,
        [{port, ?PORT}],
        #{env => #{dispatch => Dispatch}}
    ),
    
    io:format("Cookie CRUD API server started on port ~p~n", [?PORT]),
    cookie_crud_sup:start_link().

-spec stop(term()) -> ok.
stop(_State) ->
    ok = cowboy:stop_listener(http_listener),
    ok.