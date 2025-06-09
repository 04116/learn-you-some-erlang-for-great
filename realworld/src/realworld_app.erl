-module(realworld_app).
-behaviour(application).

-export([start/2, stop/1]).

-spec start(application:start_type(), any()) -> {ok, pid()} | {error, any()}.
start(_StartType, _StartArgs) ->
    % Start the supervisor
    case realworld_sup:start_link() of
        {ok, Pid} ->
            % Start the HTTP server
            start_http_server(),
            {ok, Pid};
        Error ->
            Error
    end.

-spec stop(any()) -> ok.
stop(_State) ->
    ok.

%% Internal functions

-spec start_http_server() -> ok.
start_http_server() ->
    Port = application:get_env(realworld, http_port, 8080),
    
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/api/users", realworld_users_handler, [register]},
            {"/api/users/login", realworld_users_handler, [login]},
            {"/api/user", realworld_user_handler, []},
            {"/api/profiles/:username", realworld_profiles_handler, []},
            {"/api/profiles/:username/follow", realworld_profiles_handler, [follow]},
            {"/api/articles", realworld_articles_handler, []},
            {"/api/articles/feed", realworld_articles_handler, [feed]},
            {"/api/articles/:slug", realworld_articles_handler, [slug]},
            {"/api/articles/:slug/comments", realworld_comments_handler, []},
            {"/api/articles/:slug/comments/:id", realworld_comments_handler, [id]},
            {"/api/articles/:slug/favorite", realworld_articles_handler, [favorite]},
            {"/api/tags", realworld_tags_handler, []}
        ]}
    ]),
    
    {ok, _} = cowboy:start_clear(realworld_http_listener,
        [{port, Port}],
        #{env => #{dispatch => Dispatch},
          middlewares => [cowboy_router, realworld_middleware, cowboy_handler]}
    ),
    
    logger:info("HTTP server started on port ~p", [Port]),
    ok. 