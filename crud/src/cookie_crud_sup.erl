-module(cookie_crud_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-define(SERVER, ?MODULE).

-spec start_link() -> {ok, pid()} | ignore | {error, term()}.
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

-spec init([]) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init([]) ->
    SupFlags = #{strategy => one_for_one,
                 intensity => 5,
                 period => 10},

    ChildSpecs = [
        #{id => cookie_db_pool,
          start => {cookie_db_pool, start_link, []},
          restart => permanent,
          shutdown => 5000,
          type => worker,
          modules => [cookie_db_pool]}
    ],

    {ok, {SupFlags, ChildSpecs}}.