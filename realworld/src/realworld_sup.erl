-module(realworld_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-define(SERVER, ?MODULE).

-spec start_link() -> {ok, pid()}.
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

-spec init([]) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init([]) ->
    SupFlags = #{strategy => one_for_all,
                 intensity => 0,
                 period => 1},
    
    % Database pool configuration
    DBConfig = application:get_env(realworld, database, []),
    PoolArgs = [
        {name, {local, realworld_db_pool}},
        {worker_module, realworld_db_worker},
        {size, proplists:get_value(pool_size, DBConfig, 10)},
        {max_overflow, 20}
    ],
    
    PoolSpecs = poolboy:child_spec(realworld_db_pool, PoolArgs, DBConfig),
    
    ChildSpecs = [PoolSpecs],
    
    {ok, {SupFlags, ChildSpecs}}. 