-module(realworld_db).

-export([query/1, query/2, transaction/1]).

%% Simple query without parameters
-spec query(string()) -> {ok, any()} | {error, any()}.
query(Sql) ->
    poolboy:transaction(realworld_db_pool, fun(Worker) ->
        gen_server:call(Worker, {query, Sql})
    end).

%% Parameterized query
-spec query(string(), list()) -> {ok, any()} | {error, any()}.
query(Sql, Params) ->
    poolboy:transaction(realworld_db_pool, fun(Worker) ->
        gen_server:call(Worker, {query, Sql, Params})
    end).

%% Execute multiple queries in a transaction
-spec transaction(fun()) -> {ok, any()} | {error, any()}.
transaction(Fun) ->
    poolboy:transaction(realworld_db_pool, fun(Worker) ->
        gen_server:call(Worker, {query, "BEGIN"}),
        try
            Result = Fun(Worker),
            gen_server:call(Worker, {query, "COMMIT"}),
            {ok, Result}
        catch
            _:Error ->
                gen_server:call(Worker, {query, "ROLLBACK"}),
                {error, Error}
        end
    end). 