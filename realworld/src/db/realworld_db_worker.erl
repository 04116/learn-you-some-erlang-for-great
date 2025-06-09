-module(realworld_db_worker).
-behaviour(gen_server).
-behaviour(poolboy_worker).

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {conn}).

-spec start_link(list()) -> {ok, pid()}.
start_link(Args) ->
    gen_server:start_link(?MODULE, Args, []).

-spec init(list()) -> {ok, #state{}} | {stop, any()}.
init(Args) ->
    process_flag(trap_exit, true),
    
    Host = proplists:get_value(host, Args, "localhost"),
    Port = proplists:get_value(port, Args, 5432),
    Username = proplists:get_value(username, Args, "postgres"),
    Password = proplists:get_value(password, Args, ""),
    Database = proplists:get_value(database, Args, "realworld_dev"),
    
    case epgsql:connect(Host, Username, Password, [{database, Database}, {port, Port}]) of
        {ok, Conn} ->
            {ok, #state{conn = Conn}};
        {error, Reason} ->
            {stop, Reason}
    end.

handle_call({query, Sql}, _From, #state{conn = Conn} = State) ->
    Result = epgsql:squery(Conn, Sql),
    NormalizedResult = normalize_result(Result),
    {reply, NormalizedResult, State};

handle_call({query, Sql, Params}, _From, #state{conn = Conn} = State) ->
    Result = epgsql:equery(Conn, Sql, Params),
    NormalizedResult = normalize_result(Result),
    {reply, NormalizedResult, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({'EXIT', _, _}, State) ->
    {stop, shutdown, State};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #state{conn = Conn}) ->
    ok = epgsql:close(Conn),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Normalize epgsql results to expected format
normalize_result({ok, Columns, Rows}) ->
    % Extract column names from column metadata
    ColumnNames = [Name || {column, Name, _Type, _Size, _Modifier, _Format, _ColumnType, _TableOid, _ColumnIndex} <- Columns],
    {ok, {length(Rows), ColumnNames, Rows}};
normalize_result({ok, Count}) ->
    {ok, {Count, [], []}};
normalize_result({error, Error}) ->
    {error, Error};
normalize_result(Other) ->
    {error, {unknown_result, Other}}. 