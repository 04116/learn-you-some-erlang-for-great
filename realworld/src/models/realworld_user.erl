-module(realworld_user).

-include("realworld_user.hrl").

-export([
    create/3,
    find_by_email/1,
    find_by_username/1,
    find_by_id/1,
    verify_password/2,
    hash_password/1,
    update/2,
    user_to_json/2,
    validate_email/1
]).

%% Create a new user
-spec create(binary(), binary(), binary()) -> {ok, #user{}} | {error, any()}.
create(Email, Username, Password) ->
    case validate_user_input(Email, Username, Password) of
        ok ->
            case check_existing_user(Email, Username) of
                ok ->
                    HashedPassword = hash_password(Password),
                    UserId = uuid:uuid_to_string(uuid:get_v4()),
                    
                    Sql = "INSERT INTO users (id, email, username, password_hash) VALUES ($1, $2, $3, $4) RETURNING *",
                    Params = [UserId, Email, Username, HashedPassword],
                    
                    case realworld_db:query(Sql, Params) of
                        {ok, {1, Columns, [Row]}} ->
                            User = row_to_user(Columns, Row),
                            {ok, User};
                        {error, Reason} ->
                            {error, Reason}
                    end;
                Error ->
                    Error
            end;
        Error ->
            Error
    end.

%% Find user by email
-spec find_by_email(binary()) -> {ok, #user{}} | {error, not_found}.
find_by_email(Email) ->
    Sql = "SELECT * FROM users WHERE email = $1",
    case realworld_db:query(Sql, [Email]) of
        {ok, {1, Columns, [Row]}} ->
            {ok, row_to_user(Columns, Row)};
        {ok, {0, _, _}} ->
            {error, not_found};
        {error, Reason} ->
            {error, Reason}
    end.

%% Find user by username
-spec find_by_username(binary()) -> {ok, #user{}} | {error, not_found}.
find_by_username(Username) ->
    Sql = "SELECT * FROM users WHERE username = $1",
    case realworld_db:query(Sql, [Username]) of
        {ok, {1, Columns, [Row]}} ->
            {ok, row_to_user(Columns, Row)};
        {ok, {0, _, _}} ->
            {error, not_found};
        {error, Reason} ->
            {error, Reason}
    end.

%% Find user by ID
-spec find_by_id(binary()) -> {ok, #user{}} | {error, not_found}.
find_by_id(UserId) ->
    Sql = "SELECT * FROM users WHERE id = $1",
    case realworld_db:query(Sql, [UserId]) of
        {ok, {1, Columns, [Row]}} ->
            {ok, row_to_user(Columns, Row)};
        {ok, {0, _, _}} ->
            {error, not_found};
        {error, Reason} ->
            {error, Reason}
    end.

%% Update user
-spec update(binary(), map()) -> {ok, #user{}} | {error, any()}.
update(UserId, UpdateData) ->
    % Build dynamic update query based on provided fields
    {SetClause, Values, ParamCount} = build_update_clause(UpdateData, 1, [], []),
    
    case SetClause of
        [] ->
            % Nothing to update
            find_by_id(UserId);
        _ ->
            Sql = "UPDATE users SET " ++ string:join(SetClause, ", ") ++ 
                  " WHERE id = $" ++ integer_to_list(ParamCount + 1) ++ " RETURNING *",
            Params = Values ++ [UserId],
            
            case realworld_db:query(Sql, Params) of
                {ok, {1, Columns, [Row]}} ->
                    {ok, row_to_user(Columns, Row)};
                {error, Reason} ->
                    {error, Reason}
            end
    end.

%% Verify password
-spec verify_password(binary(), binary()) -> boolean().
verify_password(Password, Hash) ->
    bcrypt:hashpw(Password, Hash) =:= Hash.

%% Hash password
-spec hash_password(binary()) -> binary().
hash_password(Password) ->
    {ok, Salt} = bcrypt:gen_salt(),
    {ok, Hash} = bcrypt:hashpw(Password, Salt),
    Hash.

%% Convert user record to JSON
-spec user_to_json(#user{}, binary()) -> map().
user_to_json(User, Token) ->
    #{
        <<"email">> => User#user.email,
        <<"token">> => Token,
        <<"username">> => User#user.username,
        <<"bio">> => case User#user.bio of null -> <<"">>;  Bio -> Bio end,
        <<"image">> => User#user.image
    }.

%% Internal helper functions

-spec validate_user_input(binary(), binary(), binary()) -> ok | {error, any()}.
validate_user_input(Email, Username, Password) ->
    case {Email, Username, Password} of
        {<<>>, _, _} ->
            {error, <<"Email is required">>};
        {_, <<>>, _} ->
            {error, <<"Username is required">>};
        {_, _, <<>>} ->
            {error, <<"Password is required">>};
        {_, _, P} when byte_size(P) < 6 ->
            {error, <<"Password must be at least 6 characters">>};
        _ ->
            case validate_email(Email) of
                true -> ok;
                false -> {error, <<"Invalid email format">>}
            end
    end.

-spec validate_email(binary()) -> boolean().
validate_email(Email) ->
    % Simple email validation
    case binary:split(Email, <<"@">>) of
        [Local, Domain] when byte_size(Local) > 0, byte_size(Domain) > 0 ->
            case binary:split(Domain, <<".">>) of
                [_, _] -> true;
                _ -> false
            end;
        _ ->
            false
    end.

-spec check_existing_user(binary(), binary()) -> ok | {error, any()}.
check_existing_user(Email, Username) ->
    case find_by_email(Email) of
        {ok, _} ->
            {error, <<"Email already taken">>};
        {error, not_found} ->
            case find_by_username(Username) of
                {ok, _} ->
                    {error, <<"Username already taken">>};
                {error, not_found} ->
                    ok;
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

-spec row_to_user(list(), tuple()) -> #user{}.
row_to_user(Columns, Row) ->
    RowList = tuple_to_list(Row),
    ColumnMap = lists:zip(Columns, RowList),
    
    #user{
        id = proplists:get_value(<<"id">>, ColumnMap),
        email = proplists:get_value(<<"email">>, ColumnMap),
        username = proplists:get_value(<<"username">>, ColumnMap),
        password_hash = proplists:get_value(<<"password_hash">>, ColumnMap),
        bio = proplists:get_value(<<"bio">>, ColumnMap),
        image = proplists:get_value(<<"image">>, ColumnMap),
        created_at = proplists:get_value(<<"created_at">>, ColumnMap),
        updated_at = proplists:get_value(<<"updated_at">>, ColumnMap)
    }.

-spec build_update_clause(map(), integer(), list(), list()) -> {list(), list(), integer()}.
build_update_clause(UpdateData, ParamCount, SetClause, Values) ->
    Fields = maps:to_list(UpdateData),
    build_update_clause_loop(Fields, ParamCount, SetClause, Values).

-spec build_update_clause_loop(list(), integer(), list(), list()) -> {list(), list(), integer()}.
build_update_clause_loop([], ParamCount, SetClause, Values) ->
    {lists:reverse(SetClause), lists:reverse(Values), ParamCount - 1};
build_update_clause_loop([{Field, Value} | Rest], ParamCount, SetClause, Values) ->
    FieldStr = atom_to_list(Field),
    Clause = FieldStr ++ " = $" ++ integer_to_list(ParamCount),
    build_update_clause_loop(Rest, ParamCount + 1, [Clause | SetClause], [Value | Values]). 