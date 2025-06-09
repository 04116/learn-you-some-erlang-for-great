-module(realworld_article).

-include("realworld_article.hrl").
-include("realworld_user.hrl").

-export([
    create/5,
    find_by_slug/1,
    find_by_id/1,
    update/2,
    delete/1,
    list_articles/3,
    get_feed/2,
    favorite/2,
    unfavorite/2,
    is_favorited/2,
    get_favorites_count/1,
    article_to_json/3,
    generate_slug/1,
    get_tags_for_article/1,
    create_tags/1
]).

%% Create a new article
-spec create(binary(), binary(), binary(), binary(), [binary()]) -> {ok, #article{}} | {error, any()}.
create(Title, Description, Body, AuthorId, TagList) ->
    Slug = generate_slug(Title),
    ArticleId = uuid:uuid_to_string(uuid:get_v4()),
    
    realworld_db:transaction(fun(Worker) ->
        % Insert article
        Sql = "INSERT INTO articles (id, slug, title, description, body, author_id) VALUES ($1, $2, $3, $4, $5, $6) RETURNING *",
        Params = [ArticleId, Slug, Title, Description, Body, AuthorId],
        
        case gen_server:call(Worker, {query, Sql, Params}) of
            {ok, {1, Columns, [Row]}} ->
                Article = row_to_article(Columns, Row),
                
                % Create tags if provided
                case TagList of
                    [] -> ok;
                    _ -> create_tags_for_article(Worker, ArticleId, TagList)
                end,
                
                Article;
            {error, Reason} ->
                throw({error, Reason})
        end
    end).

%% Find article by slug
-spec find_by_slug(binary()) -> {ok, #article{}} | {error, not_found}.
find_by_slug(Slug) ->
    Sql = "SELECT * FROM articles WHERE slug = $1",
    case realworld_db:query(Sql, [Slug]) of
        {ok, {1, Columns, [Row]}} ->
            {ok, row_to_article(Columns, Row)};
        {ok, {0, _, _}} ->
            {error, not_found};
        {error, Reason} ->
            {error, Reason}
    end.

%% Find article by ID
-spec find_by_id(binary()) -> {ok, #article{}} | {error, not_found}.
find_by_id(ArticleId) ->
    Sql = "SELECT * FROM articles WHERE id = $1",
    case realworld_db:query(Sql, [ArticleId]) of
        {ok, {1, Columns, [Row]}} ->
            {ok, row_to_article(Columns, Row)};
        {ok, {0, _, _}} ->
            {error, not_found};
        {error, Reason} ->
            {error, Reason}
    end.

%% Update article
-spec update(binary(), map()) -> {ok, #article{}} | {error, any()}.
update(ArticleId, UpdateData) ->
    {SetClause, Values, ParamCount} = build_update_clause(UpdateData, 1, [], []),
    
    case SetClause of
        [] ->
            find_by_id(ArticleId);
        _ ->
            Sql = "UPDATE articles SET " ++ string:join(SetClause, ", ") ++ 
                  " WHERE id = $" ++ integer_to_list(ParamCount + 1) ++ " RETURNING *",
            Params = Values ++ [ArticleId],
            
            case realworld_db:query(Sql, Params) of
                {ok, {1, Columns, [Row]}} ->
                    {ok, row_to_article(Columns, Row)};
                {error, Reason} ->
                    {error, Reason}
            end
    end.

%% Delete article
-spec delete(binary()) -> ok | {error, any()}.
delete(ArticleId) ->
    Sql = "DELETE FROM articles WHERE id = $1",
    case realworld_db:query(Sql, [ArticleId]) of
        {ok, {1}} ->
            ok;
        {ok, {0}} ->
            {error, not_found};
        {error, Reason} ->
            {error, Reason}
    end.

%% List articles with optional filters
-spec list_articles(map(), integer(), integer()) -> {ok, [#article{}]} | {error, any()}.
list_articles(Filters, Limit, Offset) ->
    {WhereClause, Params} = build_where_clause(Filters),
    
    BaseSql = "SELECT a.* FROM articles a LEFT JOIN users u ON a.author_id = u.id",
    OrderClause = " ORDER BY a.created_at DESC",
    LimitClause = " LIMIT $" ++ integer_to_list(length(Params) + 1) ++ 
                  " OFFSET $" ++ integer_to_list(length(Params) + 2),
    
    Sql = BaseSql ++ WhereClause ++ OrderClause ++ LimitClause,
    QueryParams = Params ++ [Limit, Offset],
    
    case realworld_db:query(Sql, QueryParams) of
        {ok, {_, Columns, Rows}} ->
            Articles = [row_to_article(Columns, Row) || Row <- Rows],
            {ok, Articles};
        {error, Reason} ->
            {error, Reason}
    end.

%% Get user feed
-spec get_feed(binary(), integer()) -> {ok, [#article{}]} | {error, any()}.
get_feed(UserId, Limit) ->
    Sql = "SELECT a.* FROM articles a " ++
          "JOIN follows f ON a.author_id = f.followed_id " ++
          "WHERE f.follower_id = $1 " ++
          "ORDER BY a.created_at DESC LIMIT $2",
    
    case realworld_db:query(Sql, [UserId, Limit]) of
        {ok, {_, Columns, Rows}} ->
            Articles = [row_to_article(Columns, Row) || Row <- Rows],
            {ok, Articles};
        {error, Reason} ->
            {error, Reason}
    end.

%% Favorite an article
-spec favorite(binary(), binary()) -> ok | {error, any()}.
favorite(UserId, ArticleId) ->
    Sql = "INSERT INTO favorites (user_id, article_id) VALUES ($1, $2) ON CONFLICT DO NOTHING",
    case realworld_db:query(Sql, [UserId, ArticleId]) of
        {ok, _} ->
            ok;
        {error, Reason} ->
            {error, Reason}
    end.

%% Unfavorite an article
-spec unfavorite(binary(), binary()) -> ok | {error, any()}.
unfavorite(UserId, ArticleId) ->
    Sql = "DELETE FROM favorites WHERE user_id = $1 AND article_id = $2",
    case realworld_db:query(Sql, [UserId, ArticleId]) of
        {ok, _} ->
            ok;
        {error, Reason} ->
            {error, Reason}
    end.

%% Check if article is favorited by user
-spec is_favorited(binary(), binary()) -> boolean().
is_favorited(UserId, ArticleId) ->
    Sql = "SELECT 1 FROM favorites WHERE user_id = $1 AND article_id = $2",
    case realworld_db:query(Sql, [UserId, ArticleId]) of
        {ok, {1, _, _}} ->
            true;
        _ ->
            false
    end.

%% Get favorites count for article
-spec get_favorites_count(binary()) -> integer().
get_favorites_count(ArticleId) ->
    Sql = "SELECT COUNT(*) FROM favorites WHERE article_id = $1",
    case realworld_db:query(Sql, [ArticleId]) of
        {ok, {1, _, [{Count}]}} ->
            Count;
        _ ->
            0
    end.

%% Get tags for article
-spec get_tags_for_article(binary()) -> [binary()].
get_tags_for_article(ArticleId) ->
    Sql = "SELECT t.name FROM tags t " ++
          "JOIN article_tags at ON t.id = at.tag_id " ++
          "WHERE at.article_id = $1",
    
    case realworld_db:query(Sql, [ArticleId]) of
        {ok, {_, _, Rows}} ->
            [TagName || {TagName} <- Rows];
        _ ->
            []
    end.

%% Create tags and associate with article
-spec create_tags([binary()]) -> ok.
create_tags(TagList) ->
    % This is a simplified version - in real implementation would handle article association
    lists:foreach(fun(TagName) ->
        TagId = uuid:uuid_to_string(uuid:get_v4()),
        Sql = "INSERT INTO tags (id, name) VALUES ($1, $2) ON CONFLICT (name) DO NOTHING",
        realworld_db:query(Sql, [TagId, TagName])
    end, TagList),
    ok.

%% Generate slug from title
-spec generate_slug(binary()) -> binary().
generate_slug(Title) ->
    % Convert to lowercase, replace spaces with hyphens, remove special chars
    LowerTitle = string:lowercase(binary_to_list(Title)),
    CleanTitle = re:replace(LowerTitle, "[^a-z0-9\\s]", "", [global, {return, list}]),
    SlugBase = re:replace(CleanTitle, "\\s+", "-", [global, {return, list}]),
    
    % Add timestamp to ensure uniqueness
    Timestamp = integer_to_list(erlang:system_time(second)),
    Slug = SlugBase ++ "-" ++ Timestamp,
    
    list_to_binary(Slug).

%% Convert article record to JSON
-spec article_to_json(#article{}, boolean(), integer()) -> map().
article_to_json(Article, Favorited, FavoritesCount) ->
    TagList = get_tags_for_article(Article#article.id),
    
    #{
        <<"slug">> => Article#article.slug,
        <<"title">> => Article#article.title,
        <<"description">> => Article#article.description,
        <<"body">> => Article#article.body,
        <<"tagList">> => TagList,
        <<"createdAt">> => format_datetime(Article#article.created_at),
        <<"updatedAt">> => format_datetime(Article#article.updated_at),
        <<"favorited">> => Favorited,
        <<"favoritesCount">> => FavoritesCount,
        <<"author">> => get_author_profile(Article#article.author_id)
    }.

%% Internal helper functions

-spec row_to_article(list(), tuple()) -> #article{}.
row_to_article(Columns, Row) ->
    RowList = tuple_to_list(Row),
    ColumnMap = lists:zip(Columns, RowList),
    
    #article{
        id = proplists:get_value(<<"id">>, ColumnMap),
        slug = proplists:get_value(<<"slug">>, ColumnMap),
        title = proplists:get_value(<<"title">>, ColumnMap),
        description = proplists:get_value(<<"description">>, ColumnMap),
        body = proplists:get_value(<<"body">>, ColumnMap),
        author_id = proplists:get_value(<<"author_id">>, ColumnMap),
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

-spec build_where_clause(map()) -> {string(), list()}.
build_where_clause(Filters) ->
    Conditions = [],
    Params = [],
    
    {WhereClause, FinalParams} = case maps:get(author, Filters, undefined) of
        undefined -> {Conditions, Params};
        Author ->
            Cond = " WHERE u.username = $" ++ integer_to_list(length(Params) + 1),
            {[Cond | Conditions], [Author | Params]}
    end,
    
    FinalWhere = case WhereClause of
        [] -> "";
        _ -> string:join(WhereClause, " AND ")
    end,
    
    {FinalWhere, FinalParams}.

-spec create_tags_for_article(pid(), binary(), [binary()]) -> ok.
create_tags_for_article(Worker, ArticleId, TagList) ->
    lists:foreach(fun(TagName) ->
        % Create tag if it doesn't exist
        TagId = uuid:uuid_to_string(uuid:get_v4()),
        InsertTagSql = "INSERT INTO tags (id, name) VALUES ($1, $2) ON CONFLICT (name) DO NOTHING",
        gen_server:call(Worker, {query, InsertTagSql, [TagId, TagName]}),
        
        % Get tag ID
        GetTagSql = "SELECT id FROM tags WHERE name = $1",
        {ok, {1, _, [{ActualTagId}]}} = gen_server:call(Worker, {query, GetTagSql, [TagName]}),
        
        % Associate tag with article
        AssocSql = "INSERT INTO article_tags (article_id, tag_id) VALUES ($1, $2) ON CONFLICT DO NOTHING",
        gen_server:call(Worker, {query, AssocSql, [ArticleId, ActualTagId]})
    end, TagList),
    ok.

-spec format_datetime(any()) -> binary().
format_datetime(DateTime) ->
    % Simple datetime formatting - in real app would use proper ISO8601
    iolist_to_binary(io_lib:format("~p", [DateTime])).

-spec get_author_profile(binary()) -> map().
get_author_profile(AuthorId) ->
    case realworld_user:find_by_id(AuthorId) of
        {ok, User} ->
            #{
                <<"username">> => User#user.username,
                <<"bio">> => case User#user.bio of null -> <<"">>;  Bio -> Bio end,
                <<"image">> => User#user.image,
                <<"following">> => false  % TODO: Implement following check
            };
        _ ->
            #{
                <<"username">> => <<"">>,
                <<"bio">> => <<"">>,
                <<"image">> => null,
                <<"following">> => false
            }
    end. 