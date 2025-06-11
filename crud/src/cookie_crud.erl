-module(cookie_crud).
-export([init/2, terminate/3]).

-define(DB_FILE, "cookies.db").

-type cookie_data() :: #{binary() => term()}.
-type http_status() :: 200..599.

-spec init(cowboy_req:req(), term()) -> {ok, cowboy_req:req(), term()}.
init(Req, _State) ->
    Method = cowboy_req:method(Req),
    Cookie = cowboy_req:binding(cookie, Req),

    Req2 = case Method of
        <<"GET">> ->
            handle_get(Req, Cookie);
        <<"POST">> ->
            handle_post(Req);
        <<"PUT">> ->
            handle_put(Req, Cookie);
        <<"DELETE">> ->
            handle_delete(Req, Cookie);
        _ ->
            reply_error(Req, 405, <<"Method Not Allowed">>)
    end,
    {ok, Req2, _State}.

-spec terminate(term(), cowboy_req:req(), term()) -> ok.
terminate(_Reason, _Req, _State) ->
    ok.

-spec handle_get(cowboy_req:req(), binary() | undefined) -> cowboy_req:req().
handle_get(Req, undefined) ->
    case get_all_cookies() of
        {ok, Cookies} ->
            Json = encode_json(#{cookies => Cookies}),
            reply_json(Req, 200, Json);
        {error, Reason} ->
            reply_error(Req, 500, list_to_binary(io_lib:format("Database error: ~p", [Reason])))
    end;

handle_get(Req, Cookie) ->
    case get_cookie(Cookie) of
        {ok, CookieData} ->
            reply_json(Req, 200, CookieData);
        {error, not_found} ->
            reply_error(Req, 404, <<"Cookie not found">>);
        {error, Reason} ->
            reply_error(Req, 500, list_to_binary(io_lib:format("Database error: ~p", [Reason])))
    end.

-spec handle_post(cowboy_req:req()) -> cowboy_req:req().
handle_post(Req) ->
    {ok, Body, Req2} = cowboy_req:read_body(Req),
    case decode_json(Body) of
        {ok, Data} ->
            case create_cookie(Data) of
                {ok, CreatedData} ->
                    reply_json(Req2, 201, CreatedData);
                {error, duplicate} ->
                    reply_error(Req2, 409, <<"Cookie already exists">>);
                {error, Reason} ->
                    reply_error(Req2, 500, list_to_binary(io_lib:format("Database error: ~p", [Reason])))
            end;
        {error, _} ->
            reply_error(Req2, 400, <<"Invalid JSON">>)
    end.

-spec handle_put(cowboy_req:req(), binary()) -> cowboy_req:req().
handle_put(Req, Cookie) ->
    {ok, Body, Req2} = cowboy_req:read_body(Req),
    case decode_json(Body) of
        {ok, Data} ->
            case update_cookie(Cookie, Data) of
                {ok, UpdatedData} ->
                    reply_json(Req2, 200, UpdatedData);
                {error, not_found} ->
                    reply_error(Req2, 404, <<"Cookie not found">>);
                {error, Reason} ->
                    reply_error(Req2, 500, list_to_binary(io_lib:format("Database error: ~p", [Reason])))
            end;
        {error, _} ->
            reply_error(Req2, 400, <<"Invalid JSON">>)
    end.

-spec handle_delete(cowboy_req:req(), binary()) -> cowboy_req:req().
handle_delete(Req, Cookie) ->
    case delete_cookie(Cookie) of
        {ok, deleted} ->
            reply_json(Req, 200, #{message => <<"Cookie deleted successfully">>});
        {error, not_found} ->
            reply_error(Req, 404, <<"Cookie not found">>);
        {error, Reason} ->
            reply_error(Req, 500, list_to_binary(io_lib:format("Database error: ~p", [Reason])))
    end.

-spec get_all_cookies() -> {ok, [cookie_data()]} | {error, term()}.
get_all_cookies() ->
    Query = "SELECT Data FROM Cookie ORDER BY Created DESC",

    case cookie_db_pool:with_connection(fun(Db) ->
        case esqlite3:q(Db, Query) of
            Rows when is_list(Rows) ->
                Cookies = [decode_json(Data) || [Data] <- Rows],
                [Cookie || {ok, Cookie} <- Cookies];
            {error, Reason} ->
                error(Reason)
        end
    end) of
        {ok, Cookies} -> {ok, Cookies};
        {error, {error, Reason, _}} -> {error, Reason};
        {error, Reason} -> {error, Reason}
    end.

-spec get_cookie(binary()) -> {ok, cookie_data()} | {error, term()}.
get_cookie(Cookie) ->
    Query = "SELECT Data FROM Cookie WHERE Cookie = ?",

    case cookie_db_pool:with_connection(fun(Db) ->
        case esqlite3:q(Db, Query, [Cookie]) of
            [[Data]] ->
                decode_json(Data);
            [] ->
                {error, not_found};
            {error, Reason} ->
                error(Reason)
        end
    end) of
        {ok, {ok, Data}} -> {ok, Data};
        {ok, {error, not_found}} -> {error, not_found};
        {error, {error, Reason, _}} -> {error, Reason};
        {error, Reason} -> {error, Reason}
    end.

-spec create_cookie(cookie_data()) -> {ok, cookie_data()} | {error, term()}.
create_cookie(Data) ->
    DataWithTimestamp = maps:merge(Data, #{
        created => format_timestamp(erlang:system_time(second)),
        last_used => format_timestamp(erlang:system_time(second))
    }),

    Json = encode_json(DataWithTimestamp),
    Query = "INSERT INTO Cookie (Data) VALUES (?)",

    case cookie_db_pool:with_connection(fun(Db) ->
        try esqlite3:q(Db, Query, [Json]) of
            [] ->
                DataWithTimestamp;
            {error, Reason} ->
                error(Reason)
        catch
            error:{sqlite_error, "UNIQUE constraint failed: Cookie.Cookie"} ->
                error(duplicate);
            error:Reason ->
                error(Reason)
        end
    end) of
        {ok, Result} -> {ok, Result};
        {error, {error, duplicate, _}} -> {error, duplicate};
        {error, {error, Reason, _}} -> {error, Reason};
        {error, Reason} -> {error, Reason}
    end.

-spec update_cookie(binary(), cookie_data()) -> {ok, cookie_data()} | {error, term()}.
update_cookie(Cookie, Data) ->
    case get_cookie(Cookie) of
        {ok, ExistingData} ->
            UpdatedData = maps:merge(ExistingData, maps:merge(Data, #{
                last_used => format_timestamp(erlang:system_time(second))
            })),

            Json = encode_json(UpdatedData),
            Query = "UPDATE Cookie SET Data = ? WHERE Cookie = ?",

            case cookie_db_pool:with_connection(fun(Db) ->
                case esqlite3:q(Db, Query, [Json, Cookie]) of
                    [] -> UpdatedData;
                    {error, Reason} -> error(Reason)
                end
            end) of
                {ok, Result} -> {ok, Result};
                {error, {error, Reason, _}} -> {error, Reason};
                {error, Reason} -> {error, Reason}
            end;
        {error, not_found} ->
            {error, not_found}
    end.

-spec delete_cookie(binary()) -> {ok, deleted} | {error, term()}.
delete_cookie(Cookie) ->
    Query = "DELETE FROM Cookie WHERE Cookie = ?",

    case cookie_db_pool:with_connection(fun(Db) ->
        case esqlite3:q(Db, Query, [Cookie]) of
            [] ->
                case esqlite3:changes(Db) of
                    Changes when Changes > 0 -> deleted;
                    0 -> error(not_found)
                end;
            {error, Reason} ->
                error(Reason)
        end
    end) of
        {ok, deleted} -> {ok, deleted};
        {error, {error, not_found, _}} -> {error, not_found};
        {error, {error, Reason, _}} -> {error, Reason};
        {error, Reason} -> {error, Reason}
    end.

-spec encode_json(term()) -> binary().
encode_json(Data) ->
    jsx:encode(Data).

-spec decode_json(binary() | string()) -> {ok, cookie_data()} | {error, term()}.
decode_json(Json) when is_binary(Json) ->
    try
        case jsx:decode(Json, [return_maps]) of
            Map when is_map(Map) -> {ok, Map};
            _ -> {error, invalid_json}
        end
    catch
        _:_ -> {error, invalid_json}
    end;
decode_json(Json) when is_list(Json) ->
    decode_json(iolist_to_binary(Json)).

-spec format_timestamp(integer()) -> binary().
format_timestamp(UnixTime) ->
    list_to_binary(integer_to_list(UnixTime)).

-spec reply_json(cowboy_req:req(), http_status(), term()) -> cowboy_req:req().
reply_json(Req, Status, Data) ->
    Json = encode_json(Data),
    cowboy_req:reply(Status,
        #{<<"content-type">> => <<"application/json">>},
        Json, Req).

-spec reply_error(cowboy_req:req(), http_status(), binary()) -> cowboy_req:req().
reply_error(Req, Status, Message) ->
    Json = encode_json(#{error => Message}),
    cowboy_req:reply(Status,
        #{<<"content-type">> => <<"application/json">>},
        Json, Req).
