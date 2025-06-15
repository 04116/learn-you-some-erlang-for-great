-module(deployer_slack).

-export([
    send_initial_message/3,
    send_message/4
]).

%% ===================================================================
%% API functions
%% ===================================================================

send_initial_message(ChannelId, BotToken, Title) ->
    Url = "https://slack.com/api/chat.postMessage",
    Headers = [{"Authorization", "Bearer " ++ BotToken},
               {"Content-Type", "application/json"}],

    PayloadBin = jsx:encode(#{
        <<"channel">> => list_to_binary(ChannelId),
        <<"text">> => list_to_binary(Title)
    }),
    Payload = case PayloadBin of
        P when is_binary(P) -> binary_to_list(P);
        _ -> "{}"
    end,

    case httpc:request(post, {Url, Headers, "application/json", Payload}, [], []) of
        {ok, {{_, 200, _}, _, ResponseBody}} when is_list(ResponseBody) ->
            try jsx:decode(list_to_binary(ResponseBody), [{return_maps, true}]) of
                Response when is_map(Response) ->
                    case maps:get(<<"ok">>, Response, false) of
                        true ->
                            case maps:get(<<"ts">>, Response, undefined) of
                                Ts when is_binary(Ts) -> {ok, binary_to_list(Ts)};
                                _ -> {error, "Missing timestamp"}
                            end;
                        false ->
                            Error = maps:get(<<"error">>, Response, <<"unknown">>),
                            ErrorStr = case Error of
                                E when is_binary(E) -> binary_to_list(E);
                                _ -> "unknown error"
                            end,
                            {error, ErrorStr}
                    end;
                _ ->
                    {error, "Invalid response format"}
            catch
                _:_ -> {error, "JSON decode error"}
            end;
        {ok, {{_, StatusCode, _}, _, ResponseBody}} ->
            {error, {http_error, StatusCode, ResponseBody}};
        {error, Reason} ->
            {error, Reason}
    end.

send_message(ChannelId, BotToken, ThreadId, Message) ->
    Url = "https://slack.com/api/chat.postMessage",
    Headers = [{"Authorization", "Bearer " ++ BotToken},
               {"Content-Type", "application/json"}],

    % Ensure Message is properly formatted as a binary, handling unicode
    MessageBin = case Message of
        Msg when is_binary(Msg) -> Msg;
        Msg when is_list(Msg) ->
            try
                unicode:characters_to_binary(lists:flatten(Msg))
            catch
                _:_ -> list_to_binary(lists:flatten(io_lib:format("~s", [Msg])))
            end;
        Msg ->
            unicode:characters_to_binary(lists:flatten(io_lib:format("~p", [Msg])))
    end,

    Payload = jsx:encode(#{
        <<"channel">> => list_to_binary(ChannelId),
        <<"text">> => MessageBin,
        <<"thread_ts">> => list_to_binary(ThreadId)
    }),

    httpc:request(post, {Url, Headers, "application/json", Payload}, [], []).
