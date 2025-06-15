-module(deployer_config).

-export([parse_config/1, validate_config/1]).

%% Type definitions
-type config_file() :: string().
-type config() :: [{atom(), term()}].
-type repo_tuple() :: {string(), string()}.
-type json_map() :: map().
-type config_result() :: {ok, config()} | {error, string()}.

%% Function specifications
-spec parse_config(config_file()) -> config_result().
-spec validate_config(config()) -> config_result().

%% ===================================================================
%% API functions
%% ===================================================================

parse_config(ConfigFile) ->
    case filename:extension(ConfigFile) of
        ".json" ->
            parse_json_config(ConfigFile);
        _ ->
            parse_erlang_config(ConfigFile)
    end.

validate_config(Config) ->
    RequiredKeys = [github_api_key, repos, slack_channel_id, slack_bot_token],
    case check_required_keys(Config, RequiredKeys) of
        ok ->
            % Add default values
            ConfigWithDefaults = [
                {master_branch, proplists:get_value(master_branch, Config, "main")},
                {staging_branch, proplists:get_value(staging_branch, Config, "staging")},
                {concurrent, proplists:get_value(concurrent, Config, true)}
                | Config
            ],
            {ok, ConfigWithDefaults};
        {missing, Key} ->
            {error, io_lib:format("Missing required key: ~p", [Key])}
    end.

%% ===================================================================
%% Internal functions
%% ===================================================================

-spec parse_json_config(config_file()) -> config_result().
-spec parse_erlang_config(config_file()) -> config_result().
-spec check_required_keys(config(), [atom()]) -> ok | {missing, atom()}.
-spec convert_repos_from_jsx([json_map()]) -> [repo_tuple()].
-spec get_binary_value(binary(), json_map()) -> string().
-spec get_binary_value(binary(), json_map(), binary()) -> string().
-spec get_boolean_value(binary(), json_map(), boolean()) -> boolean().

parse_json_config(ConfigFile) ->
    case file:read_file(ConfigFile) of
        {ok, JsonBinary} ->
            try jsx:decode(JsonBinary, [{return_maps, true}]) of
                JsonConfig when is_map(JsonConfig) ->
                    Config = [
                        {github_api_key, get_binary_value(<<"github_api_key">>, JsonConfig)},
                        {slack_bot_token, get_binary_value(<<"slack_bot_token">>, JsonConfig)},
                        {slack_channel_id, get_binary_value(<<"slack_channel_id">>, JsonConfig)},
                        {thread_title, get_binary_value(<<"thread_title">>, JsonConfig, <<"🚀 Concurrent Deployment">>)},
                        {master_branch, get_binary_value(<<"master_branch">>, JsonConfig, <<"main">>)},
                        {staging_branch, get_binary_value(<<"staging_branch">>, JsonConfig, <<"staging">>)},
                        {concurrent, get_boolean_value(<<"concurrent">>, JsonConfig, true)},
                        {repos, convert_repos_from_jsx(maps:get(<<"repos">>, JsonConfig, []))}
                    ],
                    validate_config(Config);
                _Other ->
                    {error, "Invalid JSON structure"}
            catch
                Class:Error ->
                    {error, io_lib:format("Invalid JSON format: ~p:~p", [Class, Error])}
            end;
        {error, Reason} ->
            {error, io_lib:format("Failed to read JSON config file: ~p", [Reason])}
    end.

parse_erlang_config(ConfigFile) ->
    case file:consult(ConfigFile) of
        {ok, Config} ->
            validate_config(Config);
        {error, Reason} ->
            {error, io_lib:format("Failed to read config file: ~p", [Reason])}
    end.

check_required_keys(_Config, []) -> ok;
check_required_keys(Config, [Key | Rest]) ->
    case proplists:is_defined(Key, Config) of
        true -> check_required_keys(Config, Rest);
        false -> {missing, Key}
    end.

convert_repos_from_jsx(ReposList) when is_list(ReposList) ->
    lists:map(fun(RepoMap) when is_map(RepoMap) ->
        Repo = get_binary_value(<<"repo">>, RepoMap),
        Workflow = get_binary_value(<<"workflow">>, RepoMap),
        {Repo, Workflow}
    end, ReposList);
convert_repos_from_jsx(_) ->
    [].

get_binary_value(Key, Map) ->
    case maps:get(Key, Map) of
        Value when is_binary(Value) -> binary_to_list(Value);
        _ -> error({invalid_type, Key})
    end.

get_binary_value(Key, Map, Default) ->
    case maps:get(Key, Map, Default) of
        Value when is_binary(Value) -> binary_to_list(Value);
        Default when is_binary(Default) -> binary_to_list(Default);
        _ -> error({invalid_type, Key})
    end.

get_boolean_value(Key, Map, Default) ->
    case maps:get(Key, Map, Default) of
        Value when is_boolean(Value) -> Value;
        Default when is_boolean(Default) -> Default;
        _ -> error({invalid_type, Key})
    end.
