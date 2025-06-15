-module(deployer_github).

-export([
    create_pull_request/6,
    merge_pull_request/3,
    create_branch/4,
    get_next_version/2,
    create_release/4,
    trigger_workflow/4,
    get_latest_workflow_run/3,
    format_date/0
]).

%% ===================================================================
%% API functions
%% ===================================================================

create_pull_request(RepoPath, Token, Head, Base, Title, Body) ->
    % First check if branches are different
    case check_branches_diff(RepoPath, Token, Head, Base) of
        {ok, 0} ->
            {ok, no_diff_needed};
        {ok, _} ->
            % Check if PR already exists
            case get_existing_pr(RepoPath, Token, Head, Base) of
                {ok, ExistingPR} ->
                    {ok, ExistingPR};
                {error, not_found} ->
                    create_new_pr(RepoPath, Token, Head, Base, Title, Body);
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

merge_pull_request(RepoPath, Token, PRNumber) ->
    Url = io_lib:format("https://api.github.com/repos/~s/pulls/~p/merge", [RepoPath, PRNumber]),
    Headers = [{"Authorization", "token " ++ Token},
               {"Content-Type", "application/json"},
               {"User-Agent", "deployer/1.0"}],

    Payload = jsx:encode(#{
        <<"commit_title">> => <<"Automated merge by deployer">>,
        <<"merge_method">> => <<"merge">>
    }),

    case httpc:request(put, {lists:flatten(Url), Headers, "application/json", Payload}, [], []) of
        {ok, {{_, 200, _}, _, ResponseBody}} ->
            Response = jsx:decode(list_to_binary(ResponseBody), [return_maps]),
            {ok, Response};
        {ok, {{_, StatusCode, _}, _, ResponseBody}} ->
            {error, {StatusCode, ResponseBody}};
        {error, Reason} ->
            {error, Reason}
    end.

create_branch(RepoPath, Token, BranchName, BaseBranch) ->
    % First get the SHA of the base branch
    case get_branch_sha(RepoPath, Token, BaseBranch) of
        {ok, BaseSha} ->
            create_branch_from_sha(RepoPath, Token, BranchName, BaseSha);
        {error, Reason} ->
            {error, Reason}
    end.

get_next_version(RepoPath, Token) ->
    case get_latest_release(RepoPath, Token) of
        {ok, LatestVersion} ->
            {ok, increment_version(LatestVersion)};
        {error, not_found} ->
            {ok, "v1.0.0"};
        {error, Reason} ->
            {error, Reason}
    end.

create_release(RepoPath, Token, TagName, TargetCommitish) ->
    Url = io_lib:format("https://api.github.com/repos/~s/releases", [RepoPath]),
    Headers = [{"Authorization", "token " ++ Token},
               {"Content-Type", "application/json"},
               {"User-Agent", "deployer/1.0"}],

    Payload = jsx:encode(#{
        <<"tag_name">> => list_to_binary(TagName),
        <<"target_commitish">> => list_to_binary(TargetCommitish),
        <<"name">> => list_to_binary(TagName),
        <<"body">> => list_to_binary("Automated release created by deployer"),
        <<"draft">> => false,
        <<"prerelease">> => false
    }),

    case httpc:request(post, {lists:flatten(Url), Headers, "application/json", Payload}, [], []) of
        {ok, {{_, 201, _}, _, ResponseBody}} ->
            Response = jsx:decode(list_to_binary(ResponseBody), [return_maps]),
            {ok, Response};
        {ok, {{_, StatusCode, _}, _, ResponseBody}} ->
            {error, {http_error, StatusCode, ResponseBody}};
        {error, Reason} ->
            {error, Reason}
    end.

trigger_workflow(RepoPath, Token, WorkflowName, BranchRef) ->
    Url = io_lib:format("https://api.github.com/repos/~s/actions/workflows/~s/dispatches", [RepoPath, WorkflowName]),
    Headers = [{"Authorization", "token " ++ Token},
               {"Content-Type", "application/json"},
               {"User-Agent", "deployer/1.0"}],

    Payload = jsx:encode(#{
        <<"ref">> => list_to_binary(BranchRef)
    }),

    case httpc:request(post, {lists:flatten(Url), Headers, "application/json", Payload}, [], []) of
        {ok, {{_, 204, _}, _, _}} ->
            {ok, triggered};
        {ok, {{_, StatusCode, _}, _, ResponseBody}} ->
            {error, {http_error, StatusCode, ResponseBody}};
        {error, Reason} ->
            {error, Reason}
    end.

get_latest_workflow_run(RepoPath, Token, WorkflowName) ->
    Url = io_lib:format("https://api.github.com/repos/~s/actions/workflows/~s/runs", [RepoPath, WorkflowName]),
    Headers = [{"Authorization", "token " ++ Token},
               {"User-Agent", "deployer/1.0"}],

    case httpc:request(get, {lists:flatten(Url), Headers}, [], []) of
        {ok, {{_, 200, _}, _, ResponseBody}} ->
            Response = jsx:decode(list_to_binary(ResponseBody), [return_maps]),
            WorkflowRuns = maps:get(<<"workflow_runs">>, Response, []),
            case WorkflowRuns of
                [LatestRun | _] ->
                    RunId = maps:get(<<"id">>, LatestRun),
                    {ok, RunId};
                [] ->
                    {error, no_runs}
            end;
        {ok, {{_, StatusCode, _}, _, ResponseBody}} ->
            {error, {http_error, StatusCode, ResponseBody}};
        {error, Reason} ->
            {error, Reason}
    end.

format_date() ->
    {{Year, Month, Day}, _} = calendar:universal_time(),
    io_lib:format("~4..0w~2..0w~2..0w", [Year, Month, Day]).

%% ===================================================================
%% Internal functions
%% ===================================================================

check_branches_diff(RepoPath, Token, Head, Base) ->
    Url = io_lib:format("https://api.github.com/repos/~s/compare/~s...~s", [RepoPath, Base, Head]),
    Headers = [{"Authorization", "token " ++ Token},
               {"User-Agent", "deployer/1.0"}],

    case httpc:request(get, {lists:flatten(Url), Headers}, [], []) of
        {ok, {{_, 200, _}, _, ResponseBody}} ->
            Response = jsx:decode(list_to_binary(ResponseBody), [return_maps]),
            AheadBy = maps:get(<<"ahead_by">>, Response, 0),
            {ok, AheadBy};
        {ok, {{_, StatusCode, _}, _, ResponseBody}} ->
            {error, {http_error, StatusCode, ResponseBody}};
        {error, Reason} ->
            {error, Reason}
    end.

get_existing_pr(RepoPath, Token, Head, Base) ->
    Url = io_lib:format("https://api.github.com/repos/~s/pulls?head=~s&base=~s&state=open", [RepoPath, Head, Base]),
    Headers = [{"Authorization", "token " ++ Token},
               {"User-Agent", "deployer/1.0"}],

    case httpc:request(get, {lists:flatten(Url), Headers}, [], []) of
        {ok, {{_, 200, _}, _, ResponseBody}} ->
            Response = jsx:decode(list_to_binary(ResponseBody), [return_maps]),
            case Response of
                [FirstPR | _] -> {ok, FirstPR};
                [] -> {error, not_found}
            end;
        {ok, {{_, StatusCode, _}, _, ResponseBody}} ->
            {error, {http_error, StatusCode, ResponseBody}};
        {error, Reason} ->
            {error, Reason}
    end.

create_new_pr(RepoPath, Token, Head, Base, Title, Body) ->
    Url = io_lib:format("https://api.github.com/repos/~s/pulls", [RepoPath]),
    Headers = [{"Authorization", "token " ++ Token},
               {"Content-Type", "application/json"},
               {"User-Agent", "deployer/1.0"}],

    Payload = jsx:encode(#{
        <<"title">> => list_to_binary(Title),
        <<"body">> => list_to_binary(Body),
        <<"head">> => list_to_binary(Head),
        <<"base">> => list_to_binary(Base)
    }),

    case httpc:request(post, {lists:flatten(Url), Headers, "application/json", Payload}, [], []) of
        {ok, {{_, 201, _}, _, ResponseBody}} ->
            Response = jsx:decode(list_to_binary(ResponseBody), [return_maps]),
            {ok, Response};
        {ok, {{_, StatusCode, _}, _, ResponseBody}} ->
            {error, {http_error, StatusCode, ResponseBody}};
        {error, Reason} ->
            {error, Reason}
    end.

get_branch_sha(RepoPath, Token, BranchName) ->
    Url = io_lib:format("https://api.github.com/repos/~s/git/refs/heads/~s", [RepoPath, BranchName]),
    Headers = [{"Authorization", "token " ++ Token},
               {"User-Agent", "deployer/1.0"}],

    case httpc:request(get, {lists:flatten(Url), Headers}, [], []) of
        {ok, {{_, 200, _}, _, ResponseBody}} ->
            Response = jsx:decode(list_to_binary(ResponseBody), [return_maps]),
            Object = maps:get(<<"object">>, Response),
            Sha = maps:get(<<"sha">>, Object),
            {ok, binary_to_list(Sha)};
        {ok, {{_, StatusCode, _}, _, ResponseBody}} ->
            {error, {http_error, StatusCode, ResponseBody}};
        {error, Reason} ->
            {error, Reason}
    end.

create_branch_from_sha(RepoPath, Token, BranchName, BaseSha) ->
    Url = io_lib:format("https://api.github.com/repos/~s/git/refs", [RepoPath]),
    Headers = [{"Authorization", "token " ++ Token},
               {"Content-Type", "application/json"},
               {"User-Agent", "deployer/1.0"}],

    Payload = jsx:encode(#{
        <<"ref">> => list_to_binary("refs/heads/" ++ BranchName),
        <<"sha">> => list_to_binary(BaseSha)
    }),

    case httpc:request(post, {lists:flatten(Url), Headers, "application/json", Payload}, [], []) of
        {ok, {{_, 201, _}, _, ResponseBody}} ->
            Response = jsx:decode(list_to_binary(ResponseBody), [return_maps]),
            {ok, Response};
        {ok, {{_, StatusCode, _}, _, ResponseBody}} ->
            {error, {http_error, StatusCode, ResponseBody}};
        {error, Reason} ->
            {error, Reason}
    end.

get_latest_release(RepoPath, Token) ->
    Url = io_lib:format("https://api.github.com/repos/~s/releases", [RepoPath]),
    Headers = [{"Authorization", "token " ++ Token},
               {"User-Agent", "deployer/1.0"}],

    case httpc:request(get, {lists:flatten(Url), Headers}, [], []) of
        {ok, {{_, 200, _}, _, ResponseBody}} ->
            Response = jsx:decode(list_to_binary(ResponseBody), [return_maps]),
            case Response of
                [LatestRelease | _] ->
                    TagName = maps:get(<<"tag_name">>, LatestRelease),
                    {ok, binary_to_list(TagName)};
                [] ->
                    {error, not_found}
            end;
        {ok, {{_, StatusCode, _}, _, ResponseBody}} ->
            {error, {http_error, StatusCode, ResponseBody}};
        {error, Reason} ->
            {error, Reason}
    end.

increment_version("v" ++ VersionStr) ->
    case string:tokens(VersionStr, ".") of
        [MajorStr, MinorStr, PatchStr] ->
            try
                Major = list_to_integer(MajorStr),
                Minor = list_to_integer(MinorStr),
                Patch = list_to_integer(PatchStr),
                "v" ++ integer_to_list(Major) ++ "." ++ integer_to_list(Minor) ++ "." ++ integer_to_list(Patch + 1)
            catch
                _:_ -> "v1.0.0"
            end;
        _ -> "v1.0.0"
    end;
increment_version(_) -> "v1.0.0".
