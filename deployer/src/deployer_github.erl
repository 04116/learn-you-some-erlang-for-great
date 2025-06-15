-module(deployer_github).

-export([
    create_pull_request/6,
    merge_pull_request/3,
    create_branch/4,
    get_next_version/2,
    create_release_with_notes/5,
    trigger_workflow/4,
    get_workflow_runs_for_commit/4,
    get_branch_sha/3,
    branch_exists/3,
    compare_branches/4,
    generate_release_notes/4
]).

%% Type definitions
-type repo_path() :: string().
-type github_token() :: string().
-type branch_name() :: string().
-type pr_title() :: string().
-type pr_body() :: string().
-type pr_number() :: non_neg_integer().
-type commit_sha() :: string().
-type tag_name() :: string().
-type workflow_name() :: string().
-type version_string() :: string().
-type release_notes() :: string().
-type github_response() :: map().
-type http_error() :: {non_neg_integer(), string()}.
-type github_result(T) :: {ok, T} | {error, term()}.

%% Function specifications
-spec create_pull_request(repo_path(), github_token(), branch_name(), branch_name(), pr_title(), pr_body()) -> 
    github_result(github_response() | no_diff_needed).
-spec merge_pull_request(repo_path(), github_token(), pr_number()) -> github_result(github_response()).
-spec create_branch(repo_path(), github_token(), branch_name(), branch_name()) -> github_result(github_response()).
-spec get_next_version(repo_path(), github_token()) -> github_result(version_string()).
-spec create_release_with_notes(repo_path(), github_token(), tag_name(), branch_name(), release_notes()) -> 
    github_result(github_response()).
-spec trigger_workflow(repo_path(), github_token(), workflow_name(), branch_name()) -> github_result(triggered).
-spec get_workflow_runs_for_commit(repo_path(), github_token(), workflow_name(), commit_sha()) -> 
    github_result([github_response()]).
-spec get_branch_sha(repo_path(), github_token(), branch_name()) -> github_result(commit_sha()).
-spec branch_exists(repo_path(), github_token(), branch_name()) -> github_result(boolean()).
-spec compare_branches(repo_path(), github_token(), branch_name(), branch_name()) -> 
    github_result({synchronized | staging_ahead | master_ahead | diverged, map()}).
-spec generate_release_notes(repo_path(), github_token(), branch_name(), branch_name()) -> 
    github_result(release_notes()).

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


create_release_with_notes(RepoPath, Token, TagName, TargetCommitish, ReleaseNotes) ->
    Url = io_lib:format("https://api.github.com/repos/~s/releases", [RepoPath]),
    Headers = [{"Authorization", "token " ++ Token},
               {"Content-Type", "application/json"},
               {"User-Agent", "deployer/1.0"}],

    Payload = jsx:encode(#{
        <<"tag_name">> => list_to_binary(TagName),
        <<"target_commitish">> => list_to_binary(TargetCommitish),
        <<"name">> => list_to_binary(TagName),
        <<"body">> => list_to_binary(ReleaseNotes),
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


get_workflow_runs_for_commit(RepoPath, Token, WorkflowName, CommitSha) ->
    Url = io_lib:format("https://api.github.com/repos/~s/actions/workflows/~s/runs?head_sha=~s",
                       [RepoPath, WorkflowName, CommitSha]),
    Headers = [{"Authorization", "token " ++ Token},
               {"User-Agent", "deployer/1.0"}],

    case httpc:request(get, {lists:flatten(Url), Headers}, [], []) of
        {ok, {{_, 200, _}, _, ResponseBody}} ->
            Response = jsx:decode(list_to_binary(ResponseBody), [return_maps]),
            WorkflowRuns = maps:get(<<"workflow_runs">>, Response, []),
            {ok, WorkflowRuns};
        {ok, {{_, StatusCode, _}, _, ResponseBody}} ->
            {error, {http_error, StatusCode, ResponseBody}};
        {error, Reason} ->
            {error, Reason}
    end.


%% Check if a branch exists
branch_exists(RepoPath, Token, BranchName) ->
    Url = io_lib:format("https://api.github.com/repos/~s/git/refs/heads/~s", [RepoPath, BranchName]),
    Headers = [{"Authorization", "token " ++ Token},
               {"User-Agent", "deployer/1.0"}],

    case httpc:request(get, {lists:flatten(Url), Headers}, [], []) of
        {ok, {{_, 200, _}, _, _}} ->
            {ok, true};
        {ok, {{_, 404, _}, _, _}} ->
            {ok, false};
        {ok, {{_, StatusCode, _}, _, ResponseBody}} ->
            {error, {http_error, StatusCode, ResponseBody}};
        {error, Reason} ->
            {error, Reason}
    end.

%% Compare two branches and return status
compare_branches(RepoPath, Token, BaseBranch, HeadBranch) ->
    Url = io_lib:format("https://api.github.com/repos/~s/compare/~s...~s",
                       [RepoPath, BaseBranch, HeadBranch]),
    Headers = [{"Authorization", "token " ++ Token},
               {"User-Agent", "deployer/1.0"}],

    case httpc:request(get, {lists:flatten(Url), Headers}, [], []) of
        {ok, {{_, 200, _}, _, ResponseBody}} ->
            Response = jsx:decode(list_to_binary(ResponseBody), [return_maps]),
            AheadBy = maps:get(<<"ahead_by">>, Response, 0),
            BehindBy = maps:get(<<"behind_by">>, Response, 0),

            Status = if
                AheadBy > 0 andalso BehindBy =:= 0 -> staging_ahead;
                AheadBy =:= 0 andalso BehindBy > 0 -> master_ahead;
                AheadBy =:= 0 andalso BehindBy =:= 0 -> synchronized;
                true -> diverged
            end,
            {ok, Status, #{ahead_by => AheadBy, behind_by => BehindBy}};
        {ok, {{_, StatusCode, _}, _, ResponseBody}} ->
            {error, {http_error, StatusCode, ResponseBody}};
        {error, Reason} ->
            {error, Reason}
    end.

%% Generate release notes from commits
generate_release_notes(RepoPath, Token, FromRef, ToRef) ->
    Url = io_lib:format("https://api.github.com/repos/~s/compare/~s...~s",
                       [RepoPath, FromRef, ToRef]),
    Headers = [{"Authorization", "token " ++ Token},
               {"User-Agent", "deployer/1.0"}],

    case httpc:request(get, {lists:flatten(Url), Headers}, [], []) of
        {ok, {{_, 200, _}, _, ResponseBody}} ->
            Response = jsx:decode(list_to_binary(ResponseBody), [return_maps]),
            Commits = maps:get(<<"commits">>, Response, []),

            Notes = lists:foldl(fun(Commit, Acc) ->
                CommitData = maps:get(<<"commit">>, Commit, #{}),
                Message = maps:get(<<"message">>, CommitData, <<"Unknown commit">>),
                Sha = maps:get(<<"sha">>, Commit, <<"unknown">>),
                ShortSha = binary:part(Sha, 0, min(7, byte_size(Sha))),
                CommitLine = io_lib:format("- ~s (~s)~n", [Message, ShortSha]),
                [CommitLine | Acc]
            end, [], Commits),

            ReleaseNotes = io_lib:format("## Release Notes~n~nChanges in this release:~n~s",
                                       [lists:reverse(Notes)]),
            {ok, lists:flatten(ReleaseNotes)};
        {ok, {{_, StatusCode, _}, _, ResponseBody}} ->
            {error, {http_error, StatusCode, ResponseBody}};
        {error, Reason} ->
            {error, Reason}
    end.

%% ===================================================================
%% Internal functions
%% ===================================================================

-spec check_branches_diff(repo_path(), github_token(), branch_name(), branch_name()) -> github_result(non_neg_integer()).
-spec get_existing_pr(repo_path(), github_token(), branch_name(), branch_name()) -> 
    github_result(github_response()) | {error, not_found}.
-spec create_new_pr(repo_path(), github_token(), branch_name(), branch_name(), pr_title(), pr_body()) -> 
    github_result(github_response()).
-spec create_branch_from_sha(repo_path(), github_token(), branch_name(), commit_sha()) -> 
    github_result(github_response()).
-spec get_latest_release(repo_path(), github_token()) -> github_result(version_string()) | {error, not_found}.
-spec increment_version(version_string()) -> version_string().

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
