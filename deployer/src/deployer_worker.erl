-module(deployer_worker).
-behaviour(gen_statem).

%% API
-export([start_deployment/3]).

%% gen_statem callbacks
-export([init/1, callback_mode/0, handle_event/4, terminate/3, code_change/4]).

%% State functions
-export([idle/3, creating_pr/3, waiting_for_pr_merge/3, creating_branch/3,
         concurrent_post_release/3, completed/3]).

%% Records for worker state
-record(worker_state, {
    repo_name = "",
    workflow_name = "",
    coordinator_pid = undefined,
    config = [],
    github_key = "",
    slack_channel_id = "",
    slack_bot_token = "",
    slack_thread_id = "",
    master_branch = "main",
    staging_branch = "staging",
    release_branch = "",
    workflow_run_id = undefined,
    pr_number = undefined,
    final_pr_number = undefined,
    retries = 0,
    max_retries = 3,
    % Concurrent post-release operations
    pending_ops = 0,
    release_result = undefined,
    workflow_result = undefined,
    final_pr_result = undefined,
    % PR merge retry fields
    pr_merge_attempts = 0,
    max_merge_attempts = 72,  % 72 * 5 minutes = 6 hours max wait
    last_slack_notification = undefined
}).

%% ===================================================================
%% API functions
%% ===================================================================

start_deployment({RepoName, WorkflowName}, Config, CoordinatorPid) ->
    gen_statem:start(?MODULE, {RepoName, WorkflowName, Config, CoordinatorPid}, []).

%% ===================================================================
%% gen_statem callbacks
%% ===================================================================

callback_mode() -> state_functions.

init({RepoName, WorkflowName, Config, CoordinatorPid}) ->
    GithubKey = proplists:get_value(github_api_key, Config),
    SlackChannelId = proplists:get_value(slack_channel_id, Config),
    SlackBotToken = proplists:get_value(slack_bot_token, Config),
    SlackThreadId = proplists:get_value(slack_thread_id, Config, ""),
    MasterBranch = proplists:get_value(master_branch, Config, "main"),
    StagingBranch = proplists:get_value(staging_branch, Config, "staging"),

    State = #worker_state{
        repo_name = RepoName,
        workflow_name = WorkflowName,
        coordinator_pid = CoordinatorPid,
        config = Config,
        github_key = GithubKey,
        slack_channel_id = SlackChannelId,
        slack_bot_token = SlackBotToken,
        slack_thread_id = SlackThreadId,
        master_branch = MasterBranch,
        staging_branch = StagingBranch
    },

    % Notify coordinator that we're starting
    CoordinatorPid ! {repo_started, RepoName, self()},

    {ok, idle, State, [{next_event, internal, start_deployment}]}.

%% ===================================================================
%% State functions
%% ===================================================================

idle(internal, start_deployment, State) ->
    #worker_state{repo_name = RepoName,
                 coordinator_pid = CoordinatorPid} = State,

    io:format("[~s] Worker starting deployment...~n", [RepoName]),
    CoordinatorPid ! {repo_progress, RepoName, self(), creating_pr, "Starting PR creation"},

    {next_state, creating_pr, State, [{next_event, internal, create_pr}]}.

creating_pr(internal, create_pr, State) ->
    #worker_state{repo_name = RepoName,
                 github_key = GithubKey,
                 master_branch = MasterBranch,
                 staging_branch = StagingBranch,
                 coordinator_pid = CoordinatorPid} = State,

    io:format("[~s] Checking branches: ~s vs ~s~n", [RepoName, StagingBranch, MasterBranch]),
    case deployer_github:create_pull_request(RepoName, GithubKey, StagingBranch, MasterBranch,
                            io_lib:format("Automated PR: ~s -> ~s", [StagingBranch, MasterBranch]),
                            "Auto-generated PR for deployment automation") of
        {ok, no_diff_needed} ->
            io:format("[~s] Staging and master already in sync, skipping entire deployment~n", [RepoName]),
            CoordinatorPid ! {repo_skipped, RepoName, self(), "branches already synchronized"},
            {next_state, completed, State};
        {ok, PR} ->
            PRNumber = maps:get(<<"number">>, PR, 1),
            io:format("[~s] Created PR #~p, attempting to merge~n", [RepoName, PRNumber]),
            CoordinatorPid ! {repo_progress, RepoName, self(), merging_pr, io_lib:format("Created PR #~p", [PRNumber])},

            case deployer_github:merge_pull_request(RepoName, GithubKey, PRNumber) of
                {ok, _} ->
                    NewState = State#worker_state{pr_number = PRNumber},
                    {next_state, creating_branch, NewState, [{next_event, internal, create_release_branch}]};
                {error, {405, _}} ->
                    % PR cannot be merged (likely needs manual review/conflict resolution)
                    io:format("[~s] PR #~p cannot be merged automatically, waiting for manual intervention~n", [RepoName, PRNumber]),
                    InitialSlackMsg = io_lib:format("⚠️ [~s] PR #~p requires manual intervention before merge. Will retry every 5 minutes.", [RepoName, PRNumber]),
                    deployer_slack:send_message(State#worker_state.slack_channel_id, State#worker_state.slack_bot_token,
                                     State#worker_state.slack_thread_id, InitialSlackMsg),

                    NewState = State#worker_state{
                        pr_number = PRNumber,
                        pr_merge_attempts = 1,
                        last_slack_notification = erlang:system_time(second)
                    },
                    {next_state, waiting_for_pr_merge, NewState, [{state_timeout, 300000, retry_merge}]};  % 5 minutes
                {error, Reason} ->
                    % Other merge errors (permissions, etc.)
                    io:format("[~s] PR #~p merge failed with error: ~p~n", [RepoName, PRNumber, Reason]),
                    CoordinatorPid ! {repo_failed, RepoName, self(), {merge_failed, Reason}},
                    {next_state, completed, State}
            end;
        {error, Reason} ->
            io:format("[~s] Failed to create PR: ~p~n", [RepoName, Reason]),
            CoordinatorPid ! {repo_failed, RepoName, self(), {pr_creation_failed, Reason}},
            {next_state, completed, State}
    end.

waiting_for_pr_merge(state_timeout, retry_merge, State) ->
    #worker_state{repo_name = RepoName,
                 github_key = GithubKey,
                 pr_number = PRNumber,
                 pr_merge_attempts = Attempts,
                 max_merge_attempts = MaxAttempts,
                 coordinator_pid = CoordinatorPid,
                 slack_channel_id = SlackChannelId,
                 slack_bot_token = SlackBotToken,
                 slack_thread_id = SlackThreadId} = State,

    io:format("[~s] Retry attempt #~p: checking if PR #~p can be merged~n", [RepoName, Attempts, PRNumber]),

    case deployer_github:merge_pull_request(RepoName, GithubKey, PRNumber) of
        {ok, _} ->
            % PR finally merged successfully
            io:format("[~s] PR #~p merged successfully after ~p attempts~n", [RepoName, PRNumber, Attempts]),
            SuccessMsg = io_lib:format("✅ [~s] PR #~p merged successfully after ~p attempts (~p minutes)",
                                     [RepoName, PRNumber, Attempts, Attempts * 5]),
            deployer_slack:send_message(SlackChannelId, SlackBotToken, SlackThreadId, SuccessMsg),
            CoordinatorPid ! {repo_progress, RepoName, self(), creating_branch, "PR merged, proceeding to release branch creation"},

            {next_state, creating_branch, State, [{next_event, internal, create_release_branch}]};

        {error, {405, _}} when Attempts < MaxAttempts ->
            % Still cannot merge, continue waiting
            NewAttempts = Attempts + 1,
            TimeWaiting = NewAttempts * 5,  % minutes

            % Send periodic Slack notification
            RetryMsg = io_lib:format("⏳ [~s] PR #~p still waiting for manual intervention (attempt #~p, ~p minutes elapsed)",
                                   [RepoName, PRNumber, NewAttempts, TimeWaiting]),
            deployer_slack:send_message(SlackChannelId, SlackBotToken, SlackThreadId, RetryMsg),

            NewState = State#worker_state{
                pr_merge_attempts = NewAttempts,
                last_slack_notification = erlang:system_time(second)
            },

            {next_state, waiting_for_pr_merge, NewState, [{state_timeout, 300000, retry_merge}]};  % 5 minutes

        {error, {405, _}} ->
            % Maximum attempts reached
            TimeoutMsg = io_lib:format("❌ [~s] PR #~p merge timeout after ~p attempts (~p hours). Manual intervention required.",
                                     [RepoName, PRNumber, MaxAttempts, MaxAttempts * 5 / 60]),
            deployer_slack:send_message(SlackChannelId, SlackBotToken, SlackThreadId, TimeoutMsg),
            CoordinatorPid ! {repo_failed, RepoName, self(), {pr_merge_timeout, PRNumber, MaxAttempts}},
            {next_state, completed, State};

        {error, Reason} ->
            % Other error occurred during retry
            ErrorMsg = io_lib:format("❌ [~s] PR #~p merge failed during retry: ~p", [RepoName, PRNumber, Reason]),
            deployer_slack:send_message(SlackChannelId, SlackBotToken, SlackThreadId, ErrorMsg),
            CoordinatorPid ! {repo_failed, RepoName, self(), {merge_retry_failed, Reason}},
            {next_state, completed, State}
    end.

creating_branch(internal, create_release_branch, State) ->
    #worker_state{repo_name = RepoName,
                 github_key = GithubKey,
                 master_branch = MasterBranch,
                 coordinator_pid = CoordinatorPid} = State,

    ReleaseBranch = "release-" ++ deployer_github:format_date(),
    io:format("[~s] Creating release branch: ~s~n", [RepoName, ReleaseBranch]),
    CoordinatorPid ! {repo_progress, RepoName, self(), creating_branch, io_lib:format("Creating branch ~s", [ReleaseBranch])},

    case deployer_github:create_branch(RepoName, GithubKey, ReleaseBranch, MasterBranch) of
        {ok, _} ->
            NewState = State#worker_state{release_branch = ReleaseBranch},
            {next_state, concurrent_post_release, NewState, [{next_event, internal, start_concurrent_ops}]};
        {error, Reason} ->
            CoordinatorPid ! {repo_failed, RepoName, self(), {branch_creation_failed, Reason}},
            {next_state, completed, State}
    end.

%% New concurrent post-release state
concurrent_post_release(internal, start_concurrent_ops, State) ->
    #worker_state{repo_name = RepoName, coordinator_pid = CoordinatorPid} = State,

    % Spawn three concurrent operations
    Self = self(),

    % Operation 1: Create Release
    spawn_link(fun() ->
        Result = concurrent_create_release(State),
        Self ! {release_result, Result}
    end),

    % Operation 2: Trigger Workflow
    spawn_link(fun() ->
        Result = concurrent_trigger_workflow(State),
        Self ! {workflow_result, Result}
    end),

    % Operation 3: Create Final PR
    spawn_link(fun() ->
        Result = concurrent_create_final_pr(State),
        Self ! {final_pr_result, Result}
    end),

    % Track pending operations
    NewState = State#worker_state{
        pending_ops = 3,
        release_result = undefined,
        workflow_result = undefined,
        final_pr_result = undefined
    },

    CoordinatorPid ! {repo_progress, RepoName, Self, concurrent_post_release,
                     "Starting 3 concurrent operations: release creation, workflow trigger, final PR"},

    {next_state, concurrent_post_release, NewState};

concurrent_post_release(info, {release_result, Result}, State) ->
    handle_concurrent_result(release_result, Result, State);

concurrent_post_release(info, {workflow_result, Result}, State) ->
    handle_concurrent_result(workflow_result, Result, State);

concurrent_post_release(info, {final_pr_result, Result}, State) ->
    handle_concurrent_result(final_pr_result, Result, State).

completed(_EventType, _EventContent, State) ->
    {next_state, completed, State}.

handle_event(_EventType, _EventContent, StateName, State) ->
    {next_state, StateName, State}.

terminate(_Reason, _StateName, _State) ->
    ok.

code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

%% ===================================================================
%% Internal functions
%% ===================================================================

%% Concurrent operation result handling
handle_concurrent_result(OpType, Result, State) ->
    #worker_state{pending_ops = PendingOps,
                 repo_name = RepoName,
                 coordinator_pid = CoordinatorPid} = State,

    % Update result and decrement pending operations
    NewState = update_operation_result(OpType, Result, State),
    UpdatedState = NewState#worker_state{pending_ops = PendingOps - 1},

    % Report progress
    OpName = operation_name(OpType),
    CoordinatorPid ! {repo_progress, RepoName, self(), concurrent_post_release,
                     io_lib:format("~s completed, ~p operations remaining",
                                  [OpName, PendingOps - 1])},

    case UpdatedState#worker_state.pending_ops of
        0 ->
            % All operations completed, check results and transition
            handle_all_operations_complete(UpdatedState);
        _ ->
            % Still waiting for more operations
            {next_state, concurrent_post_release, UpdatedState}
    end.

update_operation_result(release_result, Result, State) ->
    State#worker_state{release_result = Result};
update_operation_result(workflow_result, Result, State) ->
    State#worker_state{workflow_result = Result};
update_operation_result(final_pr_result, Result, State) ->
    State#worker_state{final_pr_result = Result}.

operation_name(release_result) -> "Release creation";
operation_name(workflow_result) -> "Workflow trigger";
operation_name(final_pr_result) -> "Final PR creation".

handle_all_operations_complete(State) ->
    #worker_state{release_result = ReleaseResult,
                 workflow_result = WorkflowResult,
                 final_pr_result = _FinalPRResult,
                 repo_name = RepoName,
                 coordinator_pid = CoordinatorPid} = State,

    % Check if any critical operations failed
    CriticalFailures = check_critical_failures([ReleaseResult, WorkflowResult]),

    case CriticalFailures of
        [] ->
            % All critical operations succeeded
            io:format("[~s] All concurrent operations completed successfully~n", [RepoName]),
            CoordinatorPid ! {repo_completed, RepoName, self(), success},
            {next_state, completed, State};
        Failures ->
            % Some critical operations failed
            io:format("[~s] Some critical operations failed: ~p~n", [RepoName, Failures]),
            CoordinatorPid ! {repo_failed, RepoName, self(), {concurrent_ops_failed, Failures}},
            {next_state, completed, State}
    end.

check_critical_failures(Results) ->
    lists:foldl(fun
        ({error, Reason}, Acc) -> [Reason | Acc];
        (_, Acc) -> Acc
    end, [], Results).

%% Concurrent operation functions
concurrent_create_release(State) ->
    #worker_state{repo_name = RepoName,
                 github_key = GithubKey,
                 release_branch = ReleaseBranch} = State,

    case deployer_github:get_next_version(RepoName, GithubKey) of
        {ok, NewVersion} ->
            case deployer_github:create_release(RepoName, GithubKey, NewVersion, ReleaseBranch) of
                {ok, Release} ->
                    {ok, {release_created, NewVersion, Release}};
                {error, Reason} ->
                    {error, {release_creation_failed, Reason}}
            end;
        {error, VersionError} ->
            {error, {version_error, VersionError}}
    end.

concurrent_trigger_workflow(State) ->
    #worker_state{repo_name = RepoName,
                 workflow_name = WorkflowName,
                 github_key = GithubKey,
                 master_branch = MasterBranch} = State,

    case deployer_github:trigger_workflow(RepoName, GithubKey, WorkflowName, MasterBranch) of
        {ok, _} ->
            case deployer_github:get_latest_workflow_run(RepoName, GithubKey, WorkflowName) of
                {ok, WorkflowRunId} ->
                    {ok, {workflow_triggered, WorkflowRunId}};
                {error, Reason} ->
                    {ok, {workflow_triggered_no_id, Reason}}
            end;
        {error, Reason} ->
            {error, {workflow_trigger_failed, Reason}}
    end.

concurrent_create_final_pr(State) ->
    #worker_state{repo_name = RepoName,
                 github_key = GithubKey,
                 release_branch = ReleaseBranch,
                 master_branch = MasterBranch} = State,

    case deployer_github:create_pull_request(RepoName, GithubKey, ReleaseBranch, MasterBranch,
                            io_lib:format("Release: ~s -> ~s", [ReleaseBranch, MasterBranch]),
                            io_lib:format("Release ~s to ~s branch", [ReleaseBranch, MasterBranch])) of
        {ok, no_diff_needed} ->
            {ok, {final_pr_skipped, "release branch sync with master"}};
        {ok, FinalPR} ->
            FinalPRNumber = maps:get(<<"number">>, FinalPR, 2),
            {ok, {final_pr_created, FinalPRNumber, FinalPR}};
        {error, Reason} ->
            {error, {final_pr_failed, Reason}}
    end.
