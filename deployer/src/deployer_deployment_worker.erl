-module(deployer_deployment_worker).
-behaviour(gen_statem).


%% API
-export([start_deployment/3]).

%% Function specifications
-spec start_deployment(repo_workflow(), config(), coordinator_pid()) -> {ok, pid()} | {error, error_reason()}.

%% Types
-type repo_workflow() :: {string(), string()}.
-type config() :: [{atom(), term()}].
-type coordinator_pid() :: pid().
-type github_key() :: string().
-type workflow_name() :: string().
-type repo_name() :: string().
-type branch_name() :: string().
-type commit_sha() :: string().
-type task_id() :: string().
-type workflow_run_id() :: non_neg_integer().
-type workflow_status() :: binary().
-type error_reason() :: term().
-type state_name() :: atom().
-type event_type() :: atom().
-type event_content() :: term().
-type gen_statem_result() :: {next_state, state_name(), term()} |
                             {next_state, state_name(), term(), [term()]}.

%% gen_statem callbacks
-export([init/1, callback_mode/0, handle_event/4, terminate/3, code_change/4]).

%% State functions
-export([idle/3, checking_idempotency/3, checking_master_vs_staging/3,
         creating_pr_master_to_staging/3, waiting_for_master_staging_merge/3,
         checking_staging_vs_master/3, creating_release_branch/3,
         creating_pr_release_to_master/3, triggering_workflow/3,
         monitoring_workflow/3, creating_release/3, completed/3]).

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
    release_commit_sha = "",
    pr_number = undefined,
    pr_merge_attempts = 0,
    max_merge_attempts = 72,  % 72 * 5 minutes = 6 hours max wait
    % Workflow monitoring
    workflow_poll_interval = 30000,  % 30 seconds
    workflow_max_wait_time = 3600000, % 1 hour
    workflow_start_time = undefined,
    workflow_run_id = undefined,
    last_workflow_status = undefined
}).

%% gen_statem callback specs
-spec init({repo_name(), workflow_name(), config(), coordinator_pid()}) -> {ok, idle, #worker_state{}, [term()]}.
-spec callback_mode() -> state_functions.
-spec handle_event(event_type(), event_content(), state_name(), #worker_state{}) -> gen_statem_result().
-spec terminate(term(), state_name(), #worker_state{}) -> ok.
-spec code_change(term(), state_name(), #worker_state{}, term()) -> {ok, state_name(), #worker_state{}}.

%% State function specs
-spec idle(event_type(), event_content(), #worker_state{}) -> gen_statem_result().
-spec checking_idempotency(event_type(), event_content(), #worker_state{}) -> gen_statem_result().
-spec checking_master_vs_staging(event_type(), event_content(), #worker_state{}) -> gen_statem_result().
-spec creating_pr_master_to_staging(event_type(), event_content(), #worker_state{}) -> gen_statem_result().
-spec waiting_for_master_staging_merge(event_type(), event_content(), #worker_state{}) -> gen_statem_result().
-spec checking_staging_vs_master(event_type(), event_content(), #worker_state{}) -> gen_statem_result().
-spec creating_release_branch(event_type(), event_content(), #worker_state{}) -> gen_statem_result().
-spec creating_pr_release_to_master(event_type(), event_content(), #worker_state{}) -> gen_statem_result().
-spec triggering_workflow(event_type(), event_content(), #worker_state{}) -> gen_statem_result().
-spec monitoring_workflow(event_type(), event_content(), #worker_state{}) -> gen_statem_result().
-spec creating_release(event_type(), event_content(), #worker_state{}) -> gen_statem_result().
-spec completed(event_type(), event_content(), #worker_state{}) -> gen_statem_result().

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
    CoordinatorPid ! {repo_progress, RepoName, self(), checking_idempotency, "Checking if task already completed today"},

    {next_state, checking_idempotency, State, [{next_event, internal, check_idempotency}]}.

checking_idempotency(internal, check_idempotency, State) ->
    #worker_state{repo_name = RepoName,
                 github_key = GithubKey,
                 workflow_name = WorkflowName,
                 coordinator_pid = CoordinatorPid} = State,

    % Check if today's task is already completed (idempotent check including workflow)
    case is_task_completed(RepoName, GithubKey, WorkflowName) of
        {ok, true, TaskId, ReleaseBranch} ->
            io:format("[~s] Task ~s already completed today (release branch ~s exists), skipping~n",
                     [RepoName, TaskId, ReleaseBranch]),
            CoordinatorPid ! {repo_skipped, RepoName, self(), "task already completed today"},
            {next_state, completed, State};
        {ok, false, TaskId, _} ->
            io:format("[~s] Task ~s not completed yet, checking master vs staging~n", [RepoName, TaskId]),
            CoordinatorPid ! {repo_progress, RepoName, self(), checking_master_vs_staging, "Checking diff master vs staging"},
            {next_state, checking_master_vs_staging, State, [{next_event, internal, check_master_vs_staging}]};
        {error, Reason} ->
            io:format("[~s] Failed to check task completion: ~p~n", [RepoName, Reason]),
            CoordinatorPid ! {repo_failed, RepoName, self(), {idempotency_check_failed, Reason}},
            {next_state, completed, State}
    end.

checking_master_vs_staging(internal, check_master_vs_staging, State) ->
    #worker_state{repo_name = RepoName,
                 github_key = GithubKey,
                 coordinator_pid = CoordinatorPid,
                 config = Config} = State,

    MasterBranch = proplists:get_value(master_branch, Config, "main"),
    StagingBranch = proplists:get_value(staging_branch, Config, "staging"),

    io:format("[~s] Checking diff between master (~s) and staging (~s)~n", [RepoName, MasterBranch, StagingBranch]),

    % Check if master has changes that need to merge to staging
    case deployer_github:compare_branches(RepoName, GithubKey, MasterBranch, StagingBranch) of
        {ok, master_ahead, _} ->
            % Master has new commits, need to merge master -> staging first
            io:format("[~s] Master ahead of staging, creating PR master -> staging~n", [RepoName]),
            CoordinatorPid ! {repo_progress, RepoName, self(), creating_pr_master_to_staging, "Master ahead, creating PR master -> staging"},
            UpdatedState = State#worker_state{master_branch = MasterBranch, staging_branch = StagingBranch},
            {next_state, creating_pr_master_to_staging, UpdatedState, [{next_event, internal, create_pr_master_to_staging}]};
        {ok, staging_ahead, _, _} ->
            % Staging is ahead, proceed to check staging vs master for release
            io:format("[~s] Staging ahead of master, proceeding to check staging vs master~n", [RepoName]),
            CoordinatorPid ! {repo_progress, RepoName, self(), checking_staging_vs_master, "Staging ahead, checking staging vs master"},
            UpdatedState = State#worker_state{master_branch = MasterBranch, staging_branch = StagingBranch},
            {next_state, checking_staging_vs_master, UpdatedState, [{next_event, internal, check_staging_vs_master}]};
        {ok, synchronized, _, _} ->
            % Branches are in sync, no deployment needed
            io:format("[~s] Master and staging synchronized, no deployment needed~n", [RepoName]),
            CoordinatorPid ! {repo_skipped, RepoName, self(), "master and staging already synchronized"},
            {next_state, completed, State};
        {error, Reason} ->
            io:format("[~s] Failed to check master vs staging: ~p~n", [RepoName, Reason]),
            CoordinatorPid ! {repo_failed, RepoName, self(), {master_staging_check_failed, Reason}},
            {next_state, completed, State}
    end.

creating_pr_master_to_staging(internal, create_pr_master_to_staging, State) ->
    #worker_state{repo_name = RepoName,
                 github_key = GithubKey,
                 master_branch = MasterBranch,
                 staging_branch = StagingBranch,
                 coordinator_pid = CoordinatorPid} = State,

    io:format("[~s] Creating PR: ~s -> ~s~n", [RepoName, MasterBranch, StagingBranch]),
    case deployer_github:create_pull_request(RepoName, GithubKey, MasterBranch, StagingBranch,
                            io_lib:format("Sync: ~s -> ~s", [MasterBranch, StagingBranch]),
                            "Auto-sync master changes to staging before release") of
        {ok, no_diff_needed} ->
            io:format("[~s] No diff between master and staging, checking staging vs master~n", [RepoName]),
            CoordinatorPid ! {repo_progress, RepoName, self(), checking_staging_vs_master, "Branches synced, checking staging vs master"},
            {next_state, checking_staging_vs_master, State, [{next_event, internal, check_staging_vs_master}]};
        {ok, PR} ->
            PRNumber = maps:get(<<"number">>, PR, 1),
            io:format("[~s] Created sync PR #~p, attempting to merge~n", [RepoName, PRNumber]),

            case deployer_github:merge_pull_request(RepoName, GithubKey, PRNumber) of
                {ok, _} ->
                    io:format("[~s] Sync PR #~p merged, checking staging vs master~n", [RepoName, PRNumber]),
                    CoordinatorPid ! {repo_progress, RepoName, self(), checking_staging_vs_master, "Master synced to staging, checking staging vs master"},
                    {next_state, checking_staging_vs_master, State, [{next_event, internal, check_staging_vs_master}]};
                {error, {405, _}} ->
                    io:format("[~s] Sync PR #~p cannot be merged automatically, waiting~n", [RepoName, PRNumber]),
                    NewState = State#worker_state{pr_number = PRNumber, pr_merge_attempts = 1},
                    {next_state, waiting_for_master_staging_merge, NewState, [{state_timeout, 300000, retry_master_staging_merge}]};
                {error, Reason} ->
                    io:format("[~s] Sync PR #~p merge failed: ~p~n", [RepoName, PRNumber, Reason]),
                    CoordinatorPid ! {repo_failed, RepoName, self(), {master_staging_merge_failed, Reason}},
                    {next_state, completed, State}
            end;
        {error, Reason} ->
            io:format("[~s] Failed to create sync PR: ~p~n", [RepoName, Reason]),
            CoordinatorPid ! {repo_failed, RepoName, self(), {master_staging_pr_creation_failed, Reason}},
            {next_state, completed, State}
    end.

waiting_for_master_staging_merge(state_timeout, retry_master_staging_merge, State) ->
    #worker_state{repo_name = RepoName,
                 github_key = GithubKey,
                 pr_number = PRNumber,
                 coordinator_pid = CoordinatorPid} = State,

    case deployer_github:merge_pull_request(RepoName, GithubKey, PRNumber) of
        {ok, _} ->
            io:format("[~s] Master->staging PR #~p merged, checking staging vs master~n", [RepoName, PRNumber]),
            CoordinatorPid ! {repo_progress, RepoName, self(), checking_staging_vs_master, "Master synced to staging, checking staging vs master"},
            {next_state, checking_staging_vs_master, State, [{next_event, internal, check_staging_vs_master}]};
        {error, {405, _}} ->
            NewAttempts = State#worker_state.pr_merge_attempts + 1,
            if NewAttempts < 12 -> % Wait up to 1 hour
                NewState = State#worker_state{pr_merge_attempts = NewAttempts},
                {next_state, waiting_for_master_staging_merge, NewState, [{state_timeout, 300000, retry_master_staging_merge}]};
            true ->
                io:format("[~s] Master->staging PR #~p timeout, manual intervention required~n", [RepoName, PRNumber]),
                CoordinatorPid ! {repo_failed, RepoName, self(), {master_staging_merge_timeout, PRNumber}},
                {next_state, completed, State}
            end;
        {error, Reason} ->
            io:format("[~s] Master->staging PR #~p retry failed: ~p~n", [RepoName, PRNumber, Reason]),
            CoordinatorPid ! {repo_failed, RepoName, self(), {master_staging_merge_retry_failed, Reason}},
            {next_state, completed, State}
    end.

checking_staging_vs_master(internal, check_staging_vs_master, State) ->
    #worker_state{repo_name = RepoName,
                 github_key = GithubKey,
                 master_branch = MasterBranch,
                 staging_branch = StagingBranch,
                 coordinator_pid = CoordinatorPid} = State,

    io:format("[~s] Checking diff between staging (~s) and master (~s)~n", [RepoName, StagingBranch, MasterBranch]),

    % Check if staging has changes that need to be released
    case deployer_github:compare_branches(RepoName, GithubKey, StagingBranch, MasterBranch) of
        {ok, staging_ahead, _} ->
            % Staging has new commits, proceed with release
            io:format("[~s] Staging ahead of master, creating release branch~n", [RepoName]),
            CoordinatorPid ! {repo_progress, RepoName, self(), creating_release_branch, "Staging ahead, creating release branch"},
            {next_state, creating_release_branch, State, [{next_event, internal, create_release_branch}]};
        {ok, synchronized, _, _} ->
            % Branches are in sync, no release needed
            io:format("[~s] Staging and master synchronized, no release needed~n", [RepoName]),
            CoordinatorPid ! {repo_skipped, RepoName, self(), "staging and master already synchronized"},
            {next_state, completed, State};
        {ok, master_ahead, _} ->
            % This shouldn't happen if we just synced, but handle it
            io:format("[~s] Master ahead of staging after sync - unexpected state~n", [RepoName]),
            CoordinatorPid ! {repo_failed, RepoName, self(), {unexpected_master_ahead_after_sync}},
            {next_state, completed, State};
        {error, Reason} ->
            io:format("[~s] Failed to check staging vs master: ~p~n", [RepoName, Reason]),
            CoordinatorPid ! {repo_failed, RepoName, self(), {staging_master_check_failed, Reason}},
            {next_state, completed, State}
    end.

creating_release_branch(internal, create_release_branch, State) ->
    #worker_state{repo_name = RepoName,
                 github_key = GithubKey,
                 staging_branch = StagingBranch,
                 coordinator_pid = CoordinatorPid} = State,

    % Use UTC+7 date for consistency with task identification
    ReleaseBranch = "release-" ++ get_utc_plus_7_date(),
    io:format("[~s] Creating release branch: ~s from ~s~n", [RepoName, ReleaseBranch, StagingBranch]),
    CoordinatorPid ! {repo_progress, RepoName, self(), creating_release_branch, io_lib:format("Creating branch ~s", [ReleaseBranch])},

    case deployer_github:create_branch(RepoName, GithubKey, ReleaseBranch, StagingBranch) of
        {ok, _} ->
            NewState = State#worker_state{release_branch = ReleaseBranch},
            {next_state, creating_pr_release_to_master, NewState, [{next_event, internal, create_pr_release_to_master}]};
        {error, Reason} ->
            CoordinatorPid ! {repo_failed, RepoName, self(), {release_branch_creation_failed, Reason}},
            {next_state, completed, State}
    end.

creating_pr_release_to_master(internal, create_pr_release_to_master, State) ->
    #worker_state{repo_name = RepoName,
                 github_key = GithubKey,
                 release_branch = ReleaseBranch,
                 master_branch = MasterBranch,
                 coordinator_pid = CoordinatorPid} = State,

    io:format("[~s] Creating PR (NOT merging): ~s -> ~s~n", [RepoName, ReleaseBranch, MasterBranch]),
    CoordinatorPid ! {repo_progress, RepoName, self(), creating_pr_release_to_master, "Creating PR release -> master (manual merge)"},

    case deployer_github:create_pull_request(RepoName, GithubKey, ReleaseBranch, MasterBranch,
                            io_lib:format("Release: ~s -> ~s", [ReleaseBranch, MasterBranch]),
                            io_lib:format("Release ~s to ~s branch - REQUIRES MANUAL MERGE", [ReleaseBranch, MasterBranch])) of
        {ok, no_diff_needed} ->
            io:format("[~s] No diff between release and master, proceeding to workflow trigger~n", [RepoName]),
            CoordinatorPid ! {repo_progress, RepoName, self(), triggering_workflow, "No diff, triggering workflow"},
            {next_state, triggering_workflow, State, [{next_event, internal, trigger_workflow}]};
        {ok, _PR} ->
            io:format("[~s] Created release->master PR (manual merge required), triggering workflow~n", [RepoName]),
            CoordinatorPid ! {repo_progress, RepoName, self(), triggering_workflow, "PR created (manual merge), triggering workflow"},
            {next_state, triggering_workflow, State, [{next_event, internal, trigger_workflow}]};
        {error, Reason} ->
            io:format("[~s] Failed to create release->master PR: ~p~n", [RepoName, Reason]),
            CoordinatorPid ! {repo_failed, RepoName, self(), {release_pr_creation_failed, Reason}},
            {next_state, completed, State}
    end.

triggering_workflow(internal, trigger_workflow, State) ->
    #worker_state{repo_name = RepoName,
                 github_key = GithubKey,
                 workflow_name = WorkflowName,
                 release_branch = ReleaseBranch,
                 coordinator_pid = CoordinatorPid} = State,

    io:format("[~s] Triggering workflow ~s on release branch ~s~n", [RepoName, WorkflowName, ReleaseBranch]),
    CoordinatorPid ! {repo_progress, RepoName, self(), triggering_workflow, "Triggering workflow on release branch"},

    % Get the HEAD commit SHA of the release branch
    case deployer_github:get_branch_sha(RepoName, GithubKey, ReleaseBranch) of
        {ok, CommitSha} ->
            % Trigger workflow on release branch
            case deployer_github:trigger_workflow(RepoName, GithubKey, WorkflowName, ReleaseBranch) of
                {ok, _} ->
                    NewState = State#worker_state{
                        release_commit_sha = CommitSha,
                        workflow_start_time = erlang:system_time(second)
                    },
                    % Start monitoring after a short delay
                    {next_state, monitoring_workflow, NewState, [{state_timeout, 10000, start_monitoring}]};
                {error, Reason} ->
                    io:format("[~s] Failed to trigger workflow: ~p~n", [RepoName, Reason]),
                    CoordinatorPid ! {repo_failed, RepoName, self(), {workflow_trigger_failed, Reason}},
                    {next_state, completed, State}
            end;
        {error, Reason} ->
            io:format("[~s] Failed to get release branch SHA: ~p~n", [RepoName, Reason]),
            CoordinatorPid ! {repo_failed, RepoName, self(), {branch_sha_failed, Reason}},
            {next_state, completed, State}
    end.

monitoring_workflow(state_timeout, start_monitoring, State) ->
    #worker_state{repo_name = RepoName, coordinator_pid = CoordinatorPid} = State,
    io:format("[~s] Starting workflow monitoring~n", [RepoName]),
    CoordinatorPid ! {repo_progress, RepoName, self(), monitoring_workflow, "Starting workflow monitoring"},
    {next_state, monitoring_workflow, State, [{next_event, internal, check_workflow_status}]};

monitoring_workflow(internal, check_workflow_status, State) ->
    #worker_state{repo_name = RepoName,
                 github_key = GithubKey,
                 workflow_name = WorkflowName,
                 release_commit_sha = CommitSha,
                 workflow_poll_interval = PollInterval,
                 workflow_max_wait_time = MaxWaitTime,
                 workflow_start_time = StartTime,
                 coordinator_pid = CoordinatorPid} = State,

    CurrentTime = erlang:system_time(second),
    ElapsedTime = (CurrentTime - StartTime) * 1000, % Convert to milliseconds

    case ElapsedTime > MaxWaitTime of
        true ->
            io:format("[~s] Workflow monitoring timeout after ~p seconds~n", [RepoName, MaxWaitTime div 1000]),
            CoordinatorPid ! {repo_failed, RepoName, self(), workflow_timeout},
            {next_state, completed, State};
        false ->
            case get_workflow_run_for_commit(RepoName, GithubKey, WorkflowName, CommitSha) of
                {ok, RunId, Status} ->
                    handle_workflow_status(RunId, Status, State);
                {error, not_found} ->
                    % Workflow run not found yet, keep polling
                    io:format("[~s] Workflow run not found yet, retrying in ~p seconds~n",
                             [RepoName, PollInterval div 1000]),
                    {next_state, monitoring_workflow, State, [{state_timeout, PollInterval, check_workflow_status}]};
                {error, Reason} ->
                    io:format("[~s] Failed to check workflow status: ~p~n", [RepoName, Reason]),
                    CoordinatorPid ! {repo_failed, RepoName, self(), {status_check_failed, Reason}},
                    {next_state, completed, State}
            end
    end;

monitoring_workflow(state_timeout, check_workflow_status, State) ->
    {next_state, monitoring_workflow, State, [{next_event, internal, check_workflow_status}]}.

creating_release(internal, create_release, State) ->
    #worker_state{repo_name = RepoName,
                 github_key = GithubKey,
                 release_branch = ReleaseBranch,
                 master_branch = MasterBranch,
                 coordinator_pid = CoordinatorPid} = State,

    io:format("[~s] Creating release from branch ~s~n", [RepoName, ReleaseBranch]),
    CoordinatorPid ! {repo_progress, RepoName, self(), creating_release, "Creating release with notes"},

    case deployer_github:get_next_version(RepoName, GithubKey) of
        {ok, NewVersion} ->
            % Generate release notes from master to release branch
            ReleaseNotes = case deployer_github:generate_release_notes(RepoName, GithubKey, MasterBranch, ReleaseBranch) of
                {ok, Notes} -> Notes;
                {error, _} -> "Automated release created by deployer"
            end,

            case deployer_github:create_release_with_notes(RepoName, GithubKey, NewVersion, ReleaseBranch, ReleaseNotes) of
                {ok, _Release} ->
                    io:format("[~s] Release ~s created successfully~n", [RepoName, NewVersion]),
                    CoordinatorPid ! {repo_completed, RepoName, self(), success},
                    {next_state, completed, State};
                {error, Reason} ->
                    io:format("[~s] Release creation failed: ~p~n", [RepoName, Reason]),
                    CoordinatorPid ! {repo_failed, RepoName, self(), {release_creation_failed, Reason}},
                    {next_state, completed, State}
            end;
        {error, VersionError} ->
            io:format("[~s] Version error: ~p~n", [RepoName, VersionError]),
            CoordinatorPid ! {repo_failed, RepoName, self(), {version_error, VersionError}},
            {next_state, completed, State}
    end.

completed(_EventType, _EventContent, State) ->
    {next_state, completed, State}.

handle_event(_EventType, _EventContent, StateName, State) ->
    {next_state, StateName, State}.

terminate(_Reason, _StateName, _State) ->
    ok.

code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

%% ===================================================================
%% Internal functions - Task State Logic
%% ===================================================================

-spec get_task_id(repo_name()) -> task_id().
-spec is_task_completed(repo_name(), github_key(), workflow_name()) -> {ok, boolean(), task_id(), branch_name()} | {error, error_reason()}.
-spec is_workflow_completed(repo_name(), github_key(), workflow_name(), branch_name()) -> {ok, boolean()} | {error, error_reason()}.
-spec get_utc_plus_7_date() -> string().

%% Generate a deterministic task ID based on UTC+7 date and repo
get_task_id(RepoName) ->
    Date = get_utc_plus_7_date(),
    lists:flatten(io_lib:format("~s-~s", [RepoName, Date])).

%% Check if a task has been completed today (idempotent check including workflow)
is_task_completed(RepoName, GithubKey, WorkflowName) ->
    TaskId = get_task_id(RepoName),
    % Check for today's release branch existence as marker of completion
    ReleaseBranch = "release-" ++ get_utc_plus_7_date(),
    case deployer_github:branch_exists(RepoName, GithubKey, ReleaseBranch) of
        {ok, true} ->
            % Branch exists, now check if workflow is also completed
            case is_workflow_completed(RepoName, GithubKey, WorkflowName, ReleaseBranch) of
                {ok, true} ->
                    {ok, true, TaskId, ReleaseBranch};
                {ok, false} ->
                    {ok, false, TaskId, ReleaseBranch};
                {error, Reason} ->
                    % If we can't check workflow, assume task not completed
                    io:format("[~s] Warning: Could not verify workflow completion: ~p~n", [RepoName, Reason]),
                    {ok, false, TaskId, ReleaseBranch}
            end;
        {ok, false} ->
            {ok, false, TaskId, ReleaseBranch};
        {error, Reason} ->
            {error, Reason}
    end.

%% Check if workflow has completed successfully for the release branch
is_workflow_completed(RepoName, GithubKey, WorkflowName, ReleaseBranch) ->
    case deployer_github:get_branch_sha(RepoName, GithubKey, ReleaseBranch) of
        {ok, CommitSha} ->
            case deployer_github:get_workflow_runs_for_commit(RepoName, GithubKey, WorkflowName, CommitSha) of
                {ok, []} ->
                    % No workflow runs found for this commit
                    {ok, false};
                {ok, WorkflowRuns} ->
                    % Check if any workflow run completed successfully
                    HasSuccessfulRun = lists:any(fun(Run) ->
                        Status = maps:get(<<"status">>, Run, <<"unknown">>),
                        Conclusion = maps:get(<<"conclusion">>, Run, null),
                        Status =:= <<"completed">> andalso Conclusion =:= <<"success">>
                    end, WorkflowRuns),
                    {ok, HasSuccessfulRun};
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

%% Get current date in UTC+7 timezone (YYYYMMDD format)
get_utc_plus_7_date() ->
    UTCTime = calendar:universal_time(),
    UTC7Time = calendar:gregorian_seconds_to_datetime(
        calendar:datetime_to_gregorian_seconds(UTCTime) + 7 * 3600
    ),
    {{Year, Month, Day}, _} = UTC7Time,
    io_lib:format("~4..0w~2..0w~2..0w", [Year, Month, Day]).


%% ===================================================================
%% Internal functions - Workflow Logic
%% ===================================================================

-spec handle_workflow_status(workflow_run_id(), workflow_status(), #worker_state{}) -> gen_statem_result().
-spec get_workflow_run_for_commit(repo_name(), github_key(), workflow_name(), commit_sha()) -> {ok, workflow_run_id(), workflow_status()} | {error, error_reason()}.

handle_workflow_status(RunId, Status, State) ->
    #worker_state{repo_name = RepoName,
                 workflow_run_id = CurrentRunId,
                 workflow_poll_interval = PollInterval,
                 coordinator_pid = CoordinatorPid} = State,

    % Update run ID if this is the first time we found it
    NewState = case CurrentRunId of
        undefined ->
            io:format("[~s] Found workflow run ID: ~p~n", [RepoName, RunId]),
            State#worker_state{workflow_run_id = RunId};
        _ ->
            State
    end,

    case Status of
        <<"completed">> ->
            io:format("[~s] Workflow completed successfully, creating release~n", [RepoName]),
            CoordinatorPid ! {repo_progress, RepoName, self(), creating_release, "Workflow succeeded, creating release"},
            {next_state, creating_release, NewState#worker_state{last_workflow_status = completed}, [{next_event, internal, create_release}]};
        <<"failure">> ->
            io:format("[~s] Workflow failed~n", [RepoName]),
            CoordinatorPid ! {repo_failed, RepoName, self(), {workflow_failed, RunId}},
            {next_state, completed, NewState#worker_state{last_workflow_status = failed}};
        <<"cancelled">> ->
            io:format("[~s] Workflow was cancelled~n", [RepoName]),
            CoordinatorPid ! {repo_failed, RepoName, self(), {workflow_cancelled, RunId}},
            {next_state, completed, NewState#worker_state{last_workflow_status = cancelled}};
        InProgressStatus when InProgressStatus =:= <<"in_progress">>;
                              InProgressStatus =:= <<"queued">>;
                              InProgressStatus =:= <<"pending">> ->
            io:format("[~s] Workflow ~s, continuing to monitor~n", [RepoName, InProgressStatus]),
            {next_state, monitoring_workflow, NewState#worker_state{last_workflow_status = InProgressStatus},
             [{state_timeout, PollInterval, check_workflow_status}]};
        OtherStatus ->
            io:format("[~s] Unknown workflow status: ~s~n", [RepoName, OtherStatus]),
            {next_state, monitoring_workflow, NewState#worker_state{last_workflow_status = OtherStatus},
             [{state_timeout, PollInterval, check_workflow_status}]}
    end.

get_workflow_run_for_commit(RepoName, GithubKey, WorkflowName, CommitSha) ->
    case deployer_github:get_workflow_runs_for_commit(RepoName, GithubKey, WorkflowName, CommitSha) of
        {ok, []} ->
            {error, not_found};
        {ok, [Run | _]} ->
            RunId = maps:get(<<"id">>, Run),
            Status = maps:get(<<"status">>, Run),
            Conclusion = maps:get(<<"conclusion">>, Run, null),

            % Determine final status
            FinalStatus = case {Status, Conclusion} of
                {<<"completed">>, <<"success">>} -> <<"completed">>;
                {<<"completed">>, <<"failure">>} -> <<"failure">>;
                {<<"completed">>, <<"cancelled">>} -> <<"cancelled">>;
                {RunningStatus, null} -> RunningStatus;
                {_, OtherConclusion} -> OtherConclusion
            end,

            {ok, RunId, FinalStatus};
        {error, Reason} ->
            {error, Reason}
    end.
