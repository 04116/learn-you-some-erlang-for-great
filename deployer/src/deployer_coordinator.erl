-module(deployer_coordinator).
-behaviour(gen_statem).

%% API
-export([start_deployment/1, get_status/1]).

%% Type definitions
-type config() :: [{atom(), term()}].
-type repo_tuple() :: {string(), string()}.
-type worker_status() :: spawned | started | success | failed | skipped.
-type worker_info() :: {pid(), worker_status(), non_neg_integer()}.
-type workers_map() :: #{string() => worker_info()}.
-type status_result() :: #{atom() => term()}.

%% Function specifications
-spec start_deployment(config()) -> {ok, pid()} | {error, term()}.
-spec get_status(pid()) -> status_result().

%% gen_statem callbacks
-export([init/1, callback_mode/0, handle_event/4, terminate/3, code_change/4]).

%% State functions
-export([coordinating/3, completed/3]).

%% Records for coordinator state
-record(coordinator_state, {
    config = [],
    workers = #{},           % #{RepoName => {Pid, Status, StartTime}}
    total_repos = 0,
    completed_repos = 0,
    failed_repos = 0,
    skipped_repos = 0,
    slack_channel_id = "",
    slack_bot_token = "",
    slack_thread_id = "",
    start_time = undefined
}).

%% gen_statem callback specs
-spec init(config()) -> {ok, coordinating | completed, #coordinator_state{}} | {stop, term()}.
-spec callback_mode() -> state_functions.
-spec handle_event(term(), term(), atom(), #coordinator_state{}) -> {next_state, atom(), #coordinator_state{}}.
-spec terminate(term(), atom(), #coordinator_state{}) -> ok.
-spec code_change(term(), atom(), #coordinator_state{}, term()) -> {ok, atom(), #coordinator_state{}}.

%% State function specs
-spec coordinating(info | {call, term()}, term(), #coordinator_state{}) ->
    {next_state, coordinating | completed, #coordinator_state{}} |
    {next_state, coordinating | completed, #coordinator_state{}, [{reply, term(), term()}]}.
-spec completed({call, term()}, term(), #coordinator_state{}) ->
    {next_state, completed, #coordinator_state{}, [{reply, term(), term()}]}.

%% ===================================================================
%% API functions
%% ===================================================================

start_deployment(Config) ->
    gen_statem:start(?MODULE, Config, []).

get_status(Pid) ->
    gen_statem:call(Pid, get_status).

%% ===================================================================
%% gen_statem callbacks
%% ===================================================================

callback_mode() -> state_functions.

init(Config) ->
    Repos = proplists:get_value(repos, Config),
    SlackChannelId = proplists:get_value(slack_channel_id, Config),
    SlackBotToken = proplists:get_value(slack_bot_token, Config),
    ThreadTitle = proplists:get_value(thread_title, Config, "🚀 Concurrent Deployment Automation"),

    RepoCount = length(Repos),
    io:format("Starting concurrent deployment for ~p repositories~n", [RepoCount]),

    % Send initial Slack message
    case deployer_slack:send_initial_message(SlackChannelId, SlackBotToken, ThreadTitle) of
        {ok, SlackThreadId} ->
            deployer_slack:send_message(SlackChannelId, SlackBotToken, SlackThreadId,
                io_lib:format("🚀 Starting concurrent deployment for ~p repositories...", [RepoCount])),

            State = #coordinator_state{
                config = Config,
                total_repos = RepoCount,
                slack_channel_id = SlackChannelId,
                slack_bot_token = SlackBotToken,
                slack_thread_id = SlackThreadId,
                start_time = erlang:system_time(second)
            },

            case Repos of
                [] ->
                    deployer_slack:send_message(SlackChannelId, SlackBotToken, SlackThreadId,
                        "ℹ️ No repositories to deploy"),
                    {ok, completed, State};
                _ ->
                    % Spawn workers for all repositories concurrently
                    Workers = spawn_workers(Repos, Config, self()),
                    NewState = State#coordinator_state{workers = Workers},
                    {ok, coordinating, NewState}
            end;
        {error, Reason} ->
            {stop, {slack_error, Reason}}
    end.

%% State functions
coordinating(info, {repo_started, RepoName, _WorkerPid}, State) ->
    #coordinator_state{workers = Workers,
                      slack_channel_id = SlackChannelId,
                      slack_bot_token = SlackBotToken,
                      slack_thread_id = SlackThreadId} = State,

    deployer_slack:send_message(SlackChannelId, SlackBotToken, SlackThreadId,
        io_lib:format("📦 [~s] Starting deployment", [RepoName])),

    % Update worker status
    UpdatedWorkers = maps:update(RepoName,
        fun({Pid, _Status, StartTime}) -> {Pid, started, StartTime} end, Workers),

    {next_state, coordinating, State#coordinator_state{workers = UpdatedWorkers}};

coordinating(info, {repo_completed, RepoName, WorkerPid, success}, State) ->
    handle_repo_completion(RepoName, WorkerPid, success, "✅", State);

coordinating(info, {repo_skipped, RepoName, WorkerPid, Reason}, State) ->
    #coordinator_state{slack_channel_id = SlackChannelId,
                      slack_bot_token = SlackBotToken,
                      slack_thread_id = SlackThreadId} = State,

    deployer_slack:send_message(SlackChannelId, SlackBotToken, SlackThreadId,
        io_lib:format("ℹ️ [~s] Deployment skipped: ~s", [RepoName, Reason])),

    handle_repo_completion(RepoName, WorkerPid, skipped, "⚠️", State);

coordinating(info, {repo_failed, RepoName, WorkerPid, Reason}, State) ->
    #coordinator_state{slack_channel_id = SlackChannelId,
                      slack_bot_token = SlackBotToken,
                      slack_thread_id = SlackThreadId} = State,

    deployer_slack:send_message(SlackChannelId, SlackBotToken, SlackThreadId,
        io_lib:format("❌ [~s] Deployment failed: ~p", [RepoName, Reason])),

    handle_repo_completion(RepoName, WorkerPid, failed, "❌", State);

coordinating(info, {'DOWN', _Ref, process, WorkerPid, Reason}, State) ->
    #coordinator_state{workers = Workers} = State,

    % Find which repo this worker was handling
    case find_repo_by_pid(WorkerPid, Workers) of
        {ok, RepoName} ->
            io:format("Worker for ~s crashed: ~p~n", [RepoName, Reason]),
            coordinating(info, {repo_failed, RepoName, WorkerPid, {crash, Reason}}, State);
        not_found ->
            io:format("Unknown worker crashed: ~p~n", [Reason]),
            {next_state, coordinating, State}
    end;

coordinating(info, {repo_progress, RepoName, _WorkerPid, StateName, Details}, State) ->
    io:format("[~s] Progress: ~s - ~s~n", [RepoName, StateName, Details]),
    {next_state, coordinating, State};

coordinating({call, From}, get_status, State) ->
    #coordinator_state{workers = Workers,
                      total_repos = Total,
                      completed_repos = Completed,
                      failed_repos = Failed,
                      skipped_repos = Skipped} = State,

    Status = #{
        total_repos => Total,
        completed_repos => Completed,
        failed_repos => Failed,
        skipped_repos => Skipped,
        in_progress => Total - Completed - Failed - Skipped,
        workers => Workers
    },

    {next_state, coordinating, State, [{reply, From, Status}]}.

completed({call, From}, get_status, State) ->
    #coordinator_state{total_repos = Total,
                      completed_repos = Completed,
                      failed_repos = Failed,
                      skipped_repos = Skipped,
                      start_time = StartTime} = State,

    EndTime = erlang:system_time(second),
    Duration = EndTime - StartTime,

    Status = #{
        total_repos => Total,
        completed_repos => Completed,
        failed_repos => Failed,
        skipped_repos => Skipped,
        duration_seconds => Duration,
        status => completed
    },

    {next_state, completed, State, [{reply, From, Status}]}.

handle_event(_EventType, _EventContent, StateName, State) ->
    {next_state, StateName, State}.

terminate(_Reason, _StateName, _State) ->
    ok.

code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

%% ===================================================================
%% Internal functions
%% ===================================================================

-spec spawn_workers([repo_tuple()], config(), pid()) -> workers_map().
-spec handle_repo_completion(string(), pid(), worker_status(), string(), #coordinator_state{}) ->
    {next_state, coordinating | completed, #coordinator_state{}}.
-spec send_completion_summary(#coordinator_state{}) -> ok.
-spec send_progress_update(#coordinator_state{}) -> ok.
-spec find_repo_by_pid(pid(), workers_map()) -> {ok, string()} | not_found.

spawn_workers(Repos, Config, CoordinatorPid) ->
    ConfigWithThread = [{slack_thread_id, proplists:get_value(slack_thread_id, Config, "")} | Config],
    lists:foldl(fun({RepoName, _WorkflowName} = Repo, Acc) ->
        case deployer_deployment_worker:start_deployment(Repo, ConfigWithThread, CoordinatorPid) of
            {ok, WorkerPid} ->
                monitor(process, WorkerPid),
                StartTime = erlang:system_time(second),
                maps:put(RepoName, {WorkerPid, spawned, StartTime}, Acc);
            {error, Reason} ->
                io:format("Failed to spawn worker for ~s: ~p~n", [RepoName, Reason]),
                % Send failure notification immediately
                CoordinatorPid ! {repo_failed, RepoName, undefined, Reason},
                Acc
        end
    end, #{}, Repos).

handle_repo_completion(RepoName, _WorkerPid, Result, _Icon, State) ->
    #coordinator_state{workers = Workers,
                      completed_repos = Completed,
                      failed_repos = Failed,
                      skipped_repos = Skipped,
                      total_repos = Total} = State,

    % Update counters
    {NewCompleted, NewFailed, NewSkipped} = case Result of
        success -> {Completed + 1, Failed, Skipped};
        failed -> {Completed, Failed + 1, Skipped};
        skipped -> {Completed, Failed, Skipped + 1}
    end,

    % Update worker status
    UpdatedWorkers = maps:update(RepoName,
        fun({Pid, _Status, StartTime}) -> {Pid, Result, StartTime} end, Workers),

    NewState = State#coordinator_state{
        workers = UpdatedWorkers,
        completed_repos = NewCompleted,
        failed_repos = NewFailed,
        skipped_repos = NewSkipped
    },

    % Check if all repos are done
    TotalFinished = NewCompleted + NewFailed + NewSkipped,
    if TotalFinished >= Total ->
        send_completion_summary(NewState),
        {next_state, completed, NewState};
    true ->
        send_progress_update(NewState),
        {next_state, coordinating, NewState}
    end.

send_completion_summary(State) ->
    #coordinator_state{slack_channel_id = SlackChannelId,
                      slack_bot_token = SlackBotToken,
                      slack_thread_id = SlackThreadId,
                      completed_repos = Completed,
                      failed_repos = Failed,
                      skipped_repos = Skipped,
                      total_repos = Total,
                      start_time = StartTime} = State,

    EndTime = erlang:system_time(second),
    Duration = EndTime - StartTime,

    Summary = io_lib:format(
        "🎉 Concurrent deployment completed in ~ps~n"
        "📊 Results: ~p success, ~p failed, ~p skipped (~p total)",
        [Duration, Completed, Failed, Skipped, Total]),

    deployer_slack:send_message(SlackChannelId, SlackBotToken, SlackThreadId, Summary).

send_progress_update(State) ->
    #coordinator_state{slack_channel_id = SlackChannelId,
                      slack_bot_token = SlackBotToken,
                      slack_thread_id = SlackThreadId,
                      completed_repos = Completed,
                      failed_repos = Failed,
                      skipped_repos = Skipped,
                      total_repos = Total} = State,

    TotalFinished = Completed + Failed + Skipped,
    Progress = io_lib:format("🚀 Deployment Progress: ~p/~p repositories completed",
                           [TotalFinished, Total]),

    deployer_slack:send_message(SlackChannelId, SlackBotToken, SlackThreadId, Progress).

find_repo_by_pid(TargetPid, Workers) ->
    maps:fold(fun(RepoName, {Pid, _Status, _StartTime}, Acc) ->
        case Pid of
            TargetPid -> {ok, RepoName};
            _ -> Acc
        end
    end, not_found, Workers).
