# Deployer - Concurrent Deployment Automation

A rebar3 Erlang application that provides concurrent automated deployment workflows by integrating GitHub and Slack APIs.

## Features

- **Concurrent processing**: Processes multiple repositories simultaneously using Erlang/OTP gen_statem workers
- **Modular architecture**: Clean separation of concerns with dedicated modules for GitHub API, Slack API, configuration, coordination, and workers
- **JSON configuration support** with configurable branch names and settings
- **Smart PR handling**: Creates PRs between configurable master/staging branches with conflict detection
- **Automatic merging**: Auto-merges when ready, with retry logic for blocked PRs
- **Release automation**: Creates release branches with date format (release-YYYYMMDD) and auto-incremented semver
- **Workflow triggering**: Integrates with GitHub Actions workflows
- **Real-time notifications**: Sends threaded Slack notifications with progress updates
- **Robust error handling**: Comprehensive error handling with Slack notifications for failures

## Prerequisites

- Erlang/OTP 24+ installed
- rebar3 build tool
- GitHub API token with appropriate permissions
- Slack Bot token with chat:write permissions

## Configuration Fields

- **github_api_key**: Your GitHub personal access token
- **slack_bot_token**: Your Slack bot OAuth token
- **slack_channel_id**: Target Slack channel ID for notifications
- **thread_title**: Custom title for the Slack thread (optional, defaults to "🚀 Concurrent Deployment Automation")
- **master_branch**: Name of master branch (optional, defaults to "main")
- **staging_branch**: Name of staging branch (optional, defaults to "staging")
- **concurrent**: Enable concurrent processing (optional, defaults to true)
- **repos**: Array of repository/workflow pairs to process concurrently

## Quick Start

1. **Build the application**:

```bash
rebar3 compile
rebar3 escriptize
```

2. **Create config file**:

```json
{
  "github_api_key": "ghp_your_github_token",
  "slack_bot_token": "xoxb-your-slack-bot-token",
  "slack_channel_id": "C1234567890",
  "thread_title": "🚀 Production Deployment",
  "concurrent": true,
  "repos": [
    { "repo": "owner/repo1", "workflow": "deploy.yml" },
    { "repo": "owner/repo2", "workflow": "ci.yml" }
  ]
}
```

3. **Run deployment**:

```bash
./_build/default/bin/deployer config.json
```

## Application Architecture

The application is structured as a proper OTP application with the following modules:

- **`deployer_app`**: Main OTP application module
- **`deployer_sup`**: Application supervisor
- **`deployer_cli`**: Command-line interface and main entry point
- **`deployer_config`**: Configuration parsing and validation
- **`deployer_coordinator`**: Coordinator gen_statem managing multiple workers
- **`deployer_worker`**: Worker gen_statem handling individual repository deployments
- **`deployer_github`**: GitHub API integration module
- **`deployer_slack`**: Slack API integration module

## Building and Development

### Build Commands

```bash
# Compile the application
rebar3 compile

# Build the escript executable
rebar3 escriptize

# Run tests (if any)
rebar3 eunit

# Clean build artifacts
rebar3 clean

# Generate documentation
rebar3 edoc
```

### Running in Development

You can also run the application directly in the Erlang shell:

```bash
rebar3 shell
```

Then in the shell:

```erlang
deployer_cli:main(["config.json"]).
```

## Configuration Format

The deployer uses JSON configuration files with the following structure:

```json
{
  "github_api_key": "ghp_xxxxxxxxxxxxxxxxxxxx",
  "slack_bot_token": "xoxb-your-slack-bot-token",
  "slack_channel_id": "C1234567890",
  "thread_title": "🚀 Production Deployment",
  "master_branch": "main",
  "staging_branch": "staging",
  "repos": [{ "repo": "owner/repo", "workflow": "deploy.yml" }]
}
```

## Workflow Process

The deployer processes all repositories **concurrently** using Erlang/OTP worker processes. For each repository:

1. **Create PR**: staging → master (checks for differences first)
2. **Handle Conflicts**: If PR cannot merge, polls every 5 minutes for manual resolution (up to 6 hours)
3. **Auto-merge**: Merges the PR when ready
4. **Create Release Branch**: Creates `release-YYYYMMDD` from master
5. **Concurrent Post-Release Operations**: Runs 3 operations in parallel:
   - **Create Release**: Auto-generates release with incremented semver
   - **Trigger Workflow**: Runs specified GitHub workflow
   - **Create Final PR**: Creates PR from release branch to master
6. **Real-time Updates**: Sends Slack notifications throughout the process
7. **Summary Report**: Final status report with timing and results for all repositories

### Concurrent Benefits

- **Speed**: Multiple repositories are processed simultaneously
- **Efficiency**: I/O operations (API calls) don't block other repositories
- **Resilience**: Failures in one repository don't affect others
- **Real-time Feedback**: Progress updates for each repository as they complete different stages

## Configuration

### GitHub Token Permissions

- Contents: Read and write
- Pull requests: Read and write
- Actions: Read and write
- Metadata: Read

### Slack Bot Permissions

- chat:write
- chat:write.public

## Error Handling

The tool includes comprehensive error handling and will:

- Report conflicts in PRs
- Retry operations when appropriate
- Send error notifications to Slack
- Gracefully handle API failures
