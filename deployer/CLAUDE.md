# CLAUDE.md - Deployment Automation Tool

## Project Overview

This is an Erlang escript CLI tool that automates deployment workflows by integrating GitHub and Slack APIs. The tool manages the complete deployment lifecycle from PR creation through workflow execution and team notifications.

## Key Features

- **JSON Configuration**: Uses JSX library for robust JSON parsing with configurable branch names
- **GitHub Integration**: Creates PRs, manages releases, triggers workflows with real API calls
- **Slack Integration**: Sends threaded notifications with hyperlinks to concrete GitHub actions
- **Release Management**: Auto-creates date-based release branches and semantic versioned releases
- **Workflow Automation**: Triggers GitHub workflows and provides direct links to action runs

## Dependencies

- **JSX 3.1.0**: Production-grade JSON parsing library for Erlang
- **Erlang/OTP**: Core runtime environment
- **inets**: HTTP client for API calls
- **ssl**: Secure connections for HTTPS

## Configuration

The tool uses JSON configuration files with the following structure:

```json
{
  "github_api_key": "github_pat_...",
  "slack_bot_token": "xoxb-...",
  "slack_channel_id": "C...",
  "thread_title": "🚀 Production Deployment", 
  "master_branch": "main",
  "staging_branch": "staging",
  "repos": [
    {"repo": "owner/repo", "workflow": "deploy.yml"}
  ]
}
```

## Commands for Development

### Build and Test
```bash
# Get dependencies
rebar3 deps get

# Compile JSX dependency
rebar3 compile

# Run deployment automation
./working_deployer config.json
```

### Lint and Type Check
*Note: Add specific commands here if the project implements linting*

## API Integration Details

### GitHub API
- **Authentication**: Personal Access Token with repo and actions permissions
- **Endpoints Used**: 
  - `/repos/{owner}/{repo}/pulls` - PR management
  - `/repos/{owner}/{repo}/git/refs` - Branch operations
  - `/repos/{owner}/{repo}/releases` - Release creation
  - `/repos/{owner}/{repo}/actions/workflows/{workflow}/dispatches` - Workflow triggering
  - `/repos/{owner}/{repo}/actions/workflows/{workflow}/runs` - Workflow run tracking

### Slack API
- **Authentication**: Bot OAuth token with chat:write permissions
- **Features**: 
  - Threaded message conversations
  - Hyperlinked GitHub resources
  - Real-time progress updates
  - QA team notifications

## Architecture Notes

- **Single File Escript**: `working_deployer` contains all functionality for portability
- **JSON Parsing**: Uses JSX library for robust JSON handling with binary keys
- **Error Handling**: Comprehensive error reporting through Slack notifications
- **Threading**: All Slack messages properly threaded using extracted timestamps
- **Hyperlinks**: Direct links to GitHub PRs, branches, releases, and action runs

## Testing Strategy

The tool makes real API calls to GitHub and Slack for integration testing:
- Creates actual PRs and branches in the configured repository
- Triggers real GitHub workflows
- Sends notifications to configured Slack channels
- Validates all API responses and error handling

## Known Limitations

- Branch conflicts need manual resolution
- Relies on existing staging branches
- Release branches are date-based (release-YYYYMMDD format)
- Workflow completion is simulated rather than monitored

This tool successfully automates complex deployment workflows while providing clear visibility through Slack integration.