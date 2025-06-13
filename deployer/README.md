# Deployer - Automated Deployment CLI

An Erlang escript that automates deployment workflows by integrating GitHub and Slack APIs.

## Features

- JSON configuration support with configurable branch names
- Creates PRs between configurable master/staging branches
- Checks for conflicts and handles approvals
- Auto-merges when ready
- Creates release branches with date format (release-YYYYMMDD)
- Generates releases with auto-incremented semver
- Triggers GitHub workflows with concrete action run links
- Sends threaded Slack notifications with hyperlinks
- Notifies QA team when workflows complete

## Prerequisites

- Erlang/OTP installed
- GitHub API token with appropriate permissions
- Slack Bot token with chat:write permissions

## Configuration Fields

- **github_api_key**: Your GitHub personal access token
- **slack_bot_token**: Your Slack bot OAuth token
- **slack_channel_id**: Target Slack channel ID for notifications
- **thread_title**: Custom title for the Slack thread (optional, defaults to "🚀 Deployment Automation")
- **master_branch**: Name of master branch (optional, defaults to "main")
- **staging_branch**: Name of staging branch (optional, defaults to "staging")
- **repos**: Array of repository/workflow pairs to process

## Quick Start

1. **Setup APIs**: Follow the [API Setup Guide](./API_SETUP_GUIDE.md) to get your GitHub and Slack tokens

2. **Create config file**:
```json
{
  "github_api_key": "ghp_your_github_token",
  "slack_bot_token": "xoxb-your-slack-bot-token",
  "slack_channel_id": "C1234567890",
  "thread_title": "🚀 Production Deployment",
  "repos": [
    {"repo": "owner/repo1", "workflow": "deploy.yml"},
    {"repo": "owner/repo2", "workflow": "ci.yml"}
  ]
}
```

3. **Run deployment**:
```bash
./deployer config.json
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
  "repos": [
    {"repo": "owner/repo", "workflow": "deploy.yml"}
  ]
}
```

## Workflow Process

For each repository:

1. **Create PR**: master → staging
2. **Check Conflicts**: Verifies PR can be merged
3. **Wait for Approval**: Polls until PR is approved and mergeable
4. **Auto-merge**: Merges the PR when ready
5. **Create Release Branch**: Creates `release-YYYYMMDD` from staging
6. **Create Release**: Auto-generates release notes with incremented version
7. **Trigger Workflow**: Runs specified GitHub workflow
8. **Notify QA**: Mentions @qa team when workflow completes
9. **Final PR**: Creates PR from release branch to master

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