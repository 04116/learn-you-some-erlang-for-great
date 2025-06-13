# API Setup Guide

This guide will walk you through setting up GitHub and Slack API access for the deployment automation tool.

## GitHub API Setup

### 1. Generate Personal Access Token (Classic)

1. **Go to GitHub Settings**
   - Visit [GitHub Settings > Developer settings > Personal access tokens](https://github.com/settings/tokens)
   - Click "Generate new token" → "Generate new token (classic)"

2. **Configure Token**
   - **Note**: Give it a descriptive name like "Deployer Bot"
   - **Expiration**: Choose appropriate expiration (recommended: 90 days or custom)
   - **Select scopes**: Check the following permissions:
     - ✅ `repo` (Full control of private repositories)
       - This includes: repo:status, repo_deployment, public_repo, repo:invite, security_events
     - ✅ `workflow` (Update GitHub Action workflows)
     - ✅ `write:packages` (Upload packages to GitHub Package Registry)
     - ✅ `read:org` (Read org and team membership, read org projects)

3. **Generate and Copy Token**
   - Click "Generate token"
   - **⚠️ IMPORTANT**: Copy the token immediately (it won't be shown again)
   - Format: `ghp_xxxxxxxxxxxxxxxxxxxx`

### 2. Alternative: Fine-grained Personal Access Token (Beta)

If you prefer more granular control:

1. **Create Fine-grained Token**
   - Go to [GitHub Settings > Developer settings > Personal access tokens > Fine-grained tokens](https://github.com/settings/personal-access-tokens/fine-grained)
   - Click "Generate new token"

2. **Configure Repository Access**
   - **Resource owner**: Select your user/organization
   - **Repository access**: Select specific repositories or all repositories
   - **Expiration**: Set appropriate expiration

3. **Repository Permissions**
   - **Actions**: Read and write (for workflow triggers)
   - **Contents**: Read and write (for branch operations)
   - **Issues**: Write (for release notes)
   - **Metadata**: Read (required)
   - **Pull requests**: Write (for creating and merging PRs)

### 3. Verify Token

Test your token with curl:
```bash
curl -H "Authorization: Bearer YOUR_TOKEN" \
     -H "Accept: application/vnd.github+json" \
     https://api.github.com/user
```

## Slack API Setup

### 1. Create Slack App

1. **Visit Slack App Dashboard**
   - Go to [https://api.slack.com/apps](https://api.slack.com/apps)
   - Click "Create New App"
   - Choose "From scratch"

2. **App Configuration**
   - **App Name**: Enter "Deployment Bot" or similar
   - **Workspace**: Select your workspace
   - Click "Create App"

### 2. Configure OAuth & Permissions

1. **Navigate to OAuth & Permissions**
   - In your app dashboard, go to "OAuth & Permissions" in the sidebar

2. **Bot Token Scopes**
   Add the following scopes under "Bot Token Scopes":
   - ✅ `chat:write` - Send messages as the bot
   - ✅ `chat:write.public` - Send messages to channels the bot isn't a member of
   - ✅ `channels:read` - View basic information about public channels
   - ✅ `groups:read` - View basic information about private channels (if needed)

3. **Install App to Workspace**
   - Scroll to "OAuth Tokens for Your Workspace"
   - Click "Install to Workspace"
   - Review permissions and click "Allow"
   - Copy the "Bot User OAuth Token" (starts with `xoxb-`)

### 3. Get Channel ID

1. **Find Channel ID via Slack**
   - Right-click on the channel name in Slack
   - Select "Copy link"
   - The URL contains the channel ID: `https://yourworkspace.slack.com/archives/C1234567890`
   - Channel ID format: `C1234567890`

2. **Alternative: Using API**
   ```bash
   curl -H "Authorization: Bearer xoxb-your-bot-token" \
        https://slack.com/api/conversations.list
   ```

### 4. Add Bot to Channel

1. **Invite Bot to Channel**
   - Go to your target Slack channel
   - Type: `/invite @YourBotName`
   - Or mention the bot: `@YourBotName`

2. **Verify Access**
   Test with a simple message:
   ```bash
   curl -X POST https://slack.com/api/chat.postMessage \
        -H "Authorization: Bearer xoxb-your-bot-token" \
        -H "Content-Type: application/json" \
        -d '{
          "channel": "C1234567890",
          "text": "Hello from deployer bot!"
        }'
   ```

## Configuration File Setup

### JSON Format (Recommended)

Create `config.json`:
```json
{
  "github_api_key": "ghp_your_github_token_here",
  "slack_channel_id": "C1234567890",
  "repos": [
    {
      "repo": "your-org/frontend-app",
      "workflow": "deploy.yml"
    },
    {
      "repo": "your-org/backend-api",
      "workflow": "ci-cd.yml"
    }
  ]
}
```

### Configuration Integration

The Slack bot token is now configured directly in your config file instead of environment variables. This provides better security and configuration management.

**JSON Configuration:**
```json
{
  "slack_bot_token": "xoxb-your-slack-bot-token",
  "thread_title": "🚀 My Custom Deployment"
}
```

**Erlang Configuration:**
```erlang
{slack_bot_token, "xoxb-your-slack-bot-token"}.
{thread_title, "🚀 My Custom Deployment"}.
```

## Security Best Practices

### 1. Token Storage
- ✅ **DO**: Store tokens in environment variables
- ✅ **DO**: Use secret management systems in production
- ❌ **DON'T**: Commit tokens to version control
- ❌ **DON'T**: Share tokens in chat or email

### 2. Access Control
- Use principle of least privilege for token scopes
- Regularly rotate tokens (every 90 days recommended)
- Monitor token usage in GitHub/Slack audit logs
- Revoke unused tokens immediately

### 3. Production Setup
- Use separate tokens for dev/staging/production
- Implement token rotation automation
- Monitor API rate limits and usage
- Set up alerting for failed API calls

## Troubleshooting

### Common GitHub Issues

**403 Forbidden**
- Check token permissions/scopes
- Verify repository access
- Ensure token hasn't expired

**404 Not Found**
- Verify repository names are correct
- Check if repository is private and token has access
- Confirm branch names exist (master/main, staging)

**422 Unprocessable Entity**
- Workflow file doesn't exist
- Branch protection rules preventing merge
- PR conflicts need resolution

### Common Slack Issues

**channel_not_found**
- Verify channel ID is correct
- Bot needs to be added to private channels
- Channel might be archived

**not_in_channel**
- Invite bot to the channel: `/invite @BotName`
- Check bot permissions

**missing_scope**
- Add required OAuth scopes in app settings
- Reinstall app to workspace after scope changes

### Rate Limits

**GitHub**: 5,000 requests/hour for authenticated requests
**Slack**: Tier-based limits, typically 1+ request/second

If you hit rate limits:
- Implement exponential backoff
- Reduce polling frequency
- Consider using webhooks instead of polling

## Testing Your Setup

1. **Test GitHub API**:
   ```bash
   curl -H "Authorization: Bearer YOUR_GITHUB_TOKEN" \
        https://api.github.com/repos/OWNER/REPO
   ```

2. **Test Slack API**:
   ```bash
   curl -X POST https://slack.com/api/chat.postMessage \
        -H "Authorization: Bearer YOUR_SLACK_TOKEN" \
        -H "Content-Type: application/json" \
        -d '{"channel": "YOUR_CHANNEL_ID", "text": "Test message"}'
   ```

3. **Test Deployer**:
   ```bash
   ./deployer config.json
   ```

Your setup is complete when both API tests return successful responses!