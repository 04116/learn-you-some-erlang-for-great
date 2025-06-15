# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Build Commands

```bash
# Compile and build escript
rebar3 compile
rebar3 escriptize

# Run tests
rebar3 eunit

# Clean build artifacts
rebar3 clean

# Generate documentation
rebar3 edoc

# Run in development shell
rebar3 shell
```

## Architecture Overview

This is an Erlang/OTP application that provides concurrent deployment automation using GitHub and Slack APIs. The architecture follows OTP principles with supervision trees and centralized state machine logic:

- **deployer_app**: Main OTP application entry point
- **deployer_sup**: Application supervisor managing all processes
- **deployer_cli**: Command-line interface and main entry point for escript
- **deployer_config**: JSON configuration parsing and validation
- **deployer_coordinator**: gen_statem coordinator managing multiple worker processes concurrently
- **deployer_deployment_worker**: Centralized gen_statem handling all deployment logic including task state, workflow monitoring, and branch management
- **deployer_github**: GitHub API integration with REST endpoints
- **deployer_slack**: Slack API integration for notifications

## Key Design Patterns

- **Centralized State Machine**: Single worker module contains all deployment logic including task state, workflow monitoring, and branch management
- **Idempotent Operations**: Uses UTC+7 date-based task identification to ensure deployments run only once per day
- **Concurrent Processing**: Uses multiple gen_statem workers to process repositories simultaneously
- **Supervision**: Proper OTP supervision tree for fault tolerance
- **Configuration-driven**: JSON configuration files define deployment targets and settings
- **Smart Branch Management**: Automatically handles master->staging syncing before releases

## Dependencies

- jsx: JSON parsing library (version 3.1.0)
- inets, ssl: HTTP client support for API calls

## Configuration

The application expects a JSON configuration file with GitHub API tokens, Slack tokens, and repository/workflow pairs. See config.json.example for structure.

## Workflow Logic

The deployer implements an idempotent, deterministic workflow based on UTC+7 date with gen_statem state machines:

1. **Idempotency Check**: Uses date-based task IDs and workflow completion status to prevent duplicate runs
2. **Branch Status Analysis**: Determines if master needs to merge to staging first
3. **Master→Staging Sync**: If master is ahead, automatically syncs to staging
4. **Release Creation**: If staging is ahead, creates release branch from staging
5. **Workflow Monitoring**: Uses dedicated gen_statem to monitor workflow completion on release branch head commit
6. **Task Completion**: Job is only considered done when workflow completes successfully

## State Machine Architecture

### Centralized Deployment Worker (deployer_deployment_worker)

**Clear workflow sequence:**
1. **idle** → **checking_idempotency**
2. **checking_idempotency** → **checking_master_vs_staging**
3. **checking_master_vs_staging** → **creating_pr_master_to_staging** | **checking_staging_vs_master**
4. **creating_pr_master_to_staging** → **waiting_for_master_staging_merge** → **checking_staging_vs_master**
5. **checking_staging_vs_master** → **creating_release_branch**
6. **creating_release_branch** → **creating_pr_release_to_master**
7. **creating_pr_release_to_master** → **triggering_workflow**
8. **triggering_workflow** → **monitoring_workflow**
9. **monitoring_workflow** → **creating_release** → **completed**

**State meanings:**
- **checking_master_vs_staging**: Compare master and staging branches
- **creating_pr_master_to_staging**: If master ahead, create and merge PR master→staging
- **checking_staging_vs_master**: Compare staging and master after sync
- **creating_release_branch**: Create release branch from staging
- **creating_pr_release_to_master**: Create PR release→master (NOT merged, manual)
- **triggering_workflow**: Trigger workflow on release branch head commit
- **monitoring_workflow**: Monitor workflow until completion
- **creating_release**: Create release with notes from release branch

**Important**: When workflow logic changes, the state machine transitions in `deployer_deployment_worker` must be updated accordingly to reflect the new flow.

## Entry Points

- **CLI**: `./_build/default/bin/deployer config.json`
- **Shell**: `deployer_cli:main(["config.json"])`

