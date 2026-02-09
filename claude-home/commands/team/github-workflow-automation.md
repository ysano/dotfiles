---
description: "Automate GitHub workflows with Actions, Webhooks, and Project automation"
---

## Instructions

Design and implement GitHub-native automation workflows. Use `gh` CLI and GitHub Actions YAML for all operations.

Workflow to automate: `$ARGUMENTS`

### Capabilities

1. **Issue Lifecycle Automation**
   - Auto-add issues to Projects on creation
   - Label-based routing and assignment
   - Stale issue detection and cleanup

2. **PR Workflow Automation**
   - Auto-link issues from PR description
   - Update Project status on merge
   - Generate release notes from merged PRs

3. **Deployment Integration**
   - Update Project items on staging/production deploy
   - Auto-create rollback issues on failure
   - Hot fix priority workflow

4. **Quality Gate Automation**
   - Link security scan findings to issues
   - Performance regression tracking
   - Code coverage threshold enforcement

5. **Reporting Workflows**
   - Velocity calculation via GitHub Actions scheduled runs
   - Sprint burndown data collection
   - Team performance insights generation

When generating workflow YAML, follow GitHub Actions best practices: pin action versions, use job-level permissions, minimize secrets exposure.
