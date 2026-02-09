---
allowed-tools: Bash, Read, Write, Edit, TodoWrite, WebFetch
description: "Install globally"
---

Complete installation and configuration guide for integrating the spec-workflow MCP (Model Context Protocol) server with Claude Code to enable advanced task management and parallel execution capabilities.

## Overview

The spec-workflow MCP server enables Claude to:
- Connect to your project's specification-based workflow system
- Manage tasks with full CRUD operations
- Track task status and dependencies
- Enable parallel task execution with multiple AI agents
- Maintain synchronization between specifications and implementation

## Instructions

This command provides a comprehensive setup guide for the spec-workflow MCP server. When executed, follow these steps in order:

1. Check prerequisites (Node.js 18+, npm, Claude Code)
2. Choose an installation method (NPM, GitHub, or Claude Command Suite)
3. Configure the MCP server (using .mcp.json or global settings)
4. Create project-specific configuration files
5. Verify the installation
6. Test the connection with spec-workflow commands

The guide below provides detailed instructions for each step.

## Prerequisites

Before installation, ensure you have:
- Node.js 18+ installed
- npm or yarn package manager
- Claude Code with MCP support enabled
- Git for cloning repositories

## Installation Methods

### Method 1: Direct Installation from NPM (Recommended)

```bash
npm install -g @pimzino/spec-workflow-mcp

# Or install locally in your project
npm install --save-dev @pimzino/spec-workflow-mcp
```

### Method 2: Install from GitHub Repository

```bash
# Clone the repository
git clone https://github.com/Pimzino/spec-workflow-mcp.git

// ... (12 lines truncated)
```

### Method 3: Install from Claude Command Suite

```bash
# Navigate to Claude Command Suite directory
cd Claude-Command-Suite

# Run the spec-workflow installer
./scripts/install-spec-workflow-mcp.sh
```

## Configuration

### Step 1: Configure MCP Server (Choose One Method)

### Method A: Project-Level Configuration (Recommended)

Create a `.mcp.json` file in your project root. This is the simplest and most portable method:

**Location:** `.mcp.json` (in project root)

```json
{
  "mcpServers": {
    "spec-workflow": {
// ... (7 lines truncated)
```

**Quick Setup:**
```bash
# Run this command in your project root
./scripts/add-spec-workflow-mcp.sh

// ... (13 lines truncated)
```

**Benefits of .mcp.json:**
- Project-specific configuration
- Automatically loaded when Claude Code opens the project
- Can be committed to version control for team consistency
- No global configuration needed

### Method B: Global Claude Configuration

Add the spec-workflow server to your global Claude configuration file:

**Location:** `~/.claude/settings.json`

```json
{
  "mcp": {
    "servers": {
// ... (13 lines truncated)
```

### Step 2: Project-Specific Configuration

Create a `.spec-workflow.json` file in your project root:

```json
{
  "version": "1.0.0",
  "project": {
// ... (37 lines truncated)
```

### Step 3: Environment Variables

Set up environment variables for the MCP server:

```bash
# Add to ~/.bashrc, ~/.zshrc, or equivalent
export SPEC_WORKFLOW_PROJECT_PATH="$HOME/projects"
export SPEC_WORKFLOW_AUTO_SYNC="true"
export SPEC_WORKFLOW_LOG_LEVEL="info"
export SPEC_WORKFLOW_CACHE_DIR="$HOME/.cache/spec-workflow"
```

## Verification

### Test MCP Server Connection

After configuration, verify the connection:

```bash
# Start Claude Code
claude code

# In Claude, test the connection
/spec-workflow:test-connection
```

Expected output:
```
✅ Spec-Workflow MCP Server Connected
Version: 1.0.0
Project: My Project
Specifications Found: 5
Tasks Available: 23
Status: Ready
```

### List Available Specifications

```bash
# In Claude Code
/spec-workflow:list-specs
```

### Check Task Status

```bash
# In Claude Code
/spec-workflow:task-status
```

## Usage Examples

### Basic Task Management

```bash
# List all tasks in a specification
/spec-workflow:list-tasks --spec-name user-authentication

// ... (9 lines truncated)
```

### Parallel Task Execution

```bash
# Execute tasks in parallel with auto-assignment
/spec-workflow:parallel-tasks --spec-name feature-xyz --auto-assign

// ... (6 lines truncated)
```

## Advanced Configuration

### Custom Task Statuses

Define project-specific task statuses:

```json
{
  "tasks": {
    "statuses": [
// ... (12 lines truncated)
```

### Agent Configuration

Customize agent behavior:

```json
{
  "agents": {
    "profiles": {
// ... (16 lines truncated)
```

### Webhook Integration

Configure webhooks for external notifications:

```json
{
  "webhooks": {
    "enabled": true,
// ... (11 lines truncated)
```

## Integration with Claude Command Suite

The spec-workflow MCP server integrates seamlessly with Claude Command Suite commands:

### Compatible Commands

- `/orchestrate` - Task orchestration with spec-workflow backend
- `/task-status` - Check spec-workflow task progress
- `/task-move` - Update task status in specifications
- `/team:sprint-planning` - Plan sprints using specifications
- `/sync:bidirectional-sync` - Sync between spec-workflow and other systems

### Workflow Examples

**Feature Development Workflow:**
```bash
1. /spec-workflow:create-spec --name user-dashboard
2. /spec-workflow:generate-tasks --spec-name user-dashboard
3. /spec-workflow:parallel-tasks --spec-name user-dashboard --auto-assign
4. /test:generate-test-cases --from-spec user-dashboard
5. /deploy:prepare-release --include-spec user-dashboard
```

**Migration Workflow:**
```bash
1. /spec-workflow:import-from-github --milestone v2.0
2. /spec-workflow:analyze-dependencies
3. /spec-workflow:create-migration-plan
4. /spec-workflow:parallel-tasks --system spec-workflow --focus migration
```

## Support and Resources

- **GitHub Repository:** https://github.com/Pimzino/spec-workflow-mcp
- **Documentation:** https://spec-workflow.dev/docs
- **Issues:** https://github.com/Pimzino/spec-workflow-mcp/issues
- **Discord Community:** https://discord.gg/spec-workflow
- **Claude Command Suite:** https://github.com/qdhenry/Claude-Command-Suite

## Next Steps

After successful setup:

1. Create your first specification: `/spec-workflow:create-spec`
2. Generate tasks from requirements: `/spec-workflow:generate-tasks`
3. Execute tasks in parallel: `/spec-workflow:parallel-tasks`
4. Monitor progress: `/spec-workflow:dashboard`
5. Generate reports: `/spec-workflow:report`

---

**Quick Start Command:**
```bash
/spec-workflow:parallel-tasks --auto-assign
```
