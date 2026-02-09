---
description: "Orchestrated parallel implementation of complex features using multiple agents, with dependency-aware batching and synchronized progress tracking."
---

## Instructions

Build features in parallel using agent orchestration for: **$ARGUMENTS**

This command extends the incremental approach with parallel execution capabilities, launching multiple agents to work on independent features simultaneously while maintaining strict tracking and clean merge protocols.

> **Note:** `$ARGUMENTS` is automatically replaced with the text following the command invocation.
> Example: `/dev:parallel-feature-build e-commerce checkout system` sets `$ARGUMENTS` to "e-commerce checkout"

---

## Phase 1: Feature Requirements & Dependency Analysis

### 1.1 Create Feature Tracking Directory

```bash
mkdir -p .feature-tracking/{agents,batches,merges} || { echo "ERROR: Cannot create directories. Check permissions."; exit 1; }
```

### 1.2 Generate Comprehensive Feature List

Same as incremental approach - expand user request into granular features.

**Create file: `.feature-tracking/features.json`**

```json
{
  "project": "$ARGUMENTS",
  "created": "YYYY-MM-DD",
// ... (7 lines truncated)
```

### 1.3 Enhanced Feature Schema for Parallel Execution

```json
{
  "id": "FEAT-001",
  "category": "functional|ui|integration|performance|security|accessibility",
// ... (7 lines truncated)
```

### 1.4 Build Dependency Graph

**Create file: `.feature-tracking/dependency-graph.json`**

```json
{
  "generated": "YYYY-MM-DD HH:MM",
  "criticalPath": ["FEAT-001", "FEAT-005", "FEAT-012"],
// ... (7 lines truncated)
```

### 1.5 Dependency Analysis Rules

1. **No Dependencies**: Can start immediately in Batch 1
2. **Single Dependency**: Waits for that feature only
3. **Multiple Dependencies**: Waits for ALL dependencies
4. **Circular Detection**: FAIL if cycles found - must restructure

Verify no cycles exist before proceeding. If detected: split one feature into sub-features or merge dependent features, then re-run analysis.

---

## Phase 2: Parallel Execution Planning

### 2.1 Calculate Optimal Batches

Use topological sort to determine execution order:

```
Batch 1: All features with no dependencies (run in parallel)
Batch 2: Features depending only on Batch 1 (run in parallel after Batch 1)
Batch 3: Features depending on Batch 1 or 2 (run in parallel after Batch 2)
...continue until all features assigned
```

### 2.2 Agent Assignment Strategy

**Create file: `.feature-tracking/agent-assignments.json`**

```json
{
  "strategy": "round-robin|load-balanced|priority-based",
  "maxAgents": 4,
// ... (7 lines truncated)
```

### 2.3 Create Main Feature Branch

```bash
# Create and switch to the main feature branch
git checkout -b feature/$PROJECT_NAME-main
// ... (6 lines truncated)
```

**Important:** All agent branches will merge INTO this branch, not directly to main/master.

### 2.4 Create Master Coordination Document

**Create file: `.feature-tracking/COORDINATION.md`**

```markdown
# Parallel Feature Build Coordination

## Project: $ARGUMENTS
// ... (7 lines truncated)
```

---

## Phase 3: Agent Orchestration

### 3.1 Launch Parallel Agents

For each feature in the current batch, use the Task tool to spawn an agent:

```markdown
**Agent Instructions for FEAT-XXX:**

You are implementing feature FEAT-XXX for project: $ARGUMENTS
// ... (7 lines truncated)
```json
{
  "agentId": "agent-X",
  "featureId": "FEAT-XXX",
  "status": "completed",
  "branch": "feat/agent-X-FEAT-XXX",
  "commitHash": "[hash]",
  "completedAt": "YYYY-MM-DD HH:MM",
  "notes": "Any implementation notes"
}
```

Report back when complete.
```

### 3.2 Agent Progress Files

Each agent writes to: `.feature-tracking/agents/agent-X-progress.md`

```markdown
# Agent X Progress

## Assigned Feature: FEAT-XXX
// ... (7 lines truncated)
```

### 3.3 Monitor Agent Completion

Poll agent status files until all agents in batch complete:

```bash
# Check all agent statuses with polling interval
check_agents_complete() {
  local all_complete=true
// ... (7 lines truncated)
```

---

## Phase 4: Merge Coordination

### 4.1 Merge Order Protocol

After all agents in a batch complete:

1. **Sort by Feature ID** for deterministic merge order
2. **Merge sequentially** to main feature branch
3. **Verify after each merge** that codebase still works

### 4.2 Merge Workflow

```bash
# Read the main feature branch name (created in Phase 2.3)
MAIN_BRANCH=$(cat .feature-tracking/main-branch.txt)

// ... (7 lines truncated)
```

### 4.3 Update Master Tracking

After successful merge, update `.feature-tracking/features.json` using file locking to prevent race conditions:

```bash
# Acquire lock before updating shared tracking file
LOCKFILE=".feature-tracking/.features.lock"

// ... (7 lines truncated)
```

### 4.4 Conflict Resolution Protocol

If merge conflicts occur:

1. **Identify conflicting files**
2. **Analyze which agent's changes take precedence**
3. **Resolve favoring the more complete implementation**
4. **Document resolution** in merge commit message
5. **Re-verify affected features**

---

## Phase 5: Batch Progression

### 5.1 Batch Completion Check

```bash
# Verify all features in batch are merged
cat .feature-tracking/features.json | jq '[.features[] | select(.batch == 1 and .status != "passed")] | length'
# Must return 0
```

### 5.2 Advance to Next Batch

1. Update COORDINATION.md with batch completion status
2. Identify next batch of features
3. Assign to agents (may reuse same agents)
4. Launch new parallel execution round

### 5.3 Continue Until All Batches Complete

```
While batches remain:
  1. Launch agents for current batch (parallel)
  2. Wait for all agents to complete
  3. Merge all branches sequentially
  4. Verify merged codebase
  5. Advance to next batch
```

---

## Phase 6: Recovery & Error Handling

### 6.1 Agent Failure Protocol

If an agent fails: check progress file for last known state, `git cherry-pick` good commits if possible, reassign feature to a different agent or handle sequentially. Do NOT block other agents in the same batch.

### 6.2 Merge Failure Protocol

If merge cannot be resolved: `git merge --abort`, identify conflicting features, re-implement one feature to avoid conflict, or execute features sequentially instead.

### 6.3 Rollback Batch

```bash
# Find commit before batch started
git log --oneline | grep "Batch X start"
// ... (5 lines truncated)
```

---

## Phase 7: Completion & Reporting

### 7.1 Final Verification

```bash
# All features must be passed
cat .feature-tracking/features.json | jq '[.features[] | select(.status != "passed")] | length'
# Must return 0
```

### 7.2 Generate Performance Report

**Create file: `.feature-tracking/PERFORMANCE-REPORT.md`**

```markdown
# Parallel Execution Performance Report

## Summary
// ... (7 lines truncated)
```

### 7.3 Final Commit

```bash
git add .
git commit -m "feat: complete parallel build of $ARGUMENTS

// ... (7 lines truncated)
```

---

## Usage Examples

```
/dev:parallel-feature-build e-commerce checkout system
/dev:parallel-feature-build --agents 2 user dashboard
```

---
