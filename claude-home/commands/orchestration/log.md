---
description: "Log work from orchestrated tasks to external project management tools like Linear, Obsidian, Jira, or GitHub Issues."
---

## Usage

```
/orchestration/log [TASK-ID] [options]
```

## Description

Automatically creates work logs in your connected project management tools or knowledge bases, transferring task completion data, time spent, and progress notes to keep external systems synchronized.

## Basic Commands

### Log Current Task
```
/orchestration/log
```
Logs the currently in-progress task to available tools.

### Log Specific Task
```
/orchestration/log TASK-003
```
Logs a specific task's work.

### Choose Destination
```
/orchestration/log TASK-003 --choose
```
Manually select where to log the work.

## Destination Selection

When multiple tools are available or no obvious connection exists:

```
Where would you like to log this work?

Available destinations:
// ... (8 lines truncated)
```

## Obsidian Integration

### Daily Note Logging
```
/orchestration/log --obsidian-daily
```
Appends to today's daily note:

```markdown
## Work Log - 15:30

### TASK-003: JWT Implementation ✅
// ... (25 lines truncated)
```

### Project Note Logging
```
/orchestration/log --obsidian-project "Authentication System"
```
Creates or appends to project-specific note.

### Custom Obsidian Location
```
/orchestration/log --obsidian-path "Projects/Sprint 24/Work Log"
```

## Linear Integration
```
/orchestration/log TASK-003 --linear-issue ENG-1234
```
Creates work log comment in Linear issue.

## Smart Detection

The system detects available destinations:

```
Analyzing task context...

Found connections:
// ... (8 lines truncated)
```

## Work Log Formats

### Obsidian Format
```markdown
## 📋 Task: TASK-003 - JWT Implementation

### Summary
// ... (28 lines truncated)
```

### Linear Format
```
Work log comment in Linear with task details, time tracking, and progress updates.
```

## Multiple Destination Logging

```
/orchestration/log TASK-003 --multi

Select all destinations for logging:
// ... (7 lines truncated)
```

## Batch Operations

### Daily Summary to Obsidian
```
/orchestration/log --daily-summary --obsidian

Creates summary in daily note:
// ... (21 lines truncated)
```

### Weekly Report
```
/orchestration/log --weekly --obsidian-path "Weekly Reviews/Week 11"
```

## Templates

### Configure Obsidian Template
```yaml
obsidian_template:
  daily_note:
    heading: "## Work Log - {time}"
// ... (9 lines truncated)
```

### Configure Linear Template
```yaml
linear_template:
  include_time: true
  update_status: true
  add_labels: ["from-orchestration"]
```

## Interactive Mode

```
/orchestration/log --interactive

Task: TASK-003 - JWT Implementation
// ... (18 lines truncated)
```

## Examples

### Example 1: End of Day Logging
```
/orchestration/log --eod

End of Day Summary:
// ... (14 lines truncated)
```

### Example 2: Sprint Review
```
/orchestration/log --sprint-review --week 11

Gathering week 11 data...
// ... (13 lines truncated)
```

### Example 3: No Connection Found
```
/orchestration/log TASK-009

No automatic destination found for TASK-009.
// ... (13 lines truncated)
```

## Configuration

### Default Destinations
```yaml
log_defaults:
  no_connection: "ask"  # ask|obsidian-daily|skip
  multi_connection: "ask"  # ask|all|first
// ... (10 lines truncated)
```

