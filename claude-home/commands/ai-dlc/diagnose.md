---
description: "Generate data-driven sprint diagnostic for retrospective"
---

## Instructions

Generate an AI-DLC data-driven diagnostic report for a retrospective session. Load `ai-dlc-ceremonies` skill for diagnostic session patterns. Load `ticket-management` skill for Churn / AI-Confidence / Janitor rules context.

Sprint range: `$ARGUMENTS`

If no arguments provided, auto-detect from the most recent milestone or use the last 7-14 days.

### Before: AI Diagnostic Report

**B.1 Velocity Analysis**

```bash
# Completed issues in sprint
gh issue list --state closed --json number,title,closedAt,labels,milestone --limit 100

# Previous sprint(s) for comparison
gh issue list --state closed --json number,closedAt,milestone --limit 200
```

Calculate: planned vs actual completion rate, trend across recent sprints.

**B.2 Quality Metrics**

```bash
# PR statistics: review rounds, rejections
gh pr list --state merged --json number,title,reviews,commits,additions,deletions --limit 100

# Bug density: bugs opened during sprint
gh issue list --state all --label "bug" --json number,title,createdAt,state --limit 50

# Test file changes
git log --since="[sprint_start]" --stat -- "**/*test*" "**/*spec*" "**/*.test.*"
```

**B.3 Interaction Churn Analysis**

If Turns-Used data is available in issue metadata, analyze high-churn tickets. Otherwise, approximate from PR review round counts and commit frequency per issue.

Classify root causes:
- **spec**: ambiguous or incomplete specification
- **scope**: task too large for single context window
- **technical**: unexpected technical complexity or dependency

**B.4 Past Action Item Tracking**

Search for previous retro action items:
```bash
# Look for retro-related issues or labels
gh issue list --state all --label "retro-action" --json number,title,state --limit 20
```

If no structured tracking, note the absence and recommend establishing it.

**Generate Before Report**:

```markdown
## Sprint Diagnostic Report - [Sprint/Date Range]

### Velocity
- Planned: [N] issues | Completed: [N] | Rate: [%]
- Previous sprint: [N] completed | Trend: [improving/stable/declining]

### Quality Metrics
- PRs merged: [N] | Avg review rounds: [N]
- PRs with rejection: [N]/[total] ([%])
- Bugs opened this sprint: [N] | Bug density trend: [up/down/stable]
- Test file changes: +[N]/-[N]

### Interaction Churn Analysis
| Issue | Indicator | Root Cause | Category |
|---|---|---|---|
| #N | [N] review rounds / [N] commits | [description] | [spec/scope/technical] |

Most common root cause: [category] ([N]/[total])
Pattern: [summary of recurring issues]

### Past Action Items
| Action | Source | Status | Impact |
|---|---|---|---|
| [action] | [retro date] | [done/in-progress/not started] | [observed effect] |

Completion rate: [N]/[total] ([%])
```

### During: Discussion Agenda

Generate a structured agenda for the human-led discussion:

```markdown
## Retrospective Discussion Agenda

### 1. AI Effectiveness Review
- Tasks where AI excelled: [list from high-velocity, low-churn items]
- Tasks where AI struggled: [list from high-churn, high-rejection items]
- Prompt: "Did AI help or hinder this sprint overall?"

### 2. Spec Quality Review
- High-churn items: [list with identified root causes]
- Recommended improvements to spec writing process: [specific suggestions]

### 3. CLAUDE.md / SKILL Improvement Proposals
Based on recurring patterns:
- [Pattern]: suggest adding to CLAUDE.md / SKILL
- [Pattern]: suggest modifying existing rule

### 4. Past Actions Review
- Completed actions and their observed impact
- Outstanding actions and blockers

### 5. New Action Items
[Capture during session]
```

### After: Action Item Template

```markdown
## Action Items - [Date]

| # | Action | Owner | Target Sprint | Auto-ticket |
|---|---|---|---|---|
| 1 | [specific, measurable action] | [person] | [sprint] | `gh issue create --title "[action]" --label "retro-action"` |
```

Offer to auto-create issues for agreed action items using `gh issue create`.
