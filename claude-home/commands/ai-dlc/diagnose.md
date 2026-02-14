---
description: "Generate data-driven sprint diagnostic for retrospective"
---

## Instructions

Generate an AI-DLC data-driven diagnostic report for a retrospective session. Load `ai-dlc-ceremonies` skill for diagnostic session patterns. Load `ticket-management` skill for Churn / AI-Confidence / Janitor rules context. Load `ai-dlc-observability` skill for DORA Four Keys / Sprint Health metrics context.

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

**B.2 Quality Metrics (EXTENDED)**

```bash
# PR statistics: review rounds, rejections
gh pr list --state merged --json number,title,reviews,commits,additions,deletions --limit 100

# Bug density: bugs opened during sprint
gh issue list --state all --label "bug" --json number,title,createdAt,state --limit 50

# Test file changes
git log --since="[sprint_start]" --stat -- "**/*test*" "**/*spec*" "**/*.test.*"
```

Run sprint aggregation for quantitative data:

```bash
python3 ~/.claude/skills/ai-dlc-observability/scripts/aggregate-sprint.py \
  --since "[sprint_start]" --until "[today]" --project-dir "$CLAUDE_PROJECT_DIR"
```

From the script output, extract and display:
- DORA Four Keys (VDF/SVLT/Rework/TTC) with levels
- AI-Confidence breakdown (4 components)
- MTTV Macro/Micro

Merge with existing PR/bug/test analysis from GitHub CLI.

**B.3 Interaction Churn Analysis (ENHANCED)**

**File-level churn** (from aggregate-sprint.py alerts):
Display all files with 3+ edits from the script output `alerts` array.
Classify root causes: spec (ambiguous spec), scope (task too large), technical (unexpected complexity).

**Session-level analysis** (from script output `activity`):
- Total sessions: [N], Total turns: [N]
- Avg turns/session: [N] (ideal: ≤10, concern: >25)
- Top tools: [tool distribution] — high Read/Grep ratio may indicate exploration churn

If sessions show avg_turns > 25, flag as "High interaction churn" and investigate root causes.

Additionally, approximate from PR review round counts and commit frequency per issue for items not tracked in session data.

**B.4 Past Action Item Tracking**

Search for previous retro action items:
```bash
# Look for retro-related issues or labels
gh issue list --state all --label "retro-action" --json number,title,state --limit 20
```

If no structured tracking, note the absence and recommend establishing it.

**B.5 Sprint Comparison**

Read previous sprint data from `~/.claude/metrics/sprints.jsonl`.
Compare current sprint metrics vs previous:

| Metric | Previous | Current | Trend |
|---|---|---|---|
| VDF | [N] | [N] | [improving/declining] |
| SVLT | [N]h | [N]h | [improving/declining] |
| Rework Rate | [N]% | [N]% | [improving/declining] |
| AI-Confidence | [N] | [N] | [improving/declining] |
| Sprint Health | [N] | [N] | [improving/declining] |

If no previous sprint data exists, note: "First sprint with observability — baseline established."

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

### DORA Four Keys
| Metric | Value | Level |
|---|---|---|
| VDF (Value Delivery Frequency) | [N] | [LEVEL] |
| SVLT (Spec-to-Value Lead Time) | [N]h | [LEVEL] |
| Rework Rate | [N]% | [LEVEL] |
| TTC (Time to Correct) | [N]h | [LEVEL] |

### AI-Confidence
| Component | Score | Weight |
|---|---|---|
| Spec Quality (SQ) | [N] | 0.30 |
| Churn Inverse (CI) | [N] | 0.25 |
| Turns/Resolution (TPR) | [N] | 0.25 |
| Session Efficiency (SE) | [N] | 0.20 |
| **Composite** | **[N]** | — |

### Interaction Churn Analysis
| Issue/File | Indicator | Root Cause | Category |
|---|---|---|---|
| [file/issue] | [N] edits / [N] review rounds | [description] | [spec/scope/technical] |

Session activity: [N] sessions, [N] total turns, avg [N] turns/session
Most common root cause: [category] ([N]/[total])
Pattern: [summary of recurring issues]

### Sprint Comparison
| Metric | Previous | Current | Trend |
|---|---|---|---|
| VDF | [N] | [N] | [improving/declining] |
| SVLT | [N]h | [N]h | [improving/declining] |
| Rework Rate | [N]% | [N]% | [improving/declining] |
| AI-Confidence | [N] | [N] | [improving/declining] |
| Sprint Health | [N] | [N] | [improving/declining] |

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
