---
description: "Verify sprint outcomes with AI output quality assessment"
---

## Prerequisites

Before running, verify:
1. `aggregate-sprint.py` is accessible: `ls ~/.claude/skills/ai-dlc-observability/scripts/aggregate-sprint.py`
2. `gh` CLI is authenticated: `gh auth status`

If prerequisites fail, inform the user and suggest: `pip install -r requirements.txt` or `gh auth login`.

## Instructions

Generate an AI-DLC sprint verification report. Load `ai-dlc-ceremonies` skill for verification session patterns. Load `ticket-management` skill for DoD / quality metrics context. Load `ai-dlc-observability` skill for MTTV / AI-Confidence / Sprint Health metrics.

Sprint range: `$ARGUMENTS`

If no arguments provided, auto-detect the current sprint from the most recent milestone or use the last 7 days.

### Step 1: Collect Sprint Deliverables

```bash
# Merged PRs in sprint period
gh pr list --state merged --json number,title,body,mergedAt,author,reviews,additions,deletions --limit 100

# Completed issues
gh issue list --state closed --json number,title,closedAt,labels,body,milestone --limit 100
```

### Step 2: Traceability Mapping

For each merged PR, extract linked issue numbers from PR body (patterns: `Fixes #N`, `Closes #N`, `Resolves #N`, `#N`).

Map: PR → Issue → Sprint Goal alignment.

Flag orphan PRs (no linked issue) and orphan issues (closed without PR).

### Step 3: Quality Assessment (ENHANCED)

**Review Statistics**:
- Average review rounds per PR
- PRs with initial rejection (changes_requested)
- Time from PR creation to merge (MTTV)

**Test Coverage** (if available):
```bash
# Approximate from test file changes
git log --since="[sprint_start]" --stat -- "**/*test*" "**/*spec*" "**/*.test.*"
```

Run sprint aggregation:

```bash
python3 ~/.claude/skills/ai-dlc-observability/scripts/aggregate-sprint.py \
  --since "[sprint_start]" --until "[today]" --project-dir "$CLAUDE_PROJECT_DIR"
```

**Quantitative Metrics** (from script output):

| Metric | Value | Level |
|---|---|---|
| MTTV Macro (PR cycle) | [N]h | — |
| MTTV Micro (per turn) | [N]s | — |
| AI-Confidence | [N] | [LEVEL] |
| Sprint Health | [N] | [LEVEL] |
| Rework Rate | [N]% | [LEVEL] |

Merge with existing review statistics and test coverage analysis.

**AI-Confidence Components**:

| Component | Score | Implication |
|---|---|---|
| Spec Quality | [N] | [Higher = specs well-defined] |
| Churn Inverse | [N] | [Higher = less rework] |
| Turns/Resolution | [N] | [Higher = efficient sessions] |
| Session Efficiency | [N] | [Higher = more productive tool use] |

### Step 4: Output Done vs Outcome Done Gap

For each completed issue:
- **Output Done**: PR merged, tests pass, review approved → check from PR data
- **Outcome Done**: User impact verified → check for tags like `validated`, `measured`, or note as `unverified`

### Step 5: Generate Verification Report

```markdown
## Sprint Verification Report - [Sprint Name/Date Range]

### Deliverables Summary
- PRs merged: [N] (+[additions] / -[deletions] lines)
- Issues completed: [N]/[planned]
- Sprint goal: [assessment]

### Traceability
| PR | Linked Issue | Sprint Goal | Status |
|---|---|---|---|
| #N | #N | [goal] | linked |

- Orphan PRs (no issue): [count] — [list]
- Orphan Issues (no PR): [count] — [list]

### Quality Assessment
| Metric | Value | Trend |
|---|---|---|
| Avg review rounds | [N] | [vs previous] |
| PRs with rejection | [N]/[total] ([%]) | [vs previous] |
| Avg MTTV | [N]h | [vs previous] |
| Test file changes | +[N]/-[N] | - |
| MTTV (PR cycle) | [N]h | [vs previous sprint] |
| AI-Confidence | [N] ([LEVEL]) | [vs previous sprint] |
| Sprint Health | [N] ([LEVEL]) | [vs previous sprint] |

### Output Done vs Outcome Done
| Issue | Output Done | Outcome Done | Gap |
|---|---|---|---|
| #N | [yes] | [verified/unverified] | [description if gap] |

- Output-only items (no outcome verification): [count]/[total]

### Recommendations
- [Specific recommendations based on analysis]
- [Suggested focus areas for next sprint]
```

### Interactive Follow-up

Offer to:
- Deep-dive into specific orphan PRs or high-rejection items
- Generate Outcome Done verification checklist for unverified items
- Compare trends against previous sprints
