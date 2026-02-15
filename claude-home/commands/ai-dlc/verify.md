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

### Step 6: Record Metrics to Board (if available)

If the project's CLAUDE.md contains `<github-project>` XML tags, write verification metrics back to board fields:

```bash
PROJECT_NUMBER=$(grep -oP 'url="https://github.com/(?:users|orgs)/[^/]+/projects/\K\d+' CLAUDE.md | head -1)
OWNER=$(grep -oP 'url="https://github.com/(?:users|orgs)/\K[^/"]+' CLAUDE.md | head -1)
PROJECT_ID=$(grep -oP '<github-project id="\K[^"]+' CLAUDE.md | head -1)
```

For each completed issue with a matching board item:

| Field | Value | Source |
|---|---|---|
| MTTV-Hours | PR creation to merge time (hours) | PR `createdAt` vs `mergedAt` |
| Rework-Count | Number of review round-trips | PR review `CHANGES_REQUESTED` count |
| AI-Confidence | Final AI-Confidence score | aggregate-sprint.py output |

```bash
# Write MTTV-Hours (1 second between mutations)
gh project item-edit --project-id "$PROJECT_ID" --id "$ITEM_ID" \
  --field-id "$MTTV_HOURS_FIELD_ID" --number "$MTTV_HOURS"

sleep 1

# Write Rework-Count
gh project item-edit --project-id "$PROJECT_ID" --id "$ITEM_ID" \
  --field-id "$REWORK_COUNT_FIELD_ID" --number "$REWORK_COUNT"
```

Skip if `<github-project>` tags not found or fields (MTTV-Hours, Rework-Count) don't exist in the board.

### Interactive Follow-up

Offer to:
- Deep-dive into specific orphan PRs or high-rejection items
- Generate Outcome Done verification checklist for unverified items
- Compare trends against previous sprints
