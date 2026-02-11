---
description: "Verify sprint outcomes with AI output quality assessment"
---

## Instructions

Generate an AI-DLC sprint verification report. Load `ai-dlc-ceremonies` skill for verification session patterns. Load `ticket-management` skill for DoD / quality metrics context.

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

### Step 3: Quality Assessment

**Review Statistics**:
- Average review rounds per PR
- PRs with initial rejection (changes_requested)
- Time from PR creation to merge (MTTV)

**Test Coverage** (if available):
```bash
# Approximate from test file changes
git log --since="[sprint_start]" --stat -- "**/*test*" "**/*spec*" "**/*.test.*"
```

**AI-Confidence Distribution** (if project uses this field):
- Collect from issue/PR metadata if available
- Distribution: 90-100, 80-89, 60-79, 0-59

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
