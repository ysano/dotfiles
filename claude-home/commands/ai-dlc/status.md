---
description: "Display AI-DLC lifecycle health dashboard with sprint progress, spec quality, and ceremony tracking"
---

## Instructions

Display a comprehensive AI-DLC lifecycle health dashboard. Load `ai-dlc-ceremonies` skill for ceremony knowledge and `ticket-management` skill for Atomic Spec / Agent Loop details.

Dashboard scope: `$ARGUMENTS` (default: current sprint / last 7 days)

### 1. Sprint Progress

Fetch current sprint status from GitHub Issues milestone:

```bash
gh issue list --state all --json number,title,state,labels,milestone,closedAt,createdAt --limit 200
```

Calculate:
- Open / In Progress / Closed counts
- Completion rate (%)
- Burndown trend (issues closed per day over sprint period)

```markdown
## Sprint Progress
- Milestone: [name] ([start] - [end])
- Progress: [N]/[total] ([%]) ████████░░
- Open: [N] | In Progress: [N] | Closed: [N]
- Daily close rate: [N]/day (target: [N]/day)
```

### 2. Spec Quality Distribution

For all open issues in the current sprint, check Atomic Spec 5 elements (Context, Current Behavior, Expected Behavior, Constraints, Verification):

```bash
gh issue list --state open --milestone "[current]" --json number,title,body --limit 100
```

Score each issue 0-5 and classify:

```markdown
## Spec Quality
| Score | Count | Issues |
|---|---|---|
| 5/5 (Ready) | [N] | #1, #2 |
| 3-4/5 (Needs Work) | [N] | #3, #4 |
| 0-2/5 (Incomplete) | [N] | #5 |

Average score: [N.N]/5
```

### 3. Blocker / Churn / Stale Detection

**Blockers**: Issues with `blocked` label or body containing "blocked by":

```bash
gh issue list --state open --label "blocked" --json number,title
```

**Churn**: Issues with high comment count or frequent reopening (proxy for interaction churn):

```bash
gh issue list --state open --json number,title,comments --limit 100
```

Flag issues with > 10 comments as potential churn.

**Stale**: Open issues with no activity in 30+ days (AI Janitor rule):

```bash
gh issue list --state open --json number,title,updatedAt --limit 200
```

```markdown
## Health Signals
### Blockers ([N])
- #N: [title] - blocked by [reason]

### Churn Risk ([N])
- #N: [title] - [N] comments (threshold: 10)

### Stale ([N]) (30+ days inactive)
- #N: [title] - last activity [date]
```

### 4. Ceremony Tracking

Estimate ceremony execution status from recent git log and command usage patterns:

```bash
git log --since="7 days ago" --all --format="%s" --grep="Sprint\|Plan\|Digest\|Verify\|Diagnose\|Calibrate\|Retro\|Refinement"
```

Also check for spec/story files recently created:

```bash
find docs/ -name "spec-*.md" -o -name "stories-*.md" -o -name "prd-*.md" | head -20
```

```markdown
## Ceremony Status (last 7 days)
| Ceremony | Expected | Evidence | Status |
|---|---|---|---|
| Planning | Sprint start | [commit/file evidence] | Done / Missing |
| Daily Digest | Daily | [evidence] | [N]/[expected] |
| Refinement | Pre-sprint | [evidence] | Done / Missing |
| Verification | Sprint end | [evidence] | Done / Missing |
| Diagnostic | Sprint end | [evidence] | Done / Missing |
| Calibration | Bi-weekly | [evidence] | Done / Missing |
```

### 5. Overall Health Score

Aggregate into a single health indicator:

```markdown
## Lifecycle Health: [Good / Attention / Critical]

| Dimension | Score | Details |
|---|---|---|
| Sprint Progress | [emoji] | [%] completion |
| Spec Quality | [emoji] | [N.N]/5 average |
| Blockers | [emoji] | [N] active |
| Churn Risk | [emoji] | [N] issues |
| Stale Issues | [emoji] | [N] issues |
| Ceremonies | [emoji] | [N]/[expected] completed |

### Recommended Actions
1. [Most impactful action based on data]
2. [Second priority]
3. [Third priority]
```

### Interactive Follow-up

After presenting the dashboard, offer to:
- Drill down into any section
- Run `/ai-dlc:refine` for stale/incomplete issues
- Run `/ai-dlc:plan` if planning is missing
- Run `/ai-dlc:diagnose` for churn analysis
