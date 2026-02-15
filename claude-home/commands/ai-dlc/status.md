---
description: "Display AI-DLC lifecycle health dashboard with sprint progress, spec quality, ceremony tracking, DORA Four Keys, and AI effectiveness metrics"
---

## Instructions

Display a comprehensive AI-DLC lifecycle health dashboard. Load `ai-dlc-ceremonies` skill for ceremony knowledge and `ticket-management` skill for Atomic Spec / Agent Loop details.

Dashboard scope: `$ARGUMENTS` (default: current sprint / last 7 days)

### 0. Board Field Integration (if available)

If the project's CLAUDE.md contains `<github-project>` XML tags, read board-level fields for richer data:

```bash
# Parse project config from CLAUDE.md
PROJECT_NUMBER=$(grep -oP 'url="https://github.com/(?:users|orgs)/[^/]+/projects/\K\d+' CLAUDE.md | head -1)
OWNER=$(grep -oP 'url="https://github.com/(?:users|orgs)/\K[^/"]+' CLAUDE.md | head -1)

# Fetch all items with field values
gh project item-list "$PROJECT_NUMBER" --owner "$OWNER" --format json --limit 200
```

Extract and display board metrics where available:

```markdown
## Board Metrics
| Metric | Value | Source |
|---|---|---|
| Status Distribution | Todo:[N] In Progress:[N] Review:[N] Done:[N] | Board |
| Avg AI-Confidence | [N] | Board field |
| Total Turns-Used | [N] | Board field |
| Items with Spec-Link | [N]/[total] | Board field |
| Review Queue | [N] items in Review | Board field |
```

If `<github-project>` tags are not found, skip this section and proceed with Issue-based analysis.

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

### 5. DORA Four Keys

Load `ai-dlc-observability` skill for metrics definitions and thresholds.

Run the sprint aggregation script:

```bash
# Auto-detect team scale from CLAUDE.md (fallback: solo)
TEAM_SCALE=$(grep -oP '<github-project[^>]*scale="\K[^"]+' CLAUDE.md 2>/dev/null || echo "solo")

python3 ~/.claude/skills/ai-dlc-observability/scripts/aggregate-sprint.py --since "[sprint_start]" --until "[today]" --project-dir "$CLAUDE_PROJECT_DIR" --team-size "$TEAM_SCALE"
```

If the script succeeds, parse the JSON output and display DORA metrics:

```markdown
## DORA Four Keys
| Metric | Value | Level | Details |
|---|---|---|---|
| VDF (Delivery Frequency) | [N]/day | [LEVEL] | [qualified]/[total] PRs in [N] days |
| SVLT (Lead Time) | [N]h | [LEVEL] | Cognitive: [N]h, Verify: [N]h |
| Rework Rate | [N]% | [LEVEL] | [N] high-churn files / [N] total |
| TTC (Time to Correct) | [N]h | [LEVEL] | [N] bugs resolved |
```

If the script fails or is not available, skip this section with a note:
"DORA metrics unavailable — run `python3 ~/.claude/skills/ai-dlc-observability/scripts/aggregate-sprint.py` to generate"

### 6. AI Effectiveness

From the same script output, display:

```markdown
## AI Effectiveness
### AI-Confidence: [value] ([LEVEL])
| Component | Score | Weight |
|---|---|---|
| Spec Quality (SQ) | [N] | 0.30 |
| Churn Inverse (CI) | [N] | 0.25 |
| Turns/Resolution (TPR) | [N] | 0.25 |
| Session Efficiency (SE) | [N] | 0.20 |

### MTTV
- Macro (PR cycle): [N]h median
- Micro (per turn): [N]s

### High Churn Files
- [file] ([N] edits)
```

### 7. Economics

If the script output `economics.available` is true, display cost metrics:

```markdown
## Economics
| Metric | Value |
|---|---|
| Tokens/Commit | [N] |
| Cost/PR | $[N] |
| Cost/Issue | $[N] |
```

If `economics.available` is false, display:
"Economics data requires OTel — set `CLAUDE_CODE_ENABLE_TELEMETRY=1` to enable"

### 8. Overall Health Score

Aggregate into a single health indicator:

```markdown
## Lifecycle Health: [Healthy / Attention / Critical]

| Dimension | Score | Details |
|---|---|---|
| Sprint Progress | [emoji] | [%] completion |
| Spec Quality | [emoji] | [N.N]/5 average |
| Blockers | [emoji] | [N] active |
| Churn Risk | [emoji] | [N] issues |
| Stale Issues | [emoji] | [N] issues |
| DORA Level | [emoji] | VDF/SVLT/Rework/TTC overall |
| AI-Confidence | [emoji] | [value] ([LEVEL]) |
| Sprint Health | [emoji] | [value] from aggregate-sprint.py |
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
