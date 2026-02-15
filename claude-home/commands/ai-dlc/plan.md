---
description: "Facilitate AI-native sprint pre-planning with 3-phase model"
---

## Instructions

Execute AI-DLC 3-phase sprint planning. Load `ai-dlc-ceremonies` skill for ceremony knowledge and `ticket-management` skill for Atomic Spec / Agent Loop details.

Sprint context: `$ARGUMENTS`

### Phase 1: AI Pre-Planning (Automated Analysis)

Collect and analyze data to prepare a pre-planning report.

**1.1 Backlog Spec Quality Check**

Fetch open issues that are sprint candidates:

```bash
gh issue list --state open --json number,title,body,labels,milestone --limit 100
```

**Upstream pipeline fallback**: If few or no issues are found, also check for upstream story files using Glob `docs/stories-*.md` and `docs/spec-*.md`. Read and assess stories from these files using the same 5-element quality check below. This ensures the pipeline works even when stories have not yet been converted to GitHub Issues.

For each issue (or story from file), check Atomic Spec 5 elements (Context, Current Behavior, Expected Behavior, Constraints, Verification). Report completeness as a quality score.

**1.2 Dependency Detection**

Analyze labels, milestones, and issue body for cross-references:

```bash
gh issue list --state open --json number,title,milestone,labels,body --limit 100
```

Build a dependency map: which issues block others.

**1.3 Velocity Estimation**

Calculate completion pace from recent history:

```bash
gh issue list --state closed --json number,closedAt,labels --limit 100
git log --since="30 days ago" --merges --format="%h|%ad|%s" --date=short
```

Compute: issues closed per week, avg PR merge rate.

**1.4 Capacity Calculation**

If team size / sprint length is provided in `$ARGUMENTS`, calculate available person-days. Otherwise, estimate from git contributor data:

```bash
git shortlog --since="30 days ago" -s --all
```

**1.5 Generate Pre-Planning Report**

```markdown
## Sprint Pre-Planning Report

### Spec Quality
| Issue | 5-Element Score | Missing Elements | Action |
|---|---|---|---|
| #N | [0-5]/5 | [list] | [ready / needs spec revision] |

Issues ready: [N] | Needs revision: [N]

### Scope Estimate
- Recent velocity: [N] issues/week
- Available capacity: [N] person-days (if provided)
- Recommended scope: [N] issues for [sprint length]

### Dependencies & Blockers
- #N blocks #N: [reason]

### Risks
- [risk]: [mitigation suggestion]
```

### Phase 2: Human Strategy Briefing

Present analysis results as structured decision points for human review:

```markdown
## Decision Points for Sprint Planning

1. **Scope**: Analysis recommends [N] issues. Approve or adjust?
   - High-confidence items: [list]
   - Items needing spec revision first: [list]

2. **AI vs Human Task Assignment**
   - Suitable for AI agents: [list with rationale]
   - Requires human implementation: [list with rationale]

3. **Verification Budget**
   - Recommended review time: [estimate] for [N] AI-generated PRs
   - High-risk items requiring senior review: [list]

4. **Sprint Length**
   - Pod (1-3 days) / Squad (3-5 days) / Enterprise (1 week)
   - Recommendation based on team scale: [suggestion]
```

### Phase 3: Delivery Preparation

After human approval, outline the dispatch plan:

```markdown
## Delivery Plan

### Agent-Ready Tickets
| Issue | Assignee | Status Transition | Priority |
|---|---|---|---|
| #N | [AI/Human] | Triage → Spec Definition | [P0/P1/P2] |

### Next Steps
1. Ensure all agent-assigned tickets have complete Atomic Specs
2. Set sprint milestone on approved issues
3. Begin Agent Loop: Triage → Spec Definition → AI Planning
```

### Phase 4: Board Status Update (if available)

If the project's CLAUDE.md contains `<github-project>` XML tags, update board fields after human approval:

```bash
# Parse project config from CLAUDE.md
PROJECT_NUMBER=$(grep -oP 'url="https://github.com/(?:users|orgs)/[^/]+/projects/\K\d+' CLAUDE.md | head -1)
OWNER=$(grep -oP 'url="https://github.com/(?:users|orgs)/\K[^/"]+' CLAUDE.md | head -1)
PROJECT_ID=$(grep -oP '<github-project id="\K[^"]+' CLAUDE.md | head -1)
```

For each approved agent-ready ticket:
1. Update Status to "In Progress" (Solo/Pod) or "Ready" (Squad+)
2. Set Iteration to current sprint (if Iteration field exists)

```bash
# Update Status
gh project item-edit --project-id "$PROJECT_ID" --id "$ITEM_ID" \
  --field-id "$STATUS_FIELD_ID" --single-select-option-id "$TARGET_STATUS_ID"

# Set Iteration (if available)
gh project item-edit --project-id "$PROJECT_ID" --id "$ITEM_ID" \
  --field-id "$ITERATION_FIELD_ID" --iteration-id "$CURRENT_ITERATION_ID"
```

Rate limit: 1 second between mutations. Skip if `<github-project>` tags not found.

### Interactive Refinement

After presenting results, offer to:
- Adjust scope up/down
- Re-classify AI vs human assignments
- Break down large tickets into Atomic Spec-sized chunks
- Generate detailed Atomic Specs for incomplete issues
