---
description: "Generate async AI digest with human judgment agenda"
---

## Instructions

Generate an AI-DLC 2-layer daily digest. Load `ai-dlc-ceremonies` skill for async digest patterns. Load `ticket-management` skill for Agent Loop / Janitor rules context.

Options: `$ARGUMENTS`

Accepted arguments:
- Time range: `24h` (default), `48h`, `1w`, `since:YYYY-MM-DD`
- Format: `markdown` (default), `slack`
- Scope: `all` (default), `mine` (current git user only)

### Layer 1: AI Digest

**1.1 Commit Summary**

```bash
git log --since="24 hours ago" --all --format="%h|%an|%ad|%s" --date=short
```

Group by author, summarize changes.

**1.2 PR Status**

```bash
gh pr list --state open --json number,title,author,createdAt,reviewDecision,isDraft
gh pr list --state merged --json number,title,mergedAt --limit 20
```

Categorize: pending review, approved, changes requested, draft, merged.

**1.3 Sprint Progress**

```bash
gh issue list --state open --json number,title,labels,milestone --limit 100
gh issue list --state closed --json number,title,closedAt,milestone --limit 50
```

Calculate: completed vs total in current milestone, velocity trend.

**1.4 Blocker Detection**

Identify:
- **Stale PRs**: open > 48 hours without review
- **Long-untouched issues**: assigned but no activity > 2 days
- **4-hour rule violations**: if timestamps available, flag AI Implementation items stalled > 4h
- **Churn alerts**: if Turns-Used data available, flag items > 3 turns

**1.5 Review Queue**

```bash
gh pr list --state open --json number,title,author,createdAt,reviewDecision --sort created
```

Age-sort pending reviews, highlight bottlenecks.

**Generate Layer 1 Output**:

```markdown
## AI Digest - [Date]

### Activity Summary
- Commits: [N] by [N] authors
- PRs merged: [N] | Opened: [N] | Pending review: [N]

### Sprint Progress
- Milestone: [name]
- Completed: [N]/[total] ([%])
- Velocity trend: [on track / behind / ahead]

### Blockers & Alerts
| Type | Item | Detail | Age |
|---|---|---|---|
| Stale PR | #N | No review | [N]h |
| 4h violation | #N | AI impl stalled | [N]h |
| Churn alert | #N | [N] turns | - |

### Review Queue
| PR | Author | Waiting | Status |
|---|---|---|---|
| #N | @user | [N]h | [pending/changes requested] |
```

### Layer 2: Human Judgment Agenda

Based on Layer 1 analysis, generate the decision agenda:

```markdown
### Human Decision Agenda

**Blockers Requiring Human Judgment**
- [item]: [context, options, recommended action]

**Escalations**
- [ticket]: [reason] → [suggested action]

**Cross-Team Coordination**
- [dependency]: [parties, action needed]

**No items** — if nothing requires human judgment, explicitly state: "No human decisions needed today."
```

### Output Variants

- **Markdown** (default): Full report as above
- **Slack**: Compact single-message format with `*bold headers*` and bullet points. Omit tables, use inline formatting
