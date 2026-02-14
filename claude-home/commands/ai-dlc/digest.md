---
description: "Generate async AI digest with human judgment agenda"
---

## Instructions

Generate an AI-DLC 2-layer daily digest. Load `ai-dlc-ceremonies` skill for async digest patterns. Load `ticket-management` skill for Agent Loop / Janitor rules context. Load `ai-dlc-observability` skill for session metrics and churn data context.

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

**1.4 Blocker Detection (ENHANCED)**

Identify:
- **Stale PRs**: open > 48 hours without review
- **Long-untouched issues**: assigned but no activity > 2 days
- **4-hour rule violations**: if timestamps available, flag AI Implementation items stalled > 4h

**Churn alerts** (from churn cache):

```bash
# Read churn cache directly
python3 -c "
import json, os, glob
cache_dir = '/tmp/claude-churn-cache'
for f in glob.glob(os.path.join(cache_dir, '*_churn.json')):
    data = json.load(open(f))
    for path, info in data.items():
        if isinstance(info, dict) and info.get('count', 0) >= 3:
            print(f'CHURN: {path} ({info[\"count\"]} edits)')
"
```

**Session activity** (from sessions.jsonl):

```bash
# Recent session summary
python3 -c "
import json, os
from datetime import datetime, timezone, timedelta
sessions_file = os.path.expanduser('~/.claude/metrics/sessions.jsonl')
if os.path.isfile(sessions_file):
    cutoff = (datetime.now(timezone.utc) - timedelta(hours=24)).isoformat()
    sessions = []
    for line in open(sessions_file):
        s = json.loads(line.strip())
        if s.get('start_time', '') >= cutoff:
            sessions.append(s)
    total_turns = sum(s.get('total_turns', 0) for s in sessions)
    print(f'Sessions (24h): {len(sessions)}, Total turns: {total_turns}')
    if sessions:
        avg = total_turns / len(sessions)
        print(f'Avg turns/session: {avg:.0f}' + (' ⚠️ HIGH' if avg > 25 else ''))
"
```

Display churn alerts alongside existing blocker detection (stale PRs, long-untouched issues).

**1.5 Review Queue**

```bash
gh pr list --state open --json number,title,author,createdAt,reviewDecision --sort created
```

Age-sort pending reviews, highlight bottlenecks.

**1.6 AI Activity Summary**

From sessions.jsonl (last 24h):
- Sessions: [N] | Total turns: [N] | Files modified: [N]
- Top tools: Edit [N], Read [N], Bash [N]
- High-churn files: [list from churn cache]

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

### AI Activity (24h)
- Sessions: [N] | Total turns: [N] | Avg turns/session: [N]
- High-churn files: [list]

### Blockers & Alerts
| Type | Item | Detail | Age |
|---|---|---|---|
| Stale PR | #N | No review | [N]h |
| 4h violation | #N | AI impl stalled | [N]h |
| File churn | [path] | [N] edits | - |
| High turns | session | avg [N] turns | - |

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
