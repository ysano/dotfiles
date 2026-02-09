---
description: "Generate daily standup reports from git and project data"
---

## Instructions

Generate a standup report by analyzing recent activity. Default time range: last 24 hours.

Time range or format: `$ARGUMENTS`

### Data Collection

1. **Git Activity**: `git log --since="24 hours ago" --all --format="%h|%an|%ad|%s" --date=short`
2. **Branch Activity**: `git for-each-ref --format='%(refname:short)|%(committerdate:short)' --sort=-committerdate refs/heads/`
3. **PR Status**: `gh pr list --author "$(gh api user --jq .login)" --state all` for open/merged/reviewed PRs
4. **Project Data**: If GitHub Projects or Linear is available, fetch task status changes

### Report Format

```
## Standup - [Date]

### Completed
- [Task/PR with link]: [Brief description]

### In Progress
- [Task/PR]: [Status, % complete, next step]

### Blockers
- [Issue]: [What's needed to unblock]

### Metrics
- Commits: [N] | PRs: [open/merged/reviewed] | Tasks completed: [N]
```

### Output Variants

If the user requests a specific format:
- **Slack**: Use bullet points with bold headers, `*Yesterday:*` / `*Today:*` / `*Blockers:*`
- **Team rollup**: Consolidate all team members into a single summary with per-person sections
- **Extended range**: Adjust `--since` parameter for custom time ranges
