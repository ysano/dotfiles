---
description: "Plan and organize sprint workflows"
---

## Instructions

Facilitate sprint planning by analyzing backlog, team capacity, and dependencies.

Sprint details: `$ARGUMENTS`

For platform-specific API details, load the relevant skill: `github-projects-v2`, `linear`, or `jira`.

### Steps

1. **Check Data Sources**
   - GitHub Issues/Projects via `gh` CLI (primary)
   - Linear via MCP if connected (optional)
   - Git history for velocity baseline

2. **Gather Sprint Context**
   - Sprint duration and dates
   - Team members and their availability
   - Previous sprint velocity (from git log or project data)
   - Sprint goals/themes

3. **Analyze Backlog**
   - Fetch prioritized backlog items
   - Map task dependencies
   - Identify blocked items and risks

4. **Generate Sprint Plan**

```
## Sprint [Name] ([Start] - [End])

### Sprint Goal
[1-2 sentence objective]

### Capacity
- Team: [N] members, [N] available days
- Previous velocity: [N] points
- Recommended capacity: [80-85% of total]

### Sprint Backlog
| Priority | Task | Estimate | Assignee | Dependencies |
|----------|------|----------|----------|-------------|
| High | [ID: Title] | [pts] | [name] | [blockers] |
| Medium | [ID: Title] | [pts] | [name] | [blockers] |

### Risks
- [Risk]: [Mitigation]

### Recommendations
- [Specific suggestion based on analysis]
```

5. **Interactive Refinement**: Offer to adjust scope, rebalance assignments, or break down large tasks
