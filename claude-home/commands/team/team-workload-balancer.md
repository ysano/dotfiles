---
description: "Balance team workload distribution"
---

## Instructions

Analyze team workload and suggest optimal task distribution. Use `gh` CLI and project management data.

Team or sprint to analyze: `$ARGUMENTS`

For GitHub Projects V2 data queries, load the `github-projects-v2` skill.

### Steps

1. **Collect Workload Data**
   - Current task assignments per team member (via `gh project item-list` or `gh issue list --assignee`)
   - Task estimates/story points if available
   - PR review load per person
   - Blocked vs active tasks ratio

2. **Calculate Workload Score**
   Per team member:
   - Active task count and total points
   - Review obligations
   - Blocked tasks (unusable capacity)
   - Historical velocity (tasks completed per sprint)

3. **Identify Imbalances**
   - Overloaded members (>110% average load)
   - Underutilized members (<70% average load)
   - Skill-task mismatches
   - Single points of failure (one person blocking multiple tasks)

4. **Generate Recommendations**

```
## Workload Analysis

### Current Distribution
| Member | Active Tasks | Points | Reviews | Load % |
|--------|-------------|--------|---------|--------|
| [name] | [N] | [pts] | [N] | [%] |

### Imbalances
- [Member] is overloaded: [N] points vs [avg] average
- [Member] has capacity: [N] points available

### Recommended Rebalancing
1. Move [Task] from [Member A] to [Member B] — [reason]
2. Reassign review of PR #[N] to [Member C] — [reason]

### Risk Flags
- [Single point of failure / bottleneck description]
```
