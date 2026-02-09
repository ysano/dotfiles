---
description: "Generate accurate project time estimates"
---

## Instructions

Analyze past commits, PR completion times, code complexity, and team velocity to generate data-backed task estimates.

Estimate: `$ARGUMENTS`

For estimation methodology and ticket decomposition, load the `ticket-management` skill.

### Steps

1. **Gather Historical Data**
   - Run `git log --since="6 months ago"` to analyze commit frequency and patterns
   - Check PR merge times via `gh pr list --state merged`
   - If project management data is available (GitHub Projects, Linear), collect past estimate accuracy

2. **Analyze Task Complexity**
   - Identify affected files and modules from the task description
   - Assess code complexity (coupling, test coverage, dependencies)
   - Compare with similar past tasks

3. **Generate Estimate**

Output format:

```
## Estimate: [Task Description]

### Complexity Assessment
- Files affected: [count]
- Dependencies: [list]
- Risk factors: [list]

### Estimate (with confidence intervals)
| Scenario | Duration | Confidence |
|----------|----------|-----------|
| Optimistic | [time] | 20th percentile |
| Most Likely | [time] | 50th percentile |
| Pessimistic | [time] | 80th percentile |

### Basis
- Similar past tasks: [references]
- Key assumptions: [list]
- Risk adjustments: [list]
```
