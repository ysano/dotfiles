---
description: "Analyze team retrospectives for insights"
---

## Instructions

Analyze sprint retrospective data to identify patterns, generate insights, and track action items.

Sprint to analyze: `$ARGUMENTS` (default: most recent sprint)

For project management platform API details, load the relevant skill: `github-projects-v2`, `linear`, or `jira`.

### Steps

1. **Collect Sprint Data**
   - Git activity: `git log --since="2 weeks ago"` for commits, branches, merge frequency
   - PR metrics: review times, merge times, rejection rate via `gh pr list --state merged`
   - If project management is available: planned vs completed items, velocity

2. **Analyze Patterns**
   - **Positive**: What went well (fast cycle times, clean merges, good coverage)
   - **Negative**: What didn't (blocked tasks, long review times, scope creep)
   - **Recurring**: Issues that appear across multiple sprints

3. **Generate Retrospective Report**

```
## Sprint [X] Retrospective Analysis

### Sprint Metrics
- Velocity: [planned vs actual]
- Cycle time: [average]
- PR review time: [average]

### What Went Well
- [Data-backed positive pattern]

### What Needs Improvement
- [Data-backed issue with root cause]

### Action Items (SMART format)
1. [Specific, Measurable, Achievable, Relevant, Time-bound]
   - Owner: [name]
   - Due: [date]
   - Success metric: [measurable outcome]

### Trend (vs previous sprints)
- Velocity: [trending up/down/stable]
- Quality: [defect rate trend]
- Team health: [review participation, blocked time]
```
