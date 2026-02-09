---
description: "Manage sprints, workload, and milestones via GitHub Projects V2"
---

## Instructions

Delegate to the `github-project-board` agent with the user's context:

```
Task tool:
  subagent_type: github-project-board
  prompt: |
    $ARGUMENTS

    Focus areas: sprint setup (Iteration fields), workload distribution,
    custom field configuration, progress reporting (burndown, velocity).
```

The agent has the `github-projects-v2` skill auto-loaded.
