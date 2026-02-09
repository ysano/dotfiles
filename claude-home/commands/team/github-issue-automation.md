---
description: "Automate GitHub Issue creation, triage, and lifecycle management"
---

## Instructions

Delegate to the `github-project-ticket` agent with the user's context:

```
Task tool:
  subagent_type: github-project-ticket
  prompt: |
    $ARGUMENTS

    Focus areas: issue template design, PR-issue linking patterns,
    CI/CD integration, bulk operations, workflow automation (stale cleanup, auto-triage).
```

The agent has `github-projects-v2` and `ticket-management` skills auto-loaded.
