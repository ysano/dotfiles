---
description: "Triage and prioritize issues effectively"
---

## Instructions

Analyze GitHub Issues and route them with appropriate categorization, priority, and assignment. Use `gh` CLI for all GitHub operations.

Issue to triage: `$ARGUMENTS`

For triage methodology and ticket quality standards, load the `ticket-management` skill.

### Triage Process

1. **Analyze Issue Content**
   - Parse title, body, and labels for category signals (bug, feature, security, performance)
   - Identify affected components from file paths and keywords
   - Assess user impact from description and reporter context

2. **Calculate Priority**
   - **Critical**: Security vulnerabilities, data loss, service outage
   - **High**: Major feature broken, significant user impact, regression
   - **Medium**: Minor feature issue, workaround available
   - **Low**: Enhancement, cosmetic, documentation

3. **Detect Duplicates**
   - Search existing issues for similar titles/labels via `gh issue list --search "keyword"`
   - Link related issues with cross-references

4. **Assign and Label**
   - Route by component area (frontend, backend, infra, docs)
   - Set priority label and milestone
   - Assign to appropriate team member based on component ownership

5. **Generate Triage Report**

```
## Triage: #[number] [title]
- Category: [bug/feature/security/performance]
- Priority: [critical/high/medium/low]
- Component: [area]
- Assignee: [suggestion]
- Duplicates: [none / #N]
- Action: [assign/close-duplicate/needs-info/escalate]
```

For batch triage, process untriaged issues (`gh issue list --label "needs-triage"`) and generate a summary table.
