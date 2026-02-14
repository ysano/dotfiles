---
description: "Refine backlog with Atomic Spec quality scoring, stale detection, and priority re-ranking"
---

## Instructions

Execute AI-DLC backlog refinement ceremony. Load `ai-dlc-ceremonies` skill for ceremony knowledge and `ticket-management` skill for Atomic Spec / Agent Loop details.

Refinement scope: `$ARGUMENTS` (default: all open issues without a milestone)

### 1. Fetch Backlog

Gather all open issues that are candidates for refinement:

```bash
gh issue list --state open --json number,title,body,labels,milestone,assignees,createdAt,updatedAt,comments --limit 200
```

Filter to backlog items (no milestone or future milestone). If `$ARGUMENTS` specifies a label or milestone, apply that filter.

### 2. Atomic Spec 5-Element Scoring

For each issue, analyze the body for the 5 elements of an Atomic Spec:

| Element | Detection | Score |
|---|---|---|
| **Context** | Background/context section, "As a..." pattern, project references | 0 or 1 |
| **Current Behavior** | "Currently...", "Current state...", bug description | 0 or 1 |
| **Expected Behavior** | "Should...", "Expected...", acceptance criteria | 0 or 1 |
| **Constraints** | Technical constraints, non-functional requirements, limitations | 0 or 1 |
| **Verification** | Test scenarios, verification steps, "How to verify" | 0 or 1 |

### 3. Stale Detection (AI Janitor 30-Day Rule)

Apply the AI Janitor stale detection rule:
- **Stale**: No updates in 30+ days
- **Warning**: No updates in 14-29 days
- **Active**: Updated within 14 days

Calculate staleness from `updatedAt` field.

### 4. Classification

Classify each issue into one of three categories:

```markdown
## Backlog Refinement Results

### Ready (5/5) - [N] issues
Sprint-ready. All Atomic Spec elements present.

| Issue | Title | Score | Age | Labels |
|---|---|---|---|---|
| #N | [title] | 5/5 | [N]d | [labels] |

### Needs Work (1-4/5) - [N] issues
Require spec completion before sprint inclusion.

| Issue | Title | Score | Missing | Suggestion |
|---|---|---|---|---|
| #N | [title] | [N]/5 | [elements] | [specific improvement] |

### Stale (30+ days) - [N] issues
No activity for 30+ days. Consider closing or reviving.

| Issue | Title | Last Activity | Score | Action |
|---|---|---|---|---|
| #N | [title] | [date] | [N]/5 | Close / Revive / Merge |
```

### 5. Priority Re-ranking Proposal

Based on scoring, staleness, and label analysis, propose a re-ranked priority:

```markdown
## Priority Re-ranking Proposal

### Tier 1: High Priority (Ready + High Impact)
1. #N: [title] - [rationale]

### Tier 2: Quick Wins (Ready + Low Effort)
1. #N: [title] - [rationale]

### Tier 3: Invest (Needs Work + High Value)
1. #N: [title] - [missing elements] → [action]

### Tier 4: Deprioritize / Close
1. #N: [title] - [reason: stale / duplicate / outdated]
```

### 6. Summary Statistics

```markdown
## Refinement Summary
- Total backlog: [N] issues
- Ready (5/5): [N] ([%])
- Needs Work: [N] ([%])
- Stale: [N] ([%])
- Average spec score: [N.N]/5
- Recommended sprint candidates: [N]
```

### 7. Interactive Spec Completion

After presenting results, offer to interactively complete specs for "Needs Work" issues:

- For each issue with score < 5, ask the user about missing elements
- Generate updated issue body with complete Atomic Spec
- Offer to update the issue via `gh issue edit`

```markdown
### Spec Completion Offer

[N] issues need spec work. Shall I help complete them?

1. #N: [title] - missing [elements] (estimated: 2-3 questions)
2. #N: [title] - missing [elements] (estimated: 1-2 questions)
...

Reply with issue numbers to start, or "all" for batch processing.
```
