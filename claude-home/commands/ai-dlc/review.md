---
description: "Review PR with AI-DLC quality gates and Atomic Spec traceability"
---

## Instructions

Perform an AI-DLC quality gate code review with Atomic Spec traceability and AI-specific metrics. Load `ai-dlc-upstream` skill for artifact format contracts and `ticket-management` skill for Atomic Spec / Agent Loop / DoD details.

PR number or context: `$ARGUMENTS`

### Step 1: PR Data Collection

Gather PR information:

```bash
gh pr view $PR_NUMBER --json number,title,body,labels,milestone,additions,deletions,changedFiles,commits,reviews,state,headRefName
gh pr diff $PR_NUMBER
```

If `$ARGUMENTS` is empty, check for current branch PR:

```bash
gh pr view --json number,title,body,labels,milestone,additions,deletions,changedFiles,commits,reviews,state
```

### Step 2: Atomic Spec Traceability

Verify the PR → Issue → Atomic Spec chain:

**2.1 Issue Link**

Extract linked issue from PR body or branch name (e.g., `fix/123-description`, `feat/456-feature`):

```bash
gh issue view $ISSUE_NUMBER --json number,title,body,labels
```

**2.2 Spec Completeness Check**

Verify the linked issue has Atomic Spec 5 elements:

```markdown
## Atomic Spec Traceability
| Element | Present | Content Summary |
|---|---|---|
| Context | Yes/No | {summary or "MISSING"} |
| Current Behavior | Yes/No | {summary or "MISSING"} |
| Expected Behavior | Yes/No | {summary or "MISSING"} |
| Constraints | Yes/No | {summary or "MISSING"} |
| Verification | Yes/No | {summary or "MISSING"} |

Spec Score: [0-5]/5
```

**2.3 Alignment Check**

Compare PR changes against Expected Behavior:
- Do the changes implement what the spec describes?
- Are there changes not covered by the spec? (scope creep)
- Are spec requirements missing from the implementation?

### Step 3: Code Quality Review

Standard code quality checks plus AI-DLC specific gates.

**3.1 Standard Quality**

- Code correctness and logic
- Error handling
- Test coverage (new tests for new behavior)
- Security considerations (OWASP top 10)
- Performance implications

**3.2 AI-DLC Quality Gates**

```markdown
## AI-DLC Quality Gates
| Gate | Status | Details |
|---|---|---|
| **Scope Check** | Pass/Fail | {N} files changed (limit: 5), +{N}/-{N} lines (limit: 300) |
| **Spec Alignment** | Pass/Fail | Changes match Expected Behavior |
| **AI-Confidence** | High/Med/Low | Self-assessment of review confidence |
| **Churn Indicator** | {N} rounds | Review rounds count (>3 = Spec revision flag) |
| **DoD Compliance** | Pass/Fail | Tests pass, docs updated, no TODO debt |
```

**Scope Check details:**
- Files changed: <= 5 (warn at 4, fail at 6+)
- Lines changed: <= 300 (warn at 250, fail at 400+)
- If exceeded: recommend splitting per `ticket-management` granularity rules

**Churn Indicator:**
- 1-3 review rounds: Normal
- 4-6 rounds: `[Churn Alert]` — Spec may need supplementation
- 7+ rounds: Recommend returning to Spec Definition stage

### Step 4: Structured Report

Generate the review report:

```markdown
## AI-DLC Code Review Report

### Summary
- **PR**: #{number} — {title}
- **Issue**: #{issue} — {issue title}
- **Spec Score**: [0-5]/5
- **Quality Gate**: Pass / Conditional Pass / Fail

### Atomic Spec Traceability
{from Step 2}

### Code Review
{categorized findings}

#### Critical (Must Fix)
- {finding with file:line reference}

#### Important (Should Fix)
- {finding}

#### Suggestions (Nice to Have)
- {finding}

### AI-DLC Quality Gates
{from Step 3.2}

### Recommendation
- [ ] **Approve**: All gates pass, spec alignment confirmed
- [ ] **Request Changes**: {specific items to address}
- [ ] **Spec Revision Needed**: Churn/scope indicates spec quality issue
```

### Interactive Refinement

After presenting the report, offer to:
- Submit the review as a PR comment (`gh pr review`)
- Drill down into specific findings
- Check related PRs for the same Epic
- Flag for Spec Definition revision if churn is high
