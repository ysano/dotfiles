---
description: "Systematic approach for building complex features incrementally, preventing premature completion and ensuring comprehensive functionality through structured feature tracking."
---

## Instructions

Build a feature incrementally using the structured approach for: **$ARGUMENTS**

This command implements best practices for long-running agent tasks, preventing the tendency to one-shot applications or prematurely consider projects complete.

> **Note:** `$ARGUMENTS` is automatically replaced with the text following the command invocation.
> Example: `/dev:incremental-feature-build user authentication system` sets `$ARGUMENTS` to "user authentication system"

---

## Phase 1: Feature Requirements Expansion

### 1.1 Create Feature Tracking Directory

```bash
mkdir -p .feature-tracking || { echo "ERROR: Cannot create .feature-tracking directory. Check permissions."; exit 1; }
```

### 1.2 Generate Comprehensive Feature List

Analyze the user's request and expand it into a comprehensive list of granular features. Each feature should be a discrete, testable unit of functionality.

**Create file: `.feature-tracking/features.json`**

```json
{
  "project": "$ARGUMENTS",
  "created": "YYYY-MM-DD",
// ... (9 lines truncated)
```

### 1.3 Feature Schema

Each feature MUST follow this exact JSON schema:

```json
{
  "id": "FEAT-001",
  "category": "functional|ui|integration|performance|security|accessibility",
// ... (13 lines truncated)
```

> **Note:** This command uses a simple boolean `passes` field since features are implemented sequentially.
> The parallel command (`/dev:parallel-feature-build`) uses a multi-state `status` field to track
> concurrent work: `pending`, `in_progress`, `passed`, `blocked`.

### 1.4 Feature Categories

Generate features across ALL relevant categories:

| Category | Description | Examples |
|----------|-------------|----------|
| `functional` | Core business logic | User can submit form, data saves correctly |
| `ui` | User interface elements | Button displays, modal opens, responsive layout |
| `integration` | System connections | API calls work, database syncs, auth flows |
| `performance` | Speed and efficiency | Page loads under 3s, lazy loading works |
| `security` | Protection measures | Input sanitized, auth required, CSRF protection |
| `accessibility` | Inclusive design | Screen reader support, keyboard navigation |

### 1.5 Feature Generation Guidelines

When expanding the user's request:

1. **Use Hierarchical Organization**: Group features into epics/modules (Small: 10-30, Medium: 30-60, Large: 60-100 features)
2. **Be Specific**: Each feature should be independently verifiable
3. **Include Edge Cases**: Error states, empty states, boundary conditions
4. **Cover All Paths**: Happy path AND unhappy paths
5. **Think Like a User**: What would they expect at each step?

---

## Phase 2: Initialize Progress Tracking

### 2.1 Create Progress File

**Create file: `.feature-tracking/PROGRESS.md`**

```markdown
# Feature Implementation Progress

## Project: $ARGUMENTS
// ... (42 lines truncated)
```

### 2.2 Git Initialization Check

```bash
git add .feature-tracking/
git commit -m "feat: initialize feature tracking for $ARGUMENTS

- Created features.json with X features to implement
- All features marked as failing (passes: false)
- Created progress tracking file"
```

---

## Phase 3: Incremental Implementation

### CRITICAL RULES

**You MUST follow these rules without exception:**

1. **ONE FEATURE AT A TIME**: Never work on multiple features simultaneously
2. **NO TEST REMOVAL**: It is **UNACCEPTABLE** to remove or edit features from features.json because this could lead to missing or buggy functionality
3. **ONLY CHANGE STATUS**: The only modification allowed to features.json is changing `passes` from `false` to `true` and updating `implementedAt` and `commitHash`
4. **COMMIT AFTER EACH**: Create a git commit after each feature is implemented
5. **CLEAN STATE**: Each commit must leave the codebase in a working state

### 3.1 Feature Implementation Loop

For each feature, follow this exact workflow:

#### Step A: Select Next Feature

1. Read `.feature-tracking/features.json`
2. Find the first feature where `passes: false`
3. Check dependencies are satisfied (all dependent features pass)
4. Announce: "Now implementing FEAT-XXX: [description]"

#### Step B: Implement Feature

1. Write the necessary code
2. Ensure existing functionality still works
3. Test the specific feature manually or with tests
4. Verify all steps in the feature definition

#### Step C: Update Tracking (JSON Only)

Edit `.feature-tracking/features.json`:
- Change `"passes": false` to `"passes": true`
- Set `"implementedAt": "YYYY-MM-DD HH:MM"`
- Update summary counts

#### Step D: Commit Progress

```bash
git add .
git commit -m "feat(FEAT-XXX): [brief description]

// ... (9 lines truncated)
```

#### Step E: Update Progress File

Add to `.feature-tracking/PROGRESS.md`:

```markdown
### FEAT-XXX - [Description]
- **Status**: PASSED
- **Implemented**: YYYY-MM-DD HH:MM
- **Commit**: [hash]
- **Notes**: [any implementation notes]
```

#### Step F: Update Commit Hash

Edit features.json to add the commit hash from Step D.

### 3.2 Continue Until Complete

Repeat the implementation loop until ALL features show `passes: true`.

---

## Phase 4: Recovery Protocols

### 4.1 If Implementation Fails

**DO NOT** mark it as passing or remove the feature. Use `git restore [files]` to revert, document the blocker in PROGRESS.md, and move to the next non-blocked feature.

### 4.2 If Code Breaks

Run `git diff` to see changes, `git stash` to save work, verify baseline still works, then `git stash pop` and fix carefully. Or `git restore .` to fully reset.

### 4.3 Revert Bad Commits

```bash
git log --oneline -10  # Find the bad commit
git revert [hash]      # Create revert commit
```

---

## Phase 5: Completion Verification

### 5.1 Final Checklist

```bash
# Must return 0 - all features must pass
cat .feature-tracking/features.json | jq '[.features[] | select(.passes == false)] | length'
```

### 5.2 Summary Generation

Create final summary in PROGRESS.md:

```markdown
## Project Complete

- **Total Features**: X
// ... (13 lines truncated)
```

### 5.3 Final Commit

```bash
git add .
git commit -m "feat: complete $ARGUMENTS implementation

// ... (8 lines truncated)
```

---
