---
description: "Analyze AI agent configuration and performance for calibration"
---

## Instructions

Generate an AI-DLC agent calibration report. Load `ai-dlc-ceremonies` skill for calibration session patterns. Load `ticket-management` skill for quality metrics context. Load `prompt-engineering` skill for Hooks / CLAUDE.md design patterns.

Focus area: `$ARGUMENTS`

Accepted arguments:
- `full` (default): all 5 analysis areas
- `claude-md`: CLAUDE.md / SKILL quality scan only
- `hooks`: Hooks effectiveness review only
- `churn`: Interaction Churn analysis only
- `recommendations`: improvement recommendations only

### 1. CLAUDE.md / SKILL Quality Scan

**1.1 File Discovery**

Scan for CLAUDE.md and SKILL files:

```bash
# Project CLAUDE.md files
ls -la CLAUDE.md .claude/CLAUDE.md 2>/dev/null

# Skill files
find .claude/skills/ -name "SKILL.md" 2>/dev/null
```

**1.2 Freshness Check**

For each file, compare last update vs recent code changes:

```bash
# CLAUDE.md last update
git log -1 --format="%ad (%ar)" -- CLAUDE.md

# Recent code changes that might need CLAUDE.md update
git log --since="30 days ago" --format="%h %s" --diff-filter=A -- "*.ts" "*.js" "*.py" "*.rs" | head -20
```

Flag files not updated in 30+ days when code has changed.

**1.3 Completeness Check**

Verify CLAUDE.md covers:
- [ ] Project structure overview
- [ ] Key commands / scripts
- [ ] Design patterns / conventions
- [ ] Testing approach
- [ ] Development workflow

**1.4 Undocumented Pattern Detection**

Look for patterns in code that aren't reflected in CLAUDE.md:

```bash
# Frequently appearing patterns in recent commits
git log --since="30 days ago" --format="" --name-only | sort | uniq -c | sort -rn | head -20
```

### 2. Hooks Effectiveness Review

**2.1 Settings Analysis**

```bash
# Claude Code hooks
cat .claude/settings.json 2>/dev/null
cat .claude/settings.local.json 2>/dev/null
```

Parse hooks configuration: event types, matchers, commands.

**2.2 Git Hooks**

```bash
ls -la .git/hooks/ 2>/dev/null
# Check for husky / lint-staged
cat .husky/pre-commit 2>/dev/null
cat package.json 2>/dev/null | grep -A5 "lint-staged"
```

**2.3 Assessment**

For each hook, assess: effective (catches real issues), noisy (too many false positives), or unused (never triggers).

### 3. Interaction Churn Analysis

Analyze high-churn patterns from recent sprint data:

```bash
# PR review rounds as churn proxy
gh pr list --state merged --json number,title,reviews,commits --limit 50

# Issues with many comments (conversation length proxy)
gh issue list --state closed --json number,title,comments --limit 50
```

Categorize by root cause: spec quality, scope, technical complexity.

### 4. AI-Confidence Distribution (if available)

If the project tracks AI-Confidence scores in issue metadata or PR descriptions, aggregate:
- Distribution across score bands (90-100, 80-89, 60-79, 0-59)
- Task categories with consistently low confidence
- Correlation with churn / rejection rates

### 5. Generate Calibration Report

```markdown
## Agent Calibration Report - [Date]

### CLAUDE.md / SKILL Health
| File | Last Updated | Age | Status | Notes |
|---|---|---|---|---|
| CLAUDE.md | [date] | [days] | [fresh/stale/missing] | [notes] |
| .claude/CLAUDE.md | [date] | [days] | [fresh/stale/missing] | [notes] |
| [skill]/SKILL.md | [date] | [days] | [fresh/stale/missing] | [notes] |

**Completeness**: [N]/5 areas documented
**Undocumented patterns**:
- [pattern]: found in [files], not in CLAUDE.md

### Hooks Review
| Hook | Type | Event | Assessment | Notes |
|---|---|---|---|---|
| [name] | [claude/git] | [event] | [effective/noisy/unused] | [notes] |

**Coverage gaps**: [areas without hook protection]
**Proposed additions**:
- [hook]: [rationale]

### Interaction Churn Summary
| Root Cause | Count | % | Top Examples |
|---|---|---|---|
| Spec quality | [N] | [%] | #N, #N |
| Scope | [N] | [%] | #N, #N |
| Technical | [N] | [%] | #N, #N |

**Trend vs previous calibration**: [improving/stable/worsening]

### Improvement Recommendations
| Priority | Action | Expected Impact | Effort |
|---|---|---|---|
| P0 | [action] | [impact] | [low/medium/high] |
| P1 | [action] | [impact] | [low/medium/high] |
| P2 | [action] | [impact] | [low/medium/high] |
```

### Interactive Follow-up

Offer to:
- Apply P0 recommendations immediately (edit CLAUDE.md, add hooks)
- Create issues for P1/P2 items
- Deep-dive into specific churn patterns
- Compare with previous calibration reports
