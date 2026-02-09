---
name: handoff-continue
model: opus
description: Create a handoff document and automatically spawn a new Claude session in a Zellij pane to continue the work seamlessly.
author: Quintin Henry (https://github.com/qdhenry/)
---

Create a comprehensive handoff document and automatically spawn a new Claude Code session in a Zellij pane to continue the work without manual intervention.

## Arguments

Optional argument: filename for the handoff document (without path)

- Example: `/handoff-continue feature-auth-handoff.md`
- If not provided, auto-generates: `HANDOFF_[TOPIC]_[TIMESTAMP].md`

## Role

You are a senior engineer creating a comprehensive handoff document AND orchestrating a seamless transition to a new Claude session. You must capture ALL relevant context, then spawn a new session that can immediately continue the work.

## Task

1. Analyze the entire conversation to extract key work context
2. Generate a comprehensive handoff document
3. Save it to the specified location (or auto-generate filename)
4. **Spawn a new Zellij pane with Claude Code loaded with the handoff document**

## Instructions

### Phase 1: Context Gathering

Analyze the conversation for:

- What task/feature/bug was being worked on
- All files that were read, created, or modified
- Key decisions made and their rationale
- Problems encountered and how they were solved
- Current state of the work (what's done vs pending)
- Any open questions or blockers

### Phase 2: Git Analysis

If in a git repository, capture:

- Current branch name
- Uncommitted changes (staged and unstaged)
- Recent commits made during this session
- Files in .gitignore that might be relevant

### Phase 3: Document Generation

Create a structured document with all sections below.

### Phase 4: Spawn Continuation Session

After saving the handoff document, execute the Zellij command to spawn a new pane:

```bash
# IMPORTANT: Use full path to claude executable (not alias) and omit -c flag to keep pane open
zellij run -n "Claude Continue" -- bash -c "cd '[PROJECT_DIR]' && /Users/quintinhenry/.claude/local/claude --dangerously-skip-permissions '[HANDOFF_FILE_PATH]'"
```

**IMPORTANT:**

- Replace `[PROJECT_DIR]` with the actual project directory path
- Replace `[HANDOFF_FILE_PATH]` with the actual absolute path to the saved handoff document
- Must use full path `/Users/quintinhenry/.claude/local/claude` (not `claude` alias) because new shell doesn't inherit aliases
- Omit the `-c` flag so the pane stays open if Claude exits

## Output Format

````markdown
# Handoff: [Brief Title of Work]

**Created:** [timestamp]
// ... (133 lines truncated)
```bash
## Useful commands for continuing this work
[command 1]
[command 2]
```
````

### Search Queries

If you need to find more context:

- `[grep/search pattern 1]` - finds [what]
- `[grep/search pattern 2]` - finds [what]

---

## Open Questions

- [ ] [Question 1 that needs answering]
- [ ] [Question 2 that needs answering]

---

## Session Notes

[Any additional context, observations, or notes that don't fit above]

---

_This handoff was generated at context window capacity. A new Claude session is being spawned to continue._

````

## Constraints

// ... (28 lines truncated)
```bash
## Spawn new Zellij pane with Claude using full executable path
zellij run -n "Claude Continue" -- bash -c "cd '[PROJECT_DIR]' && /Users/quintinhenry/.claude/local/claude '[ABSOLUTE_PATH_TO_HANDOFF]'"
````

**Key points:**

// ... (41 lines truncated)
   ```bash
   zellij run -n "Claude Continue" -- bash -c "cd '/path/to/project' && /Users/quintinhenry/.claude/local/claude --dangerously-skip-permissions '/path/to/project/docs/handoffs/HANDOFF_TOPIC_01_06_22_30.md'"
   ```
6. Confirm spawn success or provide fallback instructions

```

