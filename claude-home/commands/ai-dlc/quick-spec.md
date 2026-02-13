---
description: "Generate lightweight single-document spec (PRD + Architecture + Stories)"
---

## Instructions

Generate an AI-DLC Quick Spec — a lightweight single document combining PRD, Architecture, and Stories for small projects (1-5 stories). Load `ai-dlc-upstream` skill for artifact format contracts and `ticket-management` skill for Atomic Spec details.

Project context: `$ARGUMENTS`

### Pluggability Check

Run BMAD detection per `ai-dlc-upstream` > `references/pluggability.md`. If `_bmad/` or `.bmad/` exists, display info banner and continue.

### Step 1: Quick Discovery

Conduct a brief structured interview using AskUserQuestion (3-5 questions):

1. **Problem & Goal**: What problem are you solving? What does success look like?
2. **Scope**: What's in scope for this iteration? What's explicitly out?
3. **Technical Context**: Any existing systems, constraints, or preferences?
4. **Timeline**: What's the delivery target? (Solo / Pod scale assumed)
5. **AI Suitability** (if applicable): Which parts are best suited for AI agent implementation?

If `$ARGUMENTS` provides sufficient context, skip redundant questions.

### Step 2: Scale Check

Based on discovery answers, estimate the number of stories:

- **1-5 stories**: Continue with Quick Spec
- **6+ stories**: Recommend the full pipeline and offer to switch:

```
This project appears to need ~{N} stories. Consider using the full pipeline:
  /ai-dlc:create-prd → /ai-dlc:create-architecture → /ai-dlc:create-stories

Continue with Quick Spec anyway? (suitable for initial exploration)
```

### Step 3: Generate Quick Spec

Generate the document following `ai-dlc-upstream` > `references/artifact-quick-spec.md` format:

```yaml
---
type: ai-dlc-quick-spec
version: "1.0"
status: draft
created: {today}
story-count: {N}
---
```

Sections:

1. **Problem & Goal** — Problem statement, goal, success criteria
2. **Solution Overview** — Technical approach, key decisions, out of scope
3. **Stories** — Each as Atomic Spec 5-element stub:
   - Context, Current Behavior, Expected Behavior, Constraints, Verification
   - Size: S (1-2h) or M (2-4h). Flag L stories for splitting
4. **Out of Scope** — Explicit exclusions

### Step 4: Agent-Ready Assessment

For each story, evaluate Agent-Ready status per `ai-dlc-upstream` > `references/artifact-epic-story.md` criteria:

```markdown
## Agent-Ready Summary
| Story | Size | Status | Missing Elements |
|---|---|---|---|
| {title} | S/M | Ready / Needs Revision | {list or "None"} |
```

### Step 5: Output & Next Steps

Write the spec to `docs/spec-{kebab-case-name}.md`.

Present next steps:

```markdown
## Next Steps
- [ ] Review and refine stories (especially "Needs Revision" items)
- [ ] Run `/ai-dlc:plan` to integrate into sprint planning
- [ ] For larger scope, consider: `/ai-dlc:create-prd` → `/ai-dlc:create-architecture` → `/ai-dlc:create-stories`
```

### Interactive Refinement

After presenting results, offer to:
- Adjust scope or split/merge stories
- Add missing Atomic Spec elements
- Convert to full pipeline artifacts if complexity increased
- Create GitHub Issues from stories (`gh issue create`)
