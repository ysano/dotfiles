---
description: "Decompose PRD + Architecture into Agent Loop-ready Atomic Spec stubs"
---

## Instructions

Decompose PRD and Architecture into Epics and Stories as Atomic Spec stubs. Load `ai-dlc-upstream` skill for artifact format contracts and `ticket-management` skill for Atomic Spec template and Agent Loop details.

Input documents or context: `$ARGUMENTS`

### Pluggability Check

Run BMAD detection per `ai-dlc-upstream` > `references/pluggability.md`. If `_bmad/` or `.bmad/` exists, display info banner and continue.

### Step 1: Input Documents

Read PRD and Architecture documents:

- If `$ARGUMENTS` provides file paths: read specified files
- Otherwise: search for `docs/prd-*.md` and `docs/architecture-*.md`
- If neither exists: ask user to create them first or provide inline requirements

Extract:
- **FR-NNN** list from PRD
- **Components** from Architecture (with AI Implementation Suitability)
- **ADR** decisions (for Constraints mapping)
- **Quality Attributes** (for Verification mapping)
- **NFR** (for Constraints)

### Step 2: Epic Identification

Group related FR-NNN by functional area or component:

```markdown
## Epics

### Epic 1: {Title}
- **Goal**: {Business goal from PRD}
- **Linked PRD**: {path}
- **Linked Architecture**: {path}
- **FR Coverage**: FR-001, FR-002, FR-003
- **Priority**: P0
- **Stories**: {N} stories
```

Prioritization:
- P0: Must-have FR coverage
- P1: Should-have FR coverage
- P2: Nice-to-have FR coverage
- P3: Future FR coverage

### Step 3: Story Decomposition

For each Epic, decompose into Stories following `ai-dlc-upstream` > `references/artifact-epic-story.md` format (Atomic Spec 5-element stub with metadata).

Pre-fill each element from upstream artifacts:
- **Context** ← PRD Problem Statement + Architecture Executive Summary
- **Current Behavior** ← PRD current gap (or "New feature")
- **Expected Behavior** ← FR-NNN + Architecture Component interface
- **Constraints** ← PRD NFR + Architecture ADR Consequences
- **Verification** ← PRD Success Metrics + Architecture Quality Attributes

Decomposition rules:
- **Size constraint**: S (1-2h), M (2-4h). L (4h+) must be split
- **File constraint**: <= 5 files per story
- **Diff constraint**: <= 300 lines per story PR
- **Vertical slice**: Prefer end-to-end slices over horizontal layers
- **Parallelization**: Identify stories that can run concurrently

### Step 4: Dependency Graph

Visualize inter-story and inter-epic dependencies:

```markdown
## Dependency Graph

Epic 1:
  Story 1.1 → Story 1.2 → Story 1.3
                         → Story 1.4 (parallel with 1.3)

Epic 2 (depends on Epic 1):
  Story 2.1 → Story 2.2
```

Identify:
- Critical path (longest dependency chain)
- Parallelizable groups
- Potential bottlenecks

### Step 5: Agent-Ready Assessment

Evaluate each story per `ai-dlc-upstream` > `references/artifact-epic-story.md` criteria:

```markdown
## Agent-Ready Summary

| Story | Size | Status | Missing Elements | AI Suitability |
|---|---|---|---|---|
| {title} | S/M | Ready / Needs Revision | {list or "None"} | High/Med/Low |

**Ready**: {N} stories — can proceed to Agent Loop Stage 3 (AI Planning)
**Needs Revision**: {N} stories — require Agent Loop Stage 2 (Spec Definition)
```

### Step 6: Output & Options

Write stories to `docs/stories-{kebab-case-name}.md`.

Update linked PRD and Architecture metadata fields if files exist.

Present options:

```markdown
## Next Steps

### Option A: Sprint Planning
- [ ] Run `/ai-dlc:plan` to integrate stories into sprint planning

### Option B: GitHub Issues
Create issues from stories:
- [ ] Create {N} GitHub Issues with Atomic Spec bodies
- [ ] Add labels: ai-dlc, story, size:{S|M|L}, epic:{name}
- [ ] Set milestone: {sprint name}

### Option C: Refinement
- [ ] Review "Needs Revision" stories and complete missing elements
- [ ] Split oversized (L) stories
- [ ] Adjust priorities
```

If user selects Option B, create issues:

```bash
gh issue create --title "{Story title}" \
  --body "{Atomic Spec 5 elements}" \
  --label "ai-dlc,story,size:{size}" \
  --milestone "{sprint}"
```

### Interactive Refinement

After presenting results, offer to:
- Split or merge stories
- Adjust priorities or dependencies
- Complete missing Atomic Spec elements
- Create GitHub Issues from selected stories
- Proceed to `/ai-dlc:plan` for sprint planning
