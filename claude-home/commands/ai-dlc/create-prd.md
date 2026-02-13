---
description: "Create interview-driven PRD with AI-DLC traceability"
---

## Instructions

Create an AI-DLC PRD (Product Requirements Document) through structured interview. Load `ai-dlc-upstream` skill for artifact format contracts and `ticket-management` skill for Atomic Spec mapping.

Project context: `$ARGUMENTS`

### Pluggability Check

Run BMAD detection per `ai-dlc-upstream` > `references/pluggability.md`. If `_bmad/` or `.bmad/` exists, display info banner and continue.

### Phase 1: Discovery Interview

Conduct a structured interview using AskUserQuestion, organized in 4 domains. Adapt based on `$ARGUMENTS` — skip questions already answered.

**1.1 Strategic Context**

- What problem are you solving? What is the business impact if unsolved?
- Who are the stakeholders? What is the business value (revenue, cost, time)?
- What is the timeline and urgency driver?

**1.2 User & Scope**

- Who are the primary users? (2-3 personas with roles, goals, pain points)
- What is the user journey from problem to solution?
- What is explicitly in scope? Out of scope?

**1.3 Technical Context**

- What existing systems does this interact with?
- Are there performance, security, or scalability requirements?
- Any technology constraints or preferences?

**1.4 AI-DLC Context**

- What percentage of implementation is suitable for AI agents vs human developers?
- Which areas are highest risk (domain complexity, security, creative judgment)?
- How will success be measured? What are the verification methods?

### Phase 2: PRD Generation

Synthesize interview responses into a PRD document following `ai-dlc-upstream` > `references/artifact-prd.md` format (metadata header + all required sections).

Key generation notes:
- FR-NNN: Use action verbs ("Implement", "Display", "Calculate", "Validate")
- Priority: P0 = must-have, P1 = should-have, P2 = nice-to-have, P3 = future
- Personas: 2-3 from interview (role, goal, pain point, tech literacy)

### Phase 3: Validation & Output

**3.1 Quality Gate Check**

Validate per `ai-dlc-upstream` > `references/upstream-pipeline.md` Gate 1:

| Check | Status |
|---|---|
| Problem Statement has quantified impact | Pass/Fail |
| All requirements have FR-NNN IDs | Pass/Fail |
| Success Metrics are measurable | Pass/Fail |
| Scope boundaries are clear | Pass/Fail |

**3.2 Atomic Spec Mapping Preview**

Show how PRD sections will map to Atomic Spec elements:

```markdown
## Atomic Spec Mapping Preview
| PRD Section | → Atomic Spec Element | Coverage |
|---|---|---|
| Problem Statement | Context | {summary} |
| FR-001 to FR-NNN | Expected Behavior | {N} requirements |
| NFR | Constraints | {N} constraints |
| Success Metrics | Verification | {N} metrics |
```

**3.3 Write Output**

Write the PRD to `docs/prd-{kebab-case-name}.md`.

**3.4 Next Steps**

```markdown
## Next Steps
- [ ] Review PRD with stakeholders
- [ ] Run `/ai-dlc:create-architecture` to design system architecture
- [ ] Or run `/ai-dlc:create-stories` if architecture is already clear
- [ ] For quick iteration: `/ai-dlc:quick-spec` (1-5 stories only)
```

### Interactive Refinement

After presenting results, offer to:
- Add or modify functional requirements
- Adjust priorities (P0-P3)
- Refine personas or success metrics
- Proceed directly to `/ai-dlc:create-architecture`
