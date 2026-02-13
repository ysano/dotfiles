---
description: "Design ADR-driven architecture from PRD input"
---

## Instructions

Create an AI-DLC Architecture Document with ADR-driven design. Load `ai-dlc-upstream` skill for artifact format contracts and `ticket-management` skill for Atomic Spec compatibility requirements.

PRD path or context: `$ARGUMENTS`

### Pluggability Check

Run BMAD detection per `ai-dlc-upstream` > `references/pluggability.md`. If `_bmad/` or `.bmad/` exists, display info banner and continue.

### Step 1: PRD Input

Read the PRD document:

- If `$ARGUMENTS` contains a file path: read the specified PRD file
- If `$ARGUMENTS` contains context but no file: search for PRD files in `docs/prd-*.md`
- If no PRD exists: ask user whether to create one first (`/ai-dlc:create-prd`) or proceed with inline requirements

**Inline requirements path**: If proceeding without a PRD, derive requirements from `$ARGUMENTS` context. Set `linked-prd: none (inline)` in metadata. FR-NNN traceability will be limited — generate provisional identifiers (PFR-001, etc.) and flag them as provisional in the output.

Extract from PRD (or derive from inline context):
- **FR-NNN** functional requirements (full list)
- **NFR** non-functional requirements
- **User Personas** (for understanding access patterns)
- **Success Metrics** (for quality attributes)
- **Scope** boundaries

### Step 2: Architecture Design

Generate the architecture document following `ai-dlc-upstream` > `references/artifact-architecture.md` format (metadata header + all required sections: Executive Summary, System Context Diagram, Component Design, Data Model, ADR Records, Technology Stack, Quality Attributes).

Key design notes:
- Component Design: Include AI Implementation Suitability (High/Med/Low) per component
- ADR-NNN: Generate for each significant decision (Context/Options/Decision/Rationale/Consequences)
- Mermaid diagrams: System Context and ER diagrams where beneficial

### Step 3: AI-DLC Compatibility Check

Validate architecture per `ai-dlc-upstream` > `references/artifact-architecture.md` compatibility criteria:

```markdown
## AI-DLC Compatibility Check
| Check | Status | Notes |
|---|---|---|
| Atomic Spec Divisibility | Pass/Fail | Each component implementable in 2-4h |
| Context Window Fit | Pass/Fail | Interfaces fit AI agent understanding |
| Test Automation | Pass/Fail | CI-automatable test strategy |
| ADR Completeness | Pass/Fail | All major decisions have ADRs |
```

If any check fails, propose remediation before proceeding.

### Step 4: Output & Next Steps

Write the architecture document to `docs/architecture-{kebab-case-name}.md`.

Update the linked PRD's `linked-architecture` metadata field if the PRD file exists.

```markdown
## Next Steps
- [ ] Review architecture with team
- [ ] Run `/ai-dlc:create-stories` to decompose into Atomic Spec stubs
- [ ] Or run `/ai-dlc:plan` if stories already exist
```

### Interactive Refinement

After presenting results, offer to:
- Add or modify ADRs
- Adjust component boundaries for better AI-agent divisibility
- Add or refine quality attributes
- Proceed directly to `/ai-dlc:create-stories`
