---
description: "Convert BMAD or external documents to AI-DLC artifact format"
---

## Instructions

Convert BMAD or external requirement documents to AI-DLC contract format. Load `ai-dlc-upstream` skill for artifact format contracts (especially `references/pluggability.md` for conversion rules) and `ticket-management` skill for Atomic Spec details.

Source document path or context: `$ARGUMENTS`

### Step 1: Source Detection

Identify the source document format:

```bash
# Check for BMAD workspace
ls -d _bmad .bmad 2>/dev/null

# Read the source document
# (path from $ARGUMENTS or prompt user)
```

Classify the source:

| Format | Detection | Conversion Target |
|---|---|---|
| **BMAD PRD** | `_bmad/` exists + PRD structure | `artifact-prd.md` format |
| **BMAD Architecture** | `_bmad/` exists + Architecture structure | `artifact-architecture.md` format |
| **BMAD Stories** | `_bmad/` exists + Epic/Story structure | `artifact-epic-story.md` format |
| **Generic Requirements** | No BMAD markers, requirement-like content | Best-fit AI-DLC format |
| **Unknown** | Cannot classify | Ask user for guidance |

### Step 2: Conversion

Apply conversion rules from `ai-dlc-upstream` > `references/pluggability.md`.

**2.1 PRD Conversion**

- Extract and restructure sections per mapping table
- Assign FR-NNN numbers to functional requirements
- Convert success criteria to measurable metrics with baseline/target/method
- Add AI-DLC metadata header

**2.2 Architecture Conversion**

- Convert technology decisions to ADR-NNN format
- Add AI Implementation Suitability assessment per component
- Convert diagrams to Mermaid format if possible
- Add AI-DLC metadata header

**2.3 Story Conversion**

- Convert user stories / acceptance criteria to Atomic Spec 5-element stubs
- Apply size guidelines (S/M/L)
- Add dependency relationships
- Run Agent-Ready assessment

**2.4 Generic Document**

- Analyze structure and map to closest AI-DLC artifact type
- Extract requirements and assign FR-NNN where applicable
- Flag sections that don't map cleanly

### Step 3: Gap Analysis

For each converted document, generate a gap report:

```markdown
## Conversion Gap Report

### Source: {filename}
### Target Format: {ai-dlc-prd | ai-dlc-architecture | ai-dlc-epic-story}

### Converted Successfully
- [x] {source section} → {target section}
- [x] {source section} → {target section}

### Needs Manual Input
- [ ] {missing element}: {what's needed and why}
- [ ] {missing element}: {what's needed and why}

### Structural Changes
- {description of structural reorganization}

### Not Applicable
- {items from source that don't map to AI-DLC}
```

### Step 4: Output

Write converted documents to appropriate paths:
- PRD → `docs/prd-{kebab-case}.md`
- Architecture → `docs/architecture-{kebab-case}.md`
- Stories → `docs/stories-{kebab-case}.md`

Display the gap report and prompt for manual input on missing items.

### Interactive Refinement

After presenting results, offer to:
- Fill in missing elements interactively
- Adjust the conversion mapping
- Convert additional documents
- Proceed to `/ai-dlc:plan` with the converted artifacts
