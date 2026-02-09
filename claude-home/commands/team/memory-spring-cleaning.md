---
description: "Clean and organize project memory files"
---

## Instructions

Review and clean all CLAUDE.md and CLAUDE.local.md files in the project hierarchy.

1. **Inventory**: List all `CLAUDE.md` and `CLAUDE.local.md` files from root to subdirectories
2. **Backup**: Copy current files to a temporary location before making changes
3. **Verify**: For each file, compare documented patterns against actual codebase implementation
4. **Clean**: Remove obsolete information, consolidate duplicates, correct inaccuracies
4. **Relocate**: Move information to the most appropriate file by component scope:
   - Project-wide patterns → root `CLAUDE.md`
   - Component-specific details → subdirectory `CLAUDE.md`
   - Private developer notes → `CLAUDE.local.md`
5. **Report**: Summarize changes made (items removed, moved, updated)
