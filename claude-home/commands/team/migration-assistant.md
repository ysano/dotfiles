---
description: "Assist with system migration planning and execution"
---

## Instructions

Plan and execute data migrations between project management systems (GitHub Issues, Linear, Jira). Think harder about data integrity and rollback safety.

Action: `$ARGUMENTS`

For platform-specific data models, load the relevant skill: `github-projects-v2`, `linear`, or `jira`.

### Actions

| Action | Description |
|--------|-------------|
| `plan` | Analyze source/target systems, map fields, estimate scope |
| `analyze` | Count items, check compatibility, identify custom fields |
| `migrate` | Execute migration in batches with checkpoints |
| `verify` | Compare source/target data integrity |
| `rollback` | Restore from checkpoint |

### Migration Steps

1. **Prerequisites**: Verify `gh` CLI auth, check API access to target system, confirm backup storage
2. **Field Mapping**: Map source fields to target (e.g., GitHub labels → Linear labels, milestones → cycles)
3. **Dry Run**: Execute with `--dry-run` to validate mapping without writing
4. **Confirm**: Present dry-run results and get explicit user approval before proceeding
5. **Batch Execute**: Migrate in batches, creating checkpoints after each batch
5. **Verify**: Compare record counts, validate relationships, check attachments
6. **Finalize**: Update webhooks, redirect references, archive source if requested

### Safety Requirements

- Always create a full backup before migration
- Use batch processing with transaction checkpoints
- Log all operations for audit trail
- Support selective rollback to any checkpoint
- Never delete source data until verification passes
