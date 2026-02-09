---
description: "Validate and package a Claude Code Skill into a distributable zip file"
---

## Instructions

This command validates and packages an existing skill for distribution. Follow these steps:

### Phase 1: Skill Selection

1. **Identify the skill to package**
   - Determine skill location (personal or project)
   - Verify skill exists and has SKILL.md
   - Check current state of the skill

2. **Pre-package checks**
   - Ensure all TODOs are completed
   - Verify documentation is finalized
   - Check that examples are tested

### Phase 2: Validation

1. **Run quick validation**
   - Use the quick_validate.py script
   - Check YAML frontmatter validity
   - Verify required fields (name, description)
   - Check for prohibited characters
   - Validate directory structure

2. **Run comprehensive validation** (if quick validation passes)
   - Use validate-skill.sh for detailed checks
   - Verify all 10 validation phases
   - Check word counts for progressive disclosure
   - Validate file references
   - Test script syntax

3. **Review validation results**
   - Fix any critical errors
   - Address warnings if needed
   - Re-validate after fixes

### Phase 3: Packaging

1. **Execute packaging script**
   ```bash
   python .claude/commands/skills/scripts/package_skill.py <skill-path> [output-dir]
   ```

2. **Package creation process**
   - Script validates again before packaging
   - Creates zip file with skill name
   - Includes all files while maintaining structure
   - Excludes hidden files and __pycache__

3. **Verify package**
   - Check zip file created successfully
   - Note file count and size
   - Confirm ready for distribution

### Phase 4: Distribution Options

1. **Personal distribution**
   - Share zip file directly
   - Upload to file sharing service
   - Include installation instructions

2. **Team distribution**
   - Add to shared repository
   - Upload to team storage
   - Document in team wiki

3. **Public distribution**
   - Upload to skill marketplace (if available)
   - Share on GitHub
   - Add to community repositories

## Command Execution Flow

### Step 1: Locate Skill

```bash
ls ~/.claude/skills/

# Project skills
ls .claude/skills/
```

### Step 2: Quick Validation

```bash
python .claude/commands/skills/scripts/quick_validate.py <skill-path>
```

Expected output:
- ✅ Skill validation passed!
- Or specific error messages to fix

### Step 3: Comprehensive Validation (Optional)

```bash
.claude/commands/skills/scripts/validate-skill.sh <skill-path>
```

Expected output:
- Detailed 10-phase validation report
- Score and recommendations

### Step 4: Package Creation

```bash
# Package to current directory
python .claude/commands/skills/scripts/package_skill.py ~/.claude/skills/my-skill

# Package to specific directory
python .claude/commands/skills/scripts/package_skill.py ~/.claude/skills/my-skill ./dist
```

Expected output:
```
🔍 Validating skill...
✅ Skill validation passed!

// ... (13 lines truncated)
```

## Validation Criteria

The packaging script checks:

### Required Elements
- SKILL.md exists
- Valid YAML frontmatter
- Name and description fields present
- No TODO markers in description

### Quality Checks
- Description in third-person format
- Description mentions when to use skill
- No angle brackets in description
- Word count within limits (<5,000 for SKILL.md)

### Structure Validation
- Proper directory organization
- Scripts are executable
- File references are valid
- No duplicate content between SKILL.md and references/

## Distribution Guidelines

### Installation Instructions Template

Include with your packaged skill:

```markdown
# Installing {{SKILL_NAME}}

## Personal Installation
// ... (13 lines truncated)
```

### Version Management

For skill updates:
1. Update skill files
2. Document changes in SKILL.md
3. Increment version if using versioning
4. Re-package with same process
5. Distribute new package

## Examples

### Example 1: Package Simple Skill

```bash
# Validate first
python .claude/commands/skills/scripts/quick_validate.py ~/.claude/skills/commit-helper

// ... (5 lines truncated)
```

### Example 2: Package to Distribution Directory

```bash
# Create dist directory
mkdir -p ~/skill-packages

// ... (7 lines truncated)
```

### Example 3: Fix and Re-package

```bash
# Initial validation fails
python .claude/commands/skills/scripts/quick_validate.py ~/.claude/skills/my-skill
# ❌ Description contains TODO markers
// ... (12 lines truncated)
```

## Output Format

The command provides clear feedback:

```
📦 Packaging skill: {{skill-name}}

🔍 Validating skill...
// ... (12 lines truncated)
```

## Next Steps

After packaging:
1. **Test installation** on another system
2. **Share with team** or community
3. **Document** in skill catalog
4. **Gather feedback** for improvements
5. **Plan updates** based on usage

## Scripts Used

- `package_skill.py` - Main packaging script
- `quick_validate.py` - Fast validation
- `validate-skill.sh` - Comprehensive validation

Your skill is now ready for distribution! 📦
