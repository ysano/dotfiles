---
description: "Create comprehensive Claude Code Skills through elicitation-driven development"
---

## Instructions

This command orchestrates four specialized agents to build production-ready Claude Code Skills from user requirements. Follow this structured workflow:

### Phase 1: Requirements Elicitation

**Agent**: `skill-elicitation-agent`

1. **Activate Elicitation Agent**
   - Launch the skill-elicitation-agent using the Task tool
   - Provide context: "The user wants to create a new Claude Code Skill"
   - Include any initial requirements or ideas the user has shared

2. **Elicitation Questions**
   - The agent will ask 3-5 targeted questions to understand:
     - Purpose and scope of the skill
     - Complexity and structure requirements
     - Tool permissions needed
     - Context and references
     - Success criteria

3. **Specification Creation**
   - Agent will create a comprehensive skill specification document
   - Includes: metadata, structure, instructions outline, code needs, examples, dependencies
   - Uses progressive disclosure strategy
   - Validates completeness before proceeding

4. **User Approval**
   - Present the specification to the user
   - Confirm understanding and agreement
   - Make adjustments if needed
   - Get explicit approval to proceed to generation

### Phase 2: Skill Generation

**Agent**: `skill-generator-agent`

1. **Activate Generator Agent**
   - Launch the skill-generator-agent using the Task tool
   - Pass the approved specification document
   - Specify target location (personal, project, or plugin skill)

2. **Directory Structure Creation**
   - Agent creates appropriate directory structure:
     ```
     skill-name/
     ├── SKILL.md (required)
     ├── reference.md (if needed)
     ├── examples.md (if needed)
     ├── scripts/ (if needed)
     └── templates/ (if needed)
     ```

3. **SKILL.md Generation**
   - Creates main skill file with proper frontmatter
   - Includes clear instructions and examples
   - Follows progressive disclosure principles
   - Links to supporting files

4. **Supporting Files Generation**
   - Creates reference documentation
   - Generates example files
   - Writes scripts with proper error handling
   - Creates reusable templates

5. **Dependency Documentation**
   - Documents all required packages
   - Provides installation instructions
   - Notes version requirements

6. **Quality Checks**
   - Validates YAML frontmatter
   - Checks file structure
   - Verifies code syntax
   - Tests progressive disclosure

### Phase 3: Validation and Testing

**Agent**: `skill-validator-agent`

1. **Activate Validator Agent**
   - Launch the skill-validator-agent using the Task tool
   - Provide path to generated skill
   - Request comprehensive validation

2. **YAML Validation**
   - Check frontmatter syntax
   - Verify required fields
   - Validate optional fields
   - Test YAML parsing

3. **Description Analysis**
   - Assess discoverability
   - Check trigger keywords
   - Verify clarity and completeness
   - Compare to best practices

4. **Structure Validation**
   - Verify file organization
   - Check all references
   - Test script execution
   - Validate permissions

5. **Code Testing**
   - Syntax validation
   - Security checks
   - Dependency verification
   - Error handling tests

6. **Integration Testing**
   - Test skill loading
   - Verify triggering
   - Check execution flow
   - Validate outputs

7. **Validation Report**
   - Generate comprehensive report
   - Score each category
   - List issues by severity
   - Provide actionable fixes

8. **Issue Resolution**
   - If issues found, work with generator agent to fix
   - Re-validate after fixes
   - Iterate until validation passes

### Phase 4: Documentation Enhancement

**Agent**: `skill-docs-agent`

1. **Activate Documenter Agent**
   - Launch the skill-docs-agent using the Task tool
   - Provide skill path and specification
   - Request comprehensive documentation

2. **SKILL.md Enhancement**
   - Refine instructions for clarity
   - Add comprehensive examples
   - Include best practices
   - Create troubleshooting section

3. **Reference Documentation**
   - Create detailed technical reference (if needed)
   - Document API and configuration
   - Provide advanced patterns
   - Include performance tuning tips

4. **Example Collection**
   - Generate beginner to advanced examples
   - Include troubleshooting examples
   - Show integration patterns
   - Provide case studies

5. **README Creation**
   - Create skill directory README (if distributing)
   - Document installation
   - Provide quick start
   - Link to full documentation

6. **Documentation Quality Check**
   - Verify clarity and completeness
   - Test all code examples
   - Check organization
   - Validate accuracy

### Phase 5: Final Delivery

1. **Generate Summary**
   - List all files created
   - Document location (personal/project/plugin)
   - Provide usage instructions
   - Include test scenarios

2. **Installation Verification**
   - Confirm skill is in correct location
   - Verify file permissions
   - Check dependencies documented
   - Test skill loading

3. **Usage Guide**
   - Explain how to trigger the skill
   - Provide example prompts
   - Show expected behavior
   - Link to documentation

4. **Next Steps**
   - Suggest testing approach
   - Recommend improvements
   - Explain maintenance
   - Note future enhancements

## Agent Coordination

### Sequential Flow
```
User Request
    ↓
skill-elicitation-agent (Requirements)
// ... (13 lines truncated)
```

### Agent Communication

**Between Agents**:
- Elicitation → Generator: Pass specification document
- Generator → Validator: Pass skill location and files
- Validator → Generator: Pass validation issues (if any)
- Generator → Documenter: Pass skill for enhancement
- Documenter → User: Final documentation

**With User**:
- Get approval after elicitation
- Confirm location preference (personal/project)
- Review validation results
- Approve final deliverable

## Location Options

### Personal Skills (`~/.claude/skills/`)
Use for:
- Individual workflows
- Experimental skills
- Personal preferences
- Private tools

### Project Skills (`.claude/skills/`)
Use for:
- Team-shared workflows
- Project-specific expertise
- Version-controlled skills
- Collaborative tools

### Plugin Skills (Plugin directory structure)
Use for:
- Distributable skills
- Marketplace deployment
- Public sharing
- Bundled capabilities

## Output Format

Provide the user with a comprehensive summary:

```
🎉 Skill Created Successfully!

📋 Skill: [Skill Name]
// ... (36 lines truncated)
```

## Examples

### Example 1: Simple Instruction-Only Skill

**Request**: "Create a skill for writing conventional commit messages"

**Flow**:
1. Elicitation asks about commit style, projects, examples
2. Generates simple single-file SKILL.md
3. Validates structure and description
4. Documents with examples and best practices

### Example 2: Multi-File Skill with Scripts

**Request**: "Create a skill for PDF form filling"

**Flow**:
1. Elicitation asks about PDF types, operations, dependencies
2. Generates SKILL.md + scripts/fill_form.py + FORMS.md
3. Validates code execution and file structure
4. Documents with comprehensive examples and API reference

### Example 3: Tool-Restricted Read-Only Skill

**Request**: "Create a skill for security code analysis"

**Flow**:
1. Elicitation determines read-only requirement
2. Generates SKILL.md with allowed-tools: Read, Grep, Glob
3. Validates tool restrictions
4. Documents security patterns and analysis techniques

