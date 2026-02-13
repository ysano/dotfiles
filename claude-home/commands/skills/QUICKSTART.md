---
description: "Build production-ready Claude Code Skills in minutes!"
---

## Fastest Path to Success

### Step 1: Start Building

```bash
/skills:build-skill
```

That's it! The system will guide you through everything.

## What Happens Next

### Phase 1: Requirements (2-3 minutes)

**skill-elicitation-agent** asks you questions:

```
Example conversation:

Agent: "What specific task or workflow should this skill help with?"
// ... (16 lines truncated)
```

### Phase 2: Generation (1-2 minutes)

**skill-generator-agent** builds your skill:

```
Creating skill structure...
✅ Created commit-helper/SKILL.md
✅ Added frontmatter with proper metadata
// ... (6 lines truncated)
```

### Phase 3: Validation (30 seconds)

**skill-validator-agent** tests everything:

```
Running 10 validation checks...
✅ YAML frontmatter valid
✅ Description clear and discoverable
// ... (6 lines truncated)
```

### Phase 4: Documentation (1 minute)

**skill-docs-agent** enhances docs:

```
Enhancing documentation...
✅ Added 5 comprehensive examples
✅ Created best practices section
// ... (5 lines truncated)
```

## First-Time Examples

### Example 1: Simple Instruction Skill (5 minutes)

**What**: Skill for commit messages

```bash
/skills:build-skill
```

**Questions You'll Answer**:
- What format? → "Conventional Commits"
- When to use? → "When committing code"
- Any code needed? → "No, just instructions"

**What You Get**:
```
~/.claude/skills/commit-helper/
└── SKILL.md
```

**How to Use**:
```
"Help me write a commit message for my changes"
```

### Example 2: Multi-File Skill with Scripts (10 minutes)

**What**: PDF form filling skill

```bash
/skills:build-skill
```

**Questions You'll Answer**:
- What operations? → "Fill forms, extract fields"
- Which libraries? → "pypdf, pdfplumber"
- Execute code? → "Yes, need Python scripts"

**What You Get**:
```
~/.claude/skills/pdf-form-filler/
├── SKILL.md
├── FORMS.md
// ... (5 lines truncated)
```

**How to Use**:
```
"Fill out this PDF form with my data"
```

### Example 3: Read-Only Safety Skill (7 minutes)

**What**: Security code analyzer

```bash
/skills:build-skill
```

**Questions You'll Answer**:
- Analysis scope? → "OWASP Top 10"
- Modify files? → "No, read-only"
- Output format? → "Security report"

**What You Get**:
```
~/.claude/skills/security-analyzer/
├── SKILL.md (with tool restrictions)
├── PATTERNS.md
└── REFERENCE.md
```

**How to Use**:
```
"Analyze this code for security vulnerabilities"
```

## Common Questions

### Q: Where do skills get created?

**A**: By default in `~/.claude/skills/` (personal) or `.claude/skills/` (project).
The elicitation agent will ask which you prefer.

### Q: Can I edit a skill after creation?

**A**: Yes! Just edit the files directly. Use the validation script to check:
```bash
.claude/commands/skills/scripts/validate-skill.sh ~/.claude/skills/my-skill
```

### Q: How do I know if my skill will trigger?

**A**: Test it with:
```bash
.claude/commands/skills/scripts/test-skill-trigger.sh ~/.claude/skills/my-skill
```

### Q: What if validation fails?

**A**: The validator will tell you exactly what to fix. The generator agent can help make the changes.

### Q: Can I use templates directly?

**A**: Yes! Templates are in `.claude/commands/skills/templates/`. But using `/skills:build-skill` is recommended as it ensures quality.

## Advanced Usage

### Creating Project-Wide Skills

When asked about location, choose **project** instead of personal:

```
Location: .claude/skills/

Then commit to git:
git add .claude/skills/my-skill
git commit -m "Add my-skill for team"
git push
```

Team members get the skill automatically on pull.

### Creating Plugin Skills

For distribution via plugins:

1. Create skill with `/skills:build-skill`
2. Move to plugin structure
3. Publish plugin to marketplace

See: https://docs.anthropic.com/claude-code/plugins

### Using Templates Directly

For quick prototyping:

```bash
cp .claude/commands/skills/templates/simple-skill-template.md \
   ~/.claude/skills/my-skill/SKILL.md

// ... (6 lines truncated)
```

But `/skills:build-skill` is recommended for production use.

## What's Next?

### Learn More

- **Full Documentation**: See `.claude/commands/skills/README.md`
- **Agent Details**: See `.claude/agents/skill-builder/README.md`
- **Official Docs**: https://docs.anthropic.com/claude-code/skills

### Build Your First Skill

Ready? Let's go:

```bash
/skills:build-skill
```

The agents will guide you through everything!

### Share Your Skills

Once created:
- Share with team via git
- Distribute as plugins
- Contribute to community repos

---

**Happy skill building! Transform your workflows into reusable expertise.** 🚀
