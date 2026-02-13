# Code Formatting Setup

Configure code formatting tools for consistent code style across the team.

## 1. Select Formatting Tools

**JavaScript/TypeScript**: Prettier
**Python**: Black, isort
**Java**: Google Java Format, Spotless
**Go**: gofmt, goimports
**Rust**: rustfmt

## 2. Install Tools

JavaScript/TypeScript:
```bash
npm install -D prettier
```

Python:
```bash
pip install black isort
```

## 3. Configuration

**Prettier (.prettierrc)**:
```json
{
  "semi": true,
  "singleQuote": true,
  "tabWidth": 2,
  "trailingComma": "es5",
  "printWidth": 80
}
```

**Black (pyproject.toml)**:
```toml
[tool.black]
line-length = 88
target-version = ["py38"]
```

## 4. IDE Integration

- Install formatter extensions
- Enable format on save
- Configure keyboard shortcuts
- Set up format on paste (optional)

## 5. Scripts and Automation

Add to package.json:
```json
{
  "scripts": {
    "format": "prettier --write .",
    "format:check": "prettier --check ."
  }
}
```

## 6. Pre-commit Hooks

```bash
npm install -D husky lint-staged
```

Configure .lintstagedrc:
```json
{
  "*.{js,ts,tsx}": ["prettier --write", "eslint --fix"]
}
```

## 7. CI/CD Integration

Add formatting check to CI pipeline:
```yaml
- name: Check formatting
  run: npm run format:check
```

## 8. Editor Config

Create .editorconfig for cross-editor consistency:
```ini
root = true

[*]
charset = utf-8
end_of_line = lf
insert_final_newline = true
indent_style = space
indent_size = 2
```

## 9. Ignore Patterns

Create .prettierignore:
```
node_modules/
dist/
build/
coverage/
*.min.js
```

## 10. Team Onboarding

- Document formatting setup in README.md
- Create setup script for new developers
- Explain rationale for formatting choices
- Establish process for formatting rule changes

See also: `setup-linting.md`
