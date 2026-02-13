# Linting Setup

Configure linting tools for code quality enforcement and error prevention.

## 1. Project Analysis

- Identify programming languages and frameworks
- Check existing linting configuration
- Review current code style and patterns
- Assess team preferences and requirements

## 2. Tool Selection

**JavaScript/TypeScript**: ESLint + TypeScript ESLint
**Python**: flake8, pylint, mypy
**Java**: Checkstyle, SpotBugs, PMD
**Go**: golangci-lint
**Rust**: clippy

## 3. Installation

JavaScript/TypeScript:
```bash
npm install -D eslint @typescript-eslint/parser @typescript-eslint/eslint-plugin
npm install -D prettier eslint-config-prettier eslint-plugin-prettier
```

Python:
```bash
pip install flake8 pylint mypy
```

## 4. Configuration

**ESLint (.eslintrc.json)**:
```json
{
  "extends": [
    "eslint:recommended",
    "@typescript-eslint/recommended",
    "prettier"
  ],
  "parser": "@typescript-eslint/parser",
  "plugins": ["@typescript-eslint"],
  "rules": {
    "no-console": "warn",
    "no-unused-vars": "error",
    "@typescript-eslint/no-explicit-any": "warn"
  }
}
```

## 5. IDE Integration

- Install linting extensions
- Configure auto-fix on save
- Set up inline error display
- Configure keyboard shortcuts for fixing issues

## 6. Scripts

Add to package.json:
```json
{
  "scripts": {
    "lint": "eslint src --ext .ts,.tsx",
    "lint:fix": "eslint src --ext .ts,.tsx --fix"
  }
}
```

## 7. Pre-commit Hooks

Configure lint-staged to run linting before commits:
```json
{
  "*.{js,ts,tsx}": ["eslint --fix", "prettier --write"]
}
```

## 8. CI/CD Integration

Add linting check to CI pipeline:
```yaml
- name: Lint code
  run: npm run lint
```

## 9. Custom Rules

- Define project-specific linting rules
- Document rule rationale
- Configure severity levels (error, warn, off)
- Set up rule exceptions with inline comments when necessary

## 10. Team Standards

- Document linting setup in README.md
- Create style guide based on linting rules
- Establish process for rule changes
- Set up automated rule documentation generation

See also: `setup-formatting.md`
