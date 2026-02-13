# Dependency Modernization

Update and modernize project dependencies safely and systematically.

## 1. Dependency Audit

Check outdated packages:
```bash
npm outdated
pip list --outdated
composer outdated
```

Security audit:
```bash
npm audit
pip-audit
```

## 2. Update Strategy

Follow this progression:
1. Patch updates (1.2.3 → 1.2.4)
2. Minor updates (1.2.3 → 1.3.0)
3. Major updates (1.2.3 → 2.0.0)

Test thoroughly between each step.

## 3. Automated Updates

Safe updates:
```bash
npm update
pip install -U package-name
```

Interactive updates:
```bash
npx npm-check-updates -i
```

## 4. Breaking Changes Review

- Read changelogs and migration guides
- Identify deprecated APIs
- Plan code changes needed
- Update tests and documentation

## 5. Testing and Validation

```bash
npm test
npm run build
npm run lint
```

Verify all quality checks pass before proceeding.

## 6. Documentation

- Update README.md
- Revise installation instructions
- Update API documentation
- Note breaking changes in CHANGELOG.md

## 7. Dependency Health Monitoring

- Set up automated dependency updates (Dependabot, Renovate)
- Configure security vulnerability alerts
- Schedule regular dependency review cycles
- Track technical debt related to outdated dependencies

## 8. Version Pinning Strategy

- Production: Pin exact versions for stability
- Development: Allow minor/patch updates
- Document version constraints rationale
- Use lock files (package-lock.json, yarn.lock)

See also: `security` Skill `references/dependency-audit.md`
