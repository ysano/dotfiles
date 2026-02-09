---
description: "Review and improve system architecture"
---

## Instructions

Perform a comprehensive architectural analysis of the codebase. Think harder about trade-offs and systemic issues.

Scope: `$ARGUMENTS` (default: entire codebase)

1. **Map Architecture**: Identify patterns (MVC, Clean Architecture, etc.), module boundaries, layered structure
2. **Assess Design Quality**: Check dependency direction, coupling, circular dependencies, SRP adherence
3. **Trace Data Flow**: State management, persistence strategies, validation/transformation
4. **Evaluate Resilience**: Error handling consistency, fault tolerance, recovery patterns
5. **Assess Scalability**: Caching, stateless design, performance bottlenecks
6. **Review Security**: Trust zones, auth architecture, data protection
7. **Check Testability**: Test structure, mocking strategies, coverage across layers

Output a structured report:

```
## Architecture Review: [Project/Module]

### Architecture Overview
[Pattern, layers, key components]

### Strengths
- [What works well with file:line references]

### Issues (by severity)
#### Critical
- [Issue]: [Impact] → [Recommended fix]

#### Moderate
- [Issue]: [Impact] → [Recommended fix]

### Improvement Roadmap
1. [Immediate actions]
2. [Short-term improvements]
3. [Long-term evolution]
```
