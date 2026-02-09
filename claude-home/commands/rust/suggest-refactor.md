---
name: rust:suggest-refactor
description: Actionable refactoring suggestions for improving clean architecture compliance in Rust projects
allowed-tools: Read, Grep, Glob, Bash, Edit
author: Quintin Henry (https://github.com/qdhenry/)
---

Analyze a Rust codebase and provide actionable refactoring suggestions to improve clean architecture compliance, with prioritized recommendations and code examples.

## Instructions

Suggest refactoring improvements for: **$ARGUMENTS**

> **Note:** `$ARGUMENTS` can specify focus areas or severity.
> Examples:
> - `/rust:suggest-refactor` - All suggestions
> - `/rust:suggest-refactor --critical` - Only critical issues
> - `/rust:suggest-refactor src/application/` - Focus on specific path

---

## Refactoring Priority Framework

| Priority | Description | When to Fix |
|----------|-------------|-------------|
| **P0 - Critical** | Architecture violations blocking testability/maintainability | Immediately |
| **P1 - High** | Dependency rule violations | This sprint |
| **P2 - Medium** | Missing abstractions, unclear boundaries | Next sprint |
| **P3 - Low** | Style, organization improvements | When touching code |

---

## Phase 1: Identify Current Issues

### 1.1 Run Quick Audit

```bash
DOMAIN_VIOLATIONS=$(grep -rn "use crate::adapters\|use crate::infrastructure\|use axum\|use sqlx" src/domain/ 2>/dev/null | wc -l)

# Application violations
// ... (6 lines truncated)
```

### 1.2 Categorize Findings

Group issues by type for prioritized remediation.

---

## Phase 2: P0 - Critical Refactorings

### 2.1 Domain Layer Depends on Outer Layers

**Problem:** Domain imports adapters/infrastructure

**Detection:**
```bash
grep -rn "use crate::adapters\|use crate::infrastructure" src/domain/ 2>/dev/null
```

**Before:**
```rust
// src/domain/user.rs
use crate::adapters::db::UserRow;  // VIOLATION

// ... (7 lines truncated)
```

**After:**
```rust
// src/domain/user.rs
pub struct User {
    // Pure domain entity
// ... (7 lines truncated)
```

**Refactoring Steps:**
1. Remove adapter imports from domain
2. Move conversion logic to adapter layer
3. Ensure domain only has inward dependencies

---

### 2.2 Application Layer Depends on Concrete Adapters

**Problem:** Use cases depend on concrete implementations instead of traits

**Detection:**
```bash
grep -rn "PostgresUserRepository\|Argon2Hasher\|RedisCache" src/application/ 2>/dev/null
```

**Refactoring Steps:**
1. Define port traits in `application/ports/`
2. Update use case to depend on `Arc<dyn Trait>`
3. Update adapter to `impl Trait for ConcreteType`
4. Wire dependencies in infrastructure

---

### 2.3 Framework Types in Application Layer

**Problem:** HTTP/DB types leaked into use cases

**Detection:**
```bash
grep -rn "Json<\|HttpResponse\|StatusCode\|Query<\|Path<\|PgPool" src/application/ 2>/dev/null
```

**Refactoring Steps:**
1. Remove framework imports from application layer
2. Use primitive/domain types in use case signatures
3. Create DTOs in adapter layer
4. Handle conversion in HTTP handlers

---

## Phase 3: P1 - High Priority Refactorings

### 3.1 Missing Port Abstraction

**Problem:** Direct external service calls without trait abstraction

**Detection:**
```bash
grep -rn "reqwest::\|lettre::\|aws_sdk" src/application/ 2>/dev/null
```

**Refactoring Steps:**
1. Define trait in application/ports
2. Update use case to depend on trait
3. Create adapter implementing the trait
4. Wire in infrastructure

---

### 3.2 Business Logic in Adapters

**Problem:** Validation/business rules in HTTP handlers

**Detection:**
```bash
grep -rn "if.*validate\|if.*len()\|if.*contains\|if.*<.*>" src/adapters/ 2>/dev/null
```

**Before:**
```rust
// src/adapters/http/handlers.rs
pub async fn register_handler(Json(dto): Json<RegisterDto>) -> impl IntoResponse {
    // Business logic in handler!
// ... (7 lines truncated)
```

**After:**
```rust
// src/domain/value_objects/email.rs
pub struct Email(String);

// ... (7 lines truncated)
```

---

## Phase 4: P2 - Medium Priority Refactorings

### 4.1 Serialization in Domain Entities

**Problem:** `Serialize`/`Deserialize` derives on domain types

**Detection:**
```bash
grep -rn "#\[derive.*Serialize\|#\[derive.*Deserialize" src/domain/ 2>/dev/null
```

Move serialization derives to adapter-layer DTOs; domain entities should use only `#[derive(Debug, Clone, PartialEq, Eq, Hash)]`.

---

### 4.2 Missing Error Type Layering

**Problem:** Single error type used across all layers

**Detection:**
```bash
grep -rn "pub enum.*Error" src/ | wc -l
# If only 1-2 error types exist, layering may be missing
```

**Recommended Structure:** `DomainError` (domain), `AppError` (application, wraps domain errors), `AdapterError` / HTTP status mapping (adapters).

---

## Phase 5: P3 - Low Priority Improvements

### 5.1 Module Organization

Organize by feature within layers.

### 5.2 Add Test Infrastructure

```rust
// src/application/ports/mod.rs
#[cfg(test)]
pub mod mocks {
    pub struct MockUserRepository { /* ... */ }
    pub struct MockPasswordHasher { /* ... */ }
}
```

---

## Phase 6: Generate Refactoring Plan

### 6.1 Prioritized Task List

```markdown
# Clean Architecture Refactoring Plan

## P0 - Critical (Do First)
// ... (7 lines truncated)
```

---
