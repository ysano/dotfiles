---
name: rust:audit-clean-arch
description: Comprehensive audit of Rust codebase against Clean Architecture principles, identifying violations and improvement opportunities
allowed-tools: Read, Grep, Glob, Bash
author: Quintin Henry (https://github.com/qdhenry/)
---

Perform a comprehensive audit of a Rust codebase against Clean Architecture principles, identifying violations, anti-patterns, and improvement opportunities.

## Instructions

Audit the Rust codebase for clean architecture compliance: **$ARGUMENTS**

> **Note:** `$ARGUMENTS` can specify a path, specific concerns, or be empty for full audit.
> Examples:
> - `/rust:audit-clean-arch` - Full audit of current directory
> - `/rust:audit-clean-arch src/` - Audit specific path
> - `/rust:audit-clean-arch --focus dependencies` - Focus on dependency violations

---

## Clean Architecture Reference

```
┌─────────────────────────────────────────────────────────────┐
│                     Infrastructure                          │
│  (Setup, Config, DB Pools, Environment, Third-party Deps)  │
// ... (20 lines truncated)
```

---

## Phase 1: Structure Discovery

### 1.1 Identify Project Layout

```bash
find . -name "*.rs" -type f | head -50

# Look for common clean architecture patterns
// ... (7 lines truncated)
```

### 1.2 Identify Layer Mapping

Map discovered directories to architectural layers:

| Layer | Common Directory Names |
|-------|----------------------|
| **Domain** | `domain/`, `entities/`, `models/`, `core/domain/` |
| **Application** | `application/`, `use_cases/`, `usecases/`, `services/` (business), `core/` |
| **Adapters** | `adapters/`, `api/`, `handlers/`, `controllers/`, `repositories/`, `persistence/`, `http/`, `grpc/` |
| **Infrastructure** | `infrastructure/`, `infra/`, `config/`, `db/`, `main.rs` setup code |

---

## Phase 2: Dependency Direction Audit

### 2.1 Analyze Module Dependencies

For each layer, check what it imports:

```bash
# Check domain layer imports (should be minimal)
grep -rn "^use " src/domain/ 2>/dev/null | grep -v "use crate::domain" | grep -v "use std::" | grep -v "use uuid::"

// ... (6 lines truncated)
```

### 2.2 Dependency Violation Checklist

**CRITICAL VIOLATIONS (Red Flags):**

| Violation | Pattern to Find | Severity |
|-----------|-----------------|----------|
| Domain imports adapters | `use crate::adapters` in domain/ | CRITICAL |
| Domain imports infrastructure | `use crate::infrastructure` in domain/ | CRITICAL |
| Domain imports application | `use crate::application` in domain/ | CRITICAL |
| Application imports adapters | `use crate::adapters` in application/ | HIGH |
| Application imports infrastructure | `use crate::infrastructure` in application/ | HIGH |
| Domain depends on web framework | `use axum`, `use actix`, `use rocket` in domain/ | CRITICAL |
| Domain depends on DB | `use sqlx`, `use diesel`, `use sea_orm` in domain/ | CRITICAL |

### 2.3 Cargo.toml Analysis

```bash
cat Cargo.toml
grep -A5 "\[features\]" Cargo.toml
```

**Ideal Dependency Separation:** Domain crate (only `uuid`, `chrono`, `thiserror`), Application crate (Domain + `async-trait`), Adapters crate (Application + framework deps), Infrastructure (all dependencies for wiring).

---

## Phase 3: Ports & Adapters Pattern Audit

### 3.1 Port Trait Detection

```bash
# Find async traits (common for repository ports)
grep -rn "async_trait" src/

// ... (6 lines truncated)
```

### 3.2 Port Compliance Checklist

| Criteria | Status | Notes |
|----------|--------|-------|
| Repository traits defined in application layer | | |
| External service traits defined in application layer | | |
| Adapters implement application-layer traits | | |
| Use cases depend on trait objects, not concrete types | | |
| Dependency injection via constructor | | |

### 3.3 Anti-Pattern Detection

```bash
# Look for concrete DB types in use cases
grep -rn "Postgres\|Mysql\|Sqlite\|Pool" src/application/ 2>/dev/null

# Look for framework types in application layer
grep -rn "axum::\|actix::\|rocket::" src/application/ 2>/dev/null
```

---

## Phase 4: Entity & Domain Logic Audit

### 4.1 Domain Entity Analysis

```bash
grep -rn "^pub struct" src/domain/ 2>/dev/null
grep -rn "#\[derive" src/domain/ 2>/dev/null
```

### 4.2 Domain Purity Checklist

| Criteria | Status | Notes |
|----------|--------|-------|
| Entities have no framework dependencies | | |
| Entities encapsulate business rules | | |
| Value objects are immutable | | |
| Domain services contain cross-entity logic | | |
| No serialization concerns in domain | | |

**Acceptable derives:** `Debug, Clone, PartialEq, Eq, Hash`
**Problematic derives:** `Serialize, Deserialize` (adapter concern), `sqlx::FromRow` (DB concern)

### 4.3 Business Logic Placement

```bash
grep -rn "if.*&&\|match\|fn.*validate\|fn.*calculate" src/adapters/ 2>/dev/null
```

Business logic should be in domain entities, domain services, or application use cases -- NOT in HTTP handlers, DB repositories, or serializers.

---

## Phase 5: Use Case Analysis

### 5.1 Use Case Structure Audit

```bash
grep -rn "pub struct.*UseCase\|pub struct.*Service\|pub async fn execute" src/application/ 2>/dev/null
```

### 5.2 Use Case Checklist

| Criteria | Status | Notes |
|----------|--------|-------|
| Use cases are single-purpose | | |
| Use cases accept trait dependencies | | |
| Use cases return domain types or app errors | | |
| No HTTP/serialization concerns in use cases | | |
| Use cases are testable in isolation | | |

### 5.3 Use Case Anti-Patterns

```rust
// ANTI-PATTERN: Use case with HTTP concerns
pub async fn execute(&self, req: HttpRequest) -> HttpResponse { ... }

// CORRECT: Use case with domain types
pub async fn execute(&self, username: String, password: String) -> Result<User, AppError> { ... }
```

---

## Phase 6: Error Handling Audit

### 6.1 Error Type Analysis

```bash
grep -rn "enum.*Error\|struct.*Error" src/ | grep -v target/
```

### 6.2 Error Layering Checklist

| Layer | Error Type | Converts To |
|-------|-----------|-------------|
| Domain | `DomainError` | - |
| Application | `AppError` | Contains domain errors |
| Adapters | `AdapterError` → HTTP Status | Converts from AppError |

```bash
grep -rn "impl From<.*Error>" src/
```

---

## Phase 7: Generate Audit Report

### 7.1 Report Template

```markdown
# Clean Architecture Audit Report

**Project:** [Name]
// ... (50 lines truncated)
```rust
// problematic code
```

### Recommended
```rust
// improved code
```
```

---
