---
name: rust:audit-dependencies
description: Deep-dive audit of dependency direction ensuring the Dependency Rule is followed in Rust projects
allowed-tools: Read, Grep, Glob, Bash
author: Quintin Henry (https://github.com/qdhenry/)
---

Deep-dive audit of dependency flow in a Rust codebase, ensuring the Dependency Rule is followed: dependencies must only point inward toward the domain layer.

## Instructions

Audit dependency direction in the codebase: **$ARGUMENTS**

> **Note:** `$ARGUMENTS` can specify a path or module to focus on.
> Examples:
> - `/rust:audit-dependencies` - Audit all dependencies
> - `/rust:audit-dependencies src/application/` - Focus on application layer

---

## The Dependency Rule

```
OUTER → INNER (Allowed)
INNER → OUTER (VIOLATION!)

// ... (5 lines truncated)
```

**Control flow vs Dependency direction:**
- Control flow (runtime): Can go in any direction
- Dependency direction (compile-time): Must point inward ONLY

---

## Phase 1: Map Dependencies

### 1.1 Extract All `use` Statements

```bash
grep -rhn "^use crate::" src/ --include="*.rs" | sort

# Extract all external imports
grep -rhn "^use [a-z]" src/ --include="*.rs" | grep -v "use crate::" | grep -v "use std::" | grep -v "use self::" | grep -v "use super::" | sort
```

### 1.2 Build Dependency Matrix

```markdown
## Dependency Matrix

|              | Domain | Application | Adapters | Infrastructure | External |
// ... (6 lines truncated)
```

---

## Phase 2: Domain Layer Analysis

### 2.1 Domain Dependencies (Should Be Minimal)

```bash
DOMAIN_DIR=$(find src -type d \( -name "domain" -o -name "entities" -o -name "core" \) | head -1)
grep -rhn "^use " $DOMAIN_DIR --include="*.rs" 2>/dev/null
```

### 2.2 Acceptable Domain Imports

```rust
// ACCEPTABLE in domain layer
use std::{...};           // Standard library
use uuid::Uuid;           // Value types
// ... (11 lines truncated)
```

### 2.3 Domain Violation Detection

```bash
# CRITICAL: Domain importing outer layers
grep -rn "use crate::adapters\|use crate::infrastructure\|use crate::application" src/domain/ 2>/dev/null

// ... (9 lines truncated)
```

---

## Phase 3: Application Layer Analysis

### 3.1 Application Dependencies

The application layer should only depend on the domain layer.

```bash
APP_DIR=$(find src -type d \( -name "application" -o -name "use_cases" -o -name "usecases" -o -name "services" \) | head -1)
grep -rhn "^use crate::" $APP_DIR --include="*.rs" 2>/dev/null
```

### 3.2 Acceptable Application Imports

```rust
// ACCEPTABLE in application layer
use crate::domain::{...};    // Domain types
use std::{...};              // Standard library
// ... (9 lines truncated)
```

### 3.3 Application Violation Detection

```bash
# HIGH: Application importing adapters
grep -rn "use crate::adapters" src/application/ 2>/dev/null

// ... (9 lines truncated)
```

---

## Phase 4: Adapters Layer Analysis

### 4.1 Adapter Dependencies

Adapters should depend on the application layer (implementing its ports) and can use external frameworks.

```bash
ADAPTER_DIR=$(find src -type d \( -name "adapters" -o -name "handlers" -o -name "api" -o -name "controllers" \) | head -1)
grep -rhn "^use crate::" $ADAPTER_DIR --include="*.rs" 2>/dev/null
```

### 4.2 Adapter Import Analysis

```rust
// ACCEPTABLE in adapters layer
use crate::application::{...};  // Ports, use cases, app errors
use crate::domain::{...};       // Domain types (for conversion)
// ... (7 lines truncated)
```

### 4.3 Check Adapter-Port Implementation

```bash
grep -rn "impl.*for" src/adapters/ 2>/dev/null
grep -rn "impl.*Repository.*for\|impl.*Service.*for\|impl.*Port.*for" src/adapters/ 2>/dev/null
```

---

## Phase 5: Cargo.toml Analysis

### 5.1 Dependency Audit

```bash
cat Cargo.toml | grep -A100 "\[dependencies\]" | grep -B100 "\[dev-dependencies\]" 2>/dev/null || cat Cargo.toml
```

### 5.2 Workspace Analysis (Multi-Crate)

```bash
grep -A10 "\[workspace\]" Cargo.toml 2>/dev/null

// ... (6 lines truncated)
```

### 5.3 Ideal Dependency Structure

**Single-Crate:** Domain-safe deps (`uuid`, `chrono`, `thiserror`), application deps (`async-trait`), adapter deps (`axum`, `sqlx`, `serde`), infra deps (all wiring).

**Multi-Crate Workspace (Ideal):**
```
my-project/
├── domain/           # uuid, chrono only
├── application/      # domain + async-trait
├── adapters/         # application + frameworks
└── infrastructure/   # all dependencies
```

---

## Phase 6: Generate Violation Report

### 6.1 Violation Categories

```markdown
## Dependency Violations Report

### CRITICAL (Layer Boundary Violations)
// ... (21 lines truncated)
```

### 6.2 Dependency Graph Visualization

```
Domain
  └── (external: uuid, chrono)

// ... (14 lines truncated)
```

---

## Quick Commands

```bash
# Find all violations at once
echo "=== CRITICAL: Domain importing outer ===" && \
grep -rn "use crate::adapters\|use crate::infrastructure\|use crate::application" src/domain/ 2>/dev/null
// ... (7 lines truncated)
```

---

## Remediation Guide

### Violation: Domain imports outer layer

**Before:**
```rust
// src/domain/user.rs
use crate::adapters::db::UserRow;  // VIOLATION
```

**After:**
```rust
// src/domain/user.rs
// Domain knows nothing about persistence
// Conversion happens in adapter layer
```

### Violation: Application imports adapters

**Before:**
```rust
// src/application/register_user.rs
use crate::adapters::PostgresUserRepo;  // VIOLATION
pub struct RegisterUser {
    repo: PostgresUserRepo,
}
```

**After:**
```rust
// src/application/register_user.rs
use crate::application::ports::UserRepository;  // Port trait
pub struct RegisterUser {
    repo: Arc<dyn UserRepository>,  // Trait object
}
```

---
