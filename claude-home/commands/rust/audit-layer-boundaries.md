---
name: rust:audit-layer-boundaries
description: Analyze layer boundary definitions in Rust codebases, identifying mixed responsibilities and violations
allowed-tools: Read, Grep, Glob, Bash
author: Quintin Henry (https://github.com/qdhenry/)
---

Analyze how well-defined the architectural layer boundaries are in a Rust codebase, identifying mixed responsibilities, unclear separation, and boundary violations.

## Instructions

Audit layer boundaries in the codebase: **$ARGUMENTS**

> **Note:** `$ARGUMENTS` can specify a path or focus area.
> Examples:
> - `/rust:audit-layer-boundaries` - Full boundary audit
> - `/rust:audit-layer-boundaries src/core/` - Focus on specific path

---

## Layer Responsibility Reference

| Layer | Responsibility | Should NOT Contain |
|-------|---------------|-------------------|
| **Domain** | Entities, value objects, domain services, business rules | Serialization, HTTP, DB queries, config |
| **Application** | Use cases, orchestration, port definitions, app errors | HTTP handlers, DB implementations, framework types |
| **Adapters** | HTTP handlers, DB repositories, external API clients, serialization | Business logic, configuration setup |
| **Infrastructure** | App startup, config loading, DB pools, dependency wiring | Business logic, HTTP handling |

---

## Phase 1: Identify Layer Structure

### 1.1 Discover Project Organization

```bash
ls -la src/

# Find all modules
// ... (8 lines truncated)
```

### 1.2 Document Layer Mapping

```markdown
## Current Layer Structure

| Identified Directory | Mapped Layer | Confidence |
// ... (7 lines truncated)
```

---

## Phase 2: Domain Layer Boundary Audit

### 2.1 Domain Purity Check

The domain should contain ONLY business logic.

```bash
# Find all files in domain layer
find src/domain -name "*.rs" 2>/dev/null

// ... (14 lines truncated)
```

### 2.2 Domain Should Contain

```rust
// GOOD: Domain content
pub struct User { ... }              // Entities
pub struct Email(String);            // Value objects
pub fn validate_email(s: &str) -> bool { ... }  // Domain rules
pub trait DomainService { ... }      // Domain services
pub enum DomainError { ... }         // Domain errors
```

### 2.3 Domain Should NOT Contain

```rust
// BAD: HTTP in domain
#[derive(Deserialize)]               // Serialization concern
pub struct CreateUserRequest { ... }
// ... (8 lines truncated)
```

---

## Phase 3: Application Layer Boundary Audit

### 3.1 Application Layer Content Check

```bash
# Find application layer files
find src/application -name "*.rs" 2>/dev/null

// ... (14 lines truncated)
```

### 3.2 Application Should Contain

```rust
// GOOD: Application content
pub struct RegisterUser { ... }      // Use cases
pub trait UserRepository { ... }     // Port traits
pub enum AppError { ... }            // Application errors
pub async fn execute(&self) { ... }  // Use case execution
```

### 3.3 Application Should NOT Contain

```rust
// BAD: HTTP handlers in application
pub async fn register_handler(Json(body): Json<...>) -> impl IntoResponse { ... }

// ... (8 lines truncated)
```

---

## Phase 4: Adapters Layer Boundary Audit

### 4.1 Adapter Content Check

```bash
# Find adapter layer files
find src/adapters -name "*.rs" 2>/dev/null

// ... (11 lines truncated)
```

### 4.2 Adapters Should Contain

```rust
// GOOD: Adapter content
impl UserRepository for PostgresUserRepository { ... }  // Port implementations
pub async fn register_handler(...) -> impl IntoResponse { ... }  // HTTP handlers
pub fn app_error_to_response(e: AppError) -> Response { ... }  // Error conversion
pub struct CreateUserDto { ... }     // DTOs with serialization
```

### 4.3 Adapters Should NOT Contain

```rust
// BAD: Business logic in adapter
pub async fn register_handler(...) {
    // Business rules should be in use case
// ... (8 lines truncated)
```

---

## Phase 5: Infrastructure Layer Boundary Audit

### 5.1 Infrastructure Content Check

```bash
# Find infrastructure/main.rs
cat src/main.rs 2>/dev/null | head -100
find src/infrastructure -name "*.rs" 2>/dev/null
```

### 5.2 Infrastructure Should Contain

```rust
// GOOD: Infrastructure content
fn main() { ... }                    // Entry point
pub fn load_config() { ... }         // Configuration
pub async fn create_pool() { ... }   // Database setup
pub fn create_app_state() { ... }    // Dependency injection
tracing_subscriber::init();          // Telemetry setup
```

### 5.3 Infrastructure Should NOT Contain

```rust
// BAD: Business logic in main.rs
fn main() {
    // Business rules don't belong here
// ... (6 lines truncated)
```

---

## Phase 6: Cross-Cutting Concern Analysis

### 6.1 Check for Mixed Files

Files that mix multiple layers are red flags:

```bash
# Look for files with both HTTP and DB
for file in $(find src -name "*.rs"); do
  has_http=$(grep -l "axum::\|actix::\|HttpResponse" "$file" 2>/dev/null)
// ... (7 lines truncated)
```

### 6.2 Check Module Cohesion

```bash
# Files with too many imports from different layers
for file in $(find src -name "*.rs"); do
  layer_count=0
// ... (10 lines truncated)
```

---

## Phase 7: Generate Boundary Report

### 7.1 Report Template

```markdown
# Layer Boundary Audit Report

**Project:** [Name]
// ... (35 lines truncated)
```

---

## Quick Boundary Check Commands

```bash
# Domain purity
echo "=== Domain HTTP/DB violations ===" && \
grep -rn "axum::\|sqlx::\|Serialize\|Deserialize" src/domain/ 2>/dev/null
// ... (9 lines truncated)
```

---

## Usage

```bash
/rust:audit-layer-boundaries
/rust:audit-layer-boundaries src/
```
