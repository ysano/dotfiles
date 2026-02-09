---
name: rust:audit-ports-adapters
description: Audit Ports and Adapters (Hexagonal Architecture) pattern implementation in Rust codebases
allowed-tools: Read, Grep, Glob, Bash
author: Quintin Henry (https://github.com/qdhenry/)
---

Analyze the implementation of the Ports & Adapters (Hexagonal Architecture) pattern in a Rust codebase, checking for proper trait definitions, dependency inversion, and adapter implementations.

## Instructions

Audit ports and adapters implementation: **$ARGUMENTS**

> **Note:** `$ARGUMENTS` can specify focus areas.
> Examples:
> - `/rust:audit-ports-adapters` - Full audit
> - `/rust:audit-ports-adapters repositories` - Focus on repository ports

---

## Ports & Adapters Concept

```
                    ┌─────────────────────┐
                    │     Application     │
                    │                     │
// ... (15 lines truncated)
```

**Key Principle:** Use cases depend on PORT TRAITS, not concrete adapters.

---

## Phase 1: Port Discovery

### 1.1 Find Port Trait Definitions

Ports should be defined in the application layer:

```bash
grep -rn "#\[async_trait\]" src/ --include="*.rs"

# Find trait definitions in application layer
// ... (5 lines truncated)
```

### 1.2 Port Location Audit

**Ports MUST be defined in the application layer, NOT in adapters:**

```bash
# CORRECT: Ports in application
grep -rn "^pub trait" src/application/ 2>/dev/null

# VIOLATION: Ports defined in adapters
grep -rn "^pub trait" src/adapters/ 2>/dev/null
```

### 1.3 Document Discovered Ports

```markdown
## Discovered Ports

| Port Trait | Location | Purpose | Status |
// ... (5 lines truncated)
```

---

## Phase 2: Port Quality Analysis

### 2.1 Check Port Trait Design

Good ports should:
- Use domain types in signatures
- Be async for I/O operations
- Have clear, focused responsibilities

```bash
# Extract port trait definitions
grep -A20 "^pub trait.*Repository\|^pub trait.*Service\|^pub trait.*Port" src/application/ 2>/dev/null
```

### 2.2 Port Design Checklist

| Criteria | Status | Notes |
|----------|--------|-------|
| Uses domain types (not DTOs) | | |
| Async for I/O operations | | |
| Single responsibility | | |
| No framework types in signature | | |
| Error types are domain/app errors | | |

### 2.3 Good vs Bad Port Examples

```rust
// GOOD PORT: Uses domain types, async, focused
#[async_trait]
pub trait UserRepository: Send + Sync {
// ... (13 lines truncated)
```

---

## Phase 3: Adapter Discovery

### 3.1 Find Adapter Implementations

```bash
# Find trait implementations in adapters
grep -rn "impl.*for" src/adapters/ 2>/dev/null

# Specifically find port implementations
grep -rn "impl.*Repository.*for\|impl.*Service.*for\|impl.*Port.*for\|impl.*Gateway.*for" src/adapters/ 2>/dev/null
```

### 3.2 Map Ports to Adapters

```markdown
## Port-Adapter Mapping

| Port | Adapter(s) | Location | Status |
// ... (6 lines truncated)
```

### 3.3 Check Adapter Completeness

```bash
# For each port, verify an adapter exists
for port in $(grep -roh "trait \w*Repository\|trait \w*Service\|trait \w*Port" src/application/ 2>/dev/null | cut -d' ' -f2); do
  echo "Port: $port"
  impl_count=$(grep -rn "impl.*$port.*for" src/adapters/ 2>/dev/null | wc -l)
  echo "  Implementations found: $impl_count"
done
```

---

## Phase 4: Dependency Inversion Check

### 4.1 Use Case Dependency Analysis

Use cases should depend on trait objects, not concrete types:

```bash
# Find use case structs
grep -A10 "^pub struct.*UseCase\|^pub struct.*Service\|^struct.*UseCase" src/application/ 2>/dev/null

// ... (6 lines truncated)
```

### 4.2 Dependency Inversion Checklist

| Use Case | Dependencies | Inverted? |
|----------|-------------|-----------|
| `RegisterUser` | `Arc<dyn UserRepository>` | ✓ Yes |
| `LoginUser` | `PostgresUserRepository` | No - concrete! |

### 4.3 Good vs Bad Dependency Injection

```rust
// GOOD: Depends on trait, injected via constructor
pub struct RegisterUser {
    user_repo: Arc<dyn UserRepository>,
// ... (30 lines truncated)
```

---

## Phase 5: Testability Analysis

### 5.1 Check for Test Mocks

Proper ports & adapters enables easy testing with mocks:

```bash
# Find mock implementations
grep -rn "Mock\|mock\|Fake\|fake\|InMemory\|Stub" src/ tests/ 2>/dev/null | grep -i "impl\|struct"

# Check if tests use mocks
grep -rn "Mock\|InMemory" tests/ 2>/dev/null
```

### 5.2 Testability Score

| Aspect | Score | Notes |
|--------|-------|-------|
| Mock implementations exist | /10 | |
| Use cases have unit tests | /10 | |
| Tests don't require DB | /10 | |
| Tests are fast | /10 | |

### 5.3 Test Example Verification

```bash
# Check if use case tests mock dependencies
grep -A30 "#\[tokio::test\]\|#\[test\]" src/application/ 2>/dev/null
```

---

## Phase 6: Driving Adapters Analysis

### 6.1 Find Driving Adapters (Inbound)

Driving adapters call INTO the application (HTTP handlers, CLI, etc.):

```bash
# HTTP handlers
find src -path "*adapters*" -name "*.rs" -exec grep -l "async fn.*handler\|async fn.*route\|Router" {} \;

# Check they call use cases
grep -rn "use_case\|UseCase\|Service" src/adapters/http/ 2>/dev/null
```

### 6.2 Driving Adapter Checklist

| Adapter | Calls Use Case? | Handles Conversion? |
|---------|-----------------|---------------------|
| HTTP handlers | | |
| CLI commands | | |
| gRPC handlers | | |

### 6.3 Good Driving Adapter Pattern

```rust
// GOOD: Handler calls use case, converts types
pub async fn register_handler(
    State(state): State<AppState>,
// ... (14 lines truncated)
```

---

## Phase 7: Generate Audit Report

### 7.1 Report Template

```markdown
# Ports & Adapters Audit Report

**Project:** [Name]
// ... (41 lines truncated)
```

---

## Quick Audit Commands

```bash
# Find all ports
echo "=== PORTS ===" && \
grep -rn "^pub trait.*:\s*Send\|#\[async_trait\]" src/application/ 2>/dev/null
// ... (9 lines truncated)
```

---

## Usage

```bash
/rust:audit-ports-adapters
/rust:audit-ports-adapters repositories
```
