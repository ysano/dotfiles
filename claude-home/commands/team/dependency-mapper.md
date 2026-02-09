---
description: "Map and analyze project dependencies"
---

## Instructions

Analyze code and task dependencies to create visual dependency maps. Identify blockers, circular dependencies, and optimal execution order.

Analyze: `$ARGUMENTS`

### Steps

1. **Discover Dependencies**
   - Scan import/require statements across the codebase
   - Check package.json / Cargo.toml / go.mod for external dependencies
   - If GitHub Issues/Projects are available, query task relationships via `gh` CLI

2. **Detect Issues**
   - Circular dependencies between modules
   - Tightly coupled components
   - Blocked task chains (critical path)

3. **Generate Visualizations**

   **Mermaid dependency graph**:
   ```mermaid
   graph TD
     A[Module A] --> B[Module B]
     B --> C[Module C]
   ```

   **ASCII tree** for terminal output:
   ```
   ProjectRoot
   ├─ ModuleA [no blockers]
   ├─ ModuleB [blocked by: ModuleA]
   └─ ModuleC [blocked by: ModuleA, ModuleB]
   ```

4. **Recommend Execution Order**: Topological sort of tasks/modules, highlighting the critical path and parallelizable work
