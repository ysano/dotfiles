---
description: "Map and analyze project dependencies"
---

## Purpose
This command analyzes code dependencies, git history, and Linear tasks to create visual dependency maps. It helps identify blockers, circular dependencies, and optimal task ordering for efficient project execution.

## Usage
```bash
claude "Show dependency map for task LIN-123"

# Analyze code dependencies in a module
// ... (8 lines truncated)
```

## Instructions

### 1. Analyze Code Dependencies
Use various techniques to identify dependencies:

```bash
# Find import statements (JavaScript/TypeScript)
rg "^import.*from ['\"](\.\.?/[^'\"]+)" --type ts --type js -o | sort | uniq

// ... (9 lines truncated)
```

### 2. Extract Task Dependencies from Linear
Query Linear for task relationships:

```javascript
// Get task with its dependencies
const task = await linear.getTask(taskId, {
  include: ['blockedBy', 'blocks', 'parent', 'children']
// ... (11 lines truncated)
```

### 3. Build Dependency Graph
Create a graph structure:

```javascript
class DependencyGraph {
  constructor() {
    this.nodes = new Map(); // taskId -> task details
// ... (83 lines truncated)
```

### 4. Generate Visual Representations

#### ASCII Tree View
```
LIN-123: Authentication System
├─ LIN-124: User Model [DONE]
├─ LIN-125: JWT Implementation [IN PROGRESS]
// ... (5 lines truncated)
```

#### Mermaid Diagram
```mermaid
graph TD
    LIN-123[Authentication System] --> LIN-124[User Model]
    LIN-123 --> LIN-125[JWT Implementation]
// ... (9 lines truncated)
```

#### Dependency Matrix
```
         | LIN-123 | LIN-124 | LIN-125 | LIN-126 | LIN-127 |
---------|---------|---------|---------|---------|---------|
LIN-123  |    -    |    →    |    →    |         |    →    |
// ... (7 lines truncated)
```

### 5. Analyze File Dependencies
Map code structure to tasks:

```javascript
// Analyze file imports
async function analyzeFileDependencies(filePath) {
  const content = await readFile(filePath);
// ... (23 lines truncated)
```

### 6. Generate Execution Order
Calculate optimal task sequence:

```javascript
function calculateExecutionOrder(graph) {
  const order = graph.topologicalSort();
  const taskDetails = [];
// ... (19 lines truncated)
```

### 7. Error Handling
```javascript
// Check for Linear access
if (!linear.available) {
  console.warn("Linear MCP not available, using code analysis only");
// ... (21 lines truncated)
```

## Example Output

```
Analyzing dependencies for Epic: Authentication System (LIN-123)

📊 Dependency Graph:
// ... (40 lines truncated)
```

## Advanced Features

### Impact Analysis
Show what tasks are affected by changes:
```bash
# What tasks are impacted if we change User.ts?
claude "Show impact analysis for changes to src/models/User.ts"
```

### Sprint Planning
Optimize task order for sprint capacity:
```bash
# Generate sprint plan considering dependencies
claude "Plan sprint with 20 points capacity considering dependencies"
```

### Risk Assessment
Identify high-risk dependency chains:
```bash
# Find longest dependency chains
claude "Show tasks with longest dependency chains in current sprint"
```
