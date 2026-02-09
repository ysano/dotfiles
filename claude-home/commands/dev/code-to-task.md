---
description: "Convert code analysis to Linear tasks"
---

## Purpose
This command scans your codebase for TODO/FIXME comments, technical debt markers, deprecated code, and other indicators that should be tracked as tasks. It automatically creates organized, prioritized Linear tasks to ensure important code improvements aren't forgotten.

## Usage
```bash
claude "Create tasks from all TODO comments in the codebase"

# Scan specific directory or module
claude "Find TODOs in src/api and create Linear tasks"

# Create tasks from specific patterns
claude "Create tasks for all deprecated functions"
   # ... (11 lines total, truncated)
```

## Instructions

### 1. Scan for Task Markers
Search for common patterns indicating needed work:

```bash
# Find TODO comments
rg "TODO|FIXME|HACK|XXX|OPTIMIZE|REFACTOR" --type-add 'code:*.{js,ts,py,java,go,rb,php}' -t code

# Find deprecated markers
rg "@deprecated|DEPRECATED|@obsolete" -t code

# Find temporary code
rg "TEMPORARY|TEMP|REMOVE BEFORE|DELETE ME" -t code -i
   # ... (17 lines total, truncated)
```

### 2. Parse Comment Context
Extract meaningful information from comments:

```javascript
class CommentParser {
  parseComment(file, lineNumber, comment) {
    const parsed = {
      type: 'todo',
      priority: 'medium',
      title: '',
      description: '',
      author: null,
   # ... (78 lines total, truncated)
```

### 3. Group and Deduplicate
Organize found issues intelligently:

```javascript
class TaskGrouper {
  groupTasks(parsedComments) {
    const groups = {
      byFile: new Map(),
      byType: new Map(),
      byAuthor: new Map(),
      byModule: new Map()
    };
   # ... (70 lines total, truncated)
```

### 4. Analyze Technical Debt
Identify code quality issues:

```javascript
class TechnicalDebtAnalyzer {
  async analyzeFile(filePath) {
    const issues = [];
    const content = await readFile(filePath);
    const lines = content.split('\n');
    
    // Check for long functions
    const functionMatches = content.matchAll(/function\s+(\w+)|(\w+)\s*=\s*\(.*?\)\s*=>/g);
   # ... (68 lines total, truncated)
```

### 5. Create Linear Tasks
Convert findings into actionable tasks:

```javascript
async function createLinearTasks(groupedTasks, options = {}) {
  const created = [];
  const skipped = [];
  
  // Check for existing tasks to avoid duplicates
  const existingTasks = await linear.searchTasks('TODO OR FIXME');
  const existingTitles = new Set(existingTasks.map(t => t.title));
  
   # ... (106 lines total, truncated)
```

### 6. Generate Summary Report
Create overview of findings:

```javascript
function generateReport(scanResults, createdTasks) {
  const report = {
    summary: {
      totalFound: scanResults.length,
      tasksCreated: createdTasks.created.length,
      tasksSkipped: createdTasks.skipped.length,
      byType: {},
      byPriority: {},
   # ... (39 lines total, truncated)
```

### 7. Error Handling
```javascript
// Handle access errors
try {
  await scanDirectory(path);
} catch (error) {
  if (error.code === 'EACCES') {
    console.warn(`Skipping ${path} - permission denied`);
  }
}
   # ... (40 lines total, truncated)
```

## Example Output

```
Scanning codebase for TODOs and technical debt...

📊 Scan Results:
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

Found 47 items across 23 files:
  • 24 TODOs
  • 8 FIXMEs 
   # ... (67 lines total, truncated)
```

## Advanced Features

### Custom Patterns
Define project-specific patterns:
```bash
# Add custom markers to scan
claude "Scan for REVIEW, QUESTION, and ASSUMPTION comments"
```

### Integration with CI/CD
```bash
# Fail build if critical TODOs found
claude "Check for SECURITY or FIXME comments and exit with error if found"
```

### Scheduled Scans
```bash
# Weekly technical debt report
claude "Generate weekly technical debt report and create tasks for new items"
```
