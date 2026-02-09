---
description: "Generate accurate project time estimates"
---

## Purpose
This command analyzes past commits, PR completion times, code complexity metrics, and team performance to provide accurate task estimates. It helps teams move beyond gut-feel estimates to data-backed predictions.

## Usage
```bash
claude "Estimate task: Implement OAuth2 login flow with Google"

# Analyze historical accuracy of estimates
// ... (8 lines truncated)
```

## Instructions

### 1. Gather Historical Data
Collect data from git history and Linear:

```bash
# Get commit history with timestamps and authors
git log --pretty=format:"%h|%an|%ad|%s" --date=iso --since="6 months ago" > commit_history.txt

// ... (9 lines truncated)
```

### 2. Calculate Code Complexity Metrics
Analyze code characteristics:

```javascript
function analyzeComplexity(filePath) {
  const metrics = {
    lines: 0,
// ... (24 lines truncated)
```

### 3. Build Estimation Models

#### Time-Based Estimation
```javascript
class HistoricalEstimator {
  constructor(gitData, linearData) {
    this.gitData = gitData;
// ... (71 lines truncated)
```

#### Pattern Recognition
```javascript
function extractFeatures(taskDescription) {
  const features = {
    keywords: [],
// ... (41 lines truncated)
```

### 4. Velocity Tracking
Track team and individual performance:

```javascript
class VelocityTracker {
  async analyzeVelocity(timeframe = '3 months') {
    // Get completed tasks with estimates and actual time
// ... (44 lines truncated)
```

### 5. Machine Learning Estimation
Use historical patterns for prediction:

```javascript
class MLEstimator {
  trainModel(historicalTasks) {
    // Feature extraction
// ... (44 lines truncated)
```

### 6. Estimation Report Format

```markdown
## Task Estimation Report

**Task:** Implement OAuth2 login flow with Google
// ... (41 lines truncated)
```

### 7. Error Handling
```javascript
// Handle missing historical data
if (historicalTasks.length < 10) {
  console.warn("Limited historical data. Estimates may be less accurate.");
// ... (16 lines truncated)
```

## Example Output

```
Analyzing task: "Refactor user authentication to use JWT tokens"

📊 Historical Analysis:
// ... (35 lines truncated)
```
