---
description: "Balance team workload distribution"
---

## Purpose
This command analyzes team members' current workloads, skills, past performance, and availability to suggest optimal task assignments. It helps prevent burnout, ensures balanced distribution, and matches tasks to team members' strengths.

## Usage
```bash
claude "Show workload balance for the engineering team"

# Suggest optimal assignment for new tasks
claude "Who should work on the new payment integration task?"

# Rebalance current sprint
claude "Rebalance tasks in the current sprint for optimal distribution"
   # ... (11 lines total, truncated)
```

## Instructions

### 1. Gather Team Data
Collect information about team members:

```javascript
class TeamAnalyzer {
  async gatherTeamData() {
    const team = {};
    
    // Get team members from Linear
    const teamMembers = await linear.getTeamMembers();
    
    for (const member of teamMembers) {
   # ... (89 lines total, truncated)
```

### 2. Calculate Workload Metrics
Analyze current workload distribution:

```javascript
class WorkloadCalculator {
  calculateWorkload(teamMember) {
    const metrics = {
      currentPoints: 0,
      currentTasks: teamMember.currentTasks.length,
      inProgressPoints: 0,
      todoPoints: 0,
      blockedTasks: 0,
   # ... (78 lines total, truncated)
```

### 3. Skill Matching Algorithm
Match tasks to team members based on skills:

```javascript
class SkillMatcher {
  calculateSkillMatch(task, teamMember) {
    const taskRequirements = this.extractTaskRequirements(task);
    const memberSkills = this.consolidateSkills(teamMember);
    
    let matchScore = 0;
    let maxScore = 0;
    
   # ... (92 lines total, truncated)
```

### 4. Load Balancing Algorithm
Distribute tasks optimally:

```javascript
class LoadBalancer {
  balanceTasks(tasks, team, constraints = {}) {
    const assignments = new Map(); // task -> assignee
    const workloads = new Map(); // assignee -> current load
    
    // Initialize workloads
    for (const [memberId, member] of Object.entries(team)) {
      workloads.set(memberId, this.calculateWorkload(member));
   # ... (102 lines total, truncated)
```

### 5. Visualization Functions
Create visual representations of workload:

```javascript
function visualizeWorkload(team, assignments) {
  const output = [];
  
  // Team workload bar chart
  output.push('## Team Workload Distribution\n');
  
  const maxPoints = Math.max(...Object.values(team).map(m => m.currentPoints));
  
   # ... (82 lines total, truncated)
```

### 6. Optimization Suggestions
Generate actionable recommendations:

```javascript
class WorkloadOptimizer {
  generateSuggestions(team, currentAssignments, constraints) {
    const suggestions = [];
    const metrics = this.analyzeCurrentState(team);
    
    // Check for overloaded members
    for (const [id, member] of Object.entries(team)) {
      if (member.workloadScore > 90) {
   # ... (74 lines total, truncated)
```

### 7. Error Handling
```javascript
// Handle missing Linear access
if (!linear.available) {
  console.error("Linear MCP tool not available");
  // Fall back to manual input or cached data
}

// Handle team member availability
const availability = {
   # ... (26 lines total, truncated)
```

## Example Output

```
Analyzing team workload and generating recommendations...

👥 Team Overview
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

Current Sprint: Sprint 23 (5 days remaining)
Team Size: 5 engineers
Total Capacity: 65 points
   # ... (88 lines total, truncated)
```

## Advanced Features

### Capacity Planning
```bash
# Plan next sprint with holidays and time off
claude "Plan sprint 24 capacity - Alice off Monday, Bob at conference Wed-Thu"
```

### Skill Development
```bash
# Identify learning opportunities
claude "Suggest tasks for Carol to learn React based on current workload"
```

### Team Performance
```bash
# Analyze team velocity trends
claude "Show team velocity trends and predict sprint 24 capacity"
```
