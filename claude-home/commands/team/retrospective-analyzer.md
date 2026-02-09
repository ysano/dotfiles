---
description: "Analyze team retrospectives for insights"
---

## Instructions

1. **Retrospective Setup**
   - Identify sprint to analyze (default: most recent)
   - Check Linear MCP connection for sprint data
   - Define retrospective format preference
   - Set analysis time range

2. **Sprint Data Collection**

#### Quantitative Metrics
```
From Linear/Project Management:
- Planned vs completed story points
- Sprint velocity and capacity
// ... (11 lines truncated)
```

#### Qualitative Data Sources
```
1. PR review comments sentiment
2. Commit message patterns
3. Slack conversations (if available)
4. Previous retrospective action items
5. Support ticket trends
```

3. **Automated Analysis**

#### Sprint Performance Analysis
```markdown
## Sprint Overview
- Duration: [Start] to [End]
- Team Size: [Number] members
// ... (21 lines truncated)
```

#### Pattern Recognition
```markdown
## Identified Patterns

### Positive Patterns 🟢
// ... (21 lines truncated)
```

4. **Interactive Retrospective Facilitation**

#### Pre-Retrospective Report
```markdown
# Pre-Retrospective Insights

## Data-Driven Discussion Topics
// ... (32 lines truncated)
```

#### Live Retrospective Support
```
During the retrospective, I can help with:

1. **Fact Checking**: 
// ... (11 lines truncated)
```

5. **Retrospective Output Formats**

#### Standard Retrospective Summary
```markdown
# Sprint [X] Retrospective Summary

## Participants
// ... (28 lines truncated)
```

#### Trend Analysis Report
```markdown
# Retrospective Trends Analysis

## Recurring Themes (Last 5 Sprints)
// ... (18 lines truncated)
```

6. **Action Item Generation**

#### Smart Action Items
```
Based on retrospective discussion, here are SMART action items:

1. **Reduce Deploy Failures**
// ... (13 lines truncated)
```

## Error Handling

### No Linear Data
```
"Linear MCP not connected. Using git data only.

Missing insights:
// ... (9 lines truncated)
```

### Incomplete Sprint
```
"Sprint appears to be in progress. 

Current analysis based on:
// ... (6 lines truncated)
```

## Advanced Features

### Sentiment Analysis
```python
# Analyze PR comments and commit messages
sentiment_indicators = {
    'positive': ['fixed', 'improved', 'resolved', 'great'],
// ... (12 lines truncated)
```

### Predictive Insights
```
"Based on current patterns:

⚠️ Risk Predictions:
// ... (7 lines truncated)
```

### Experiment Tracking
```
"Previous Experiments Results:

1. 'No Meeting Fridays' (Sprint 12-14)
// ... (7 lines truncated)
```

## Integration Options

1. **Linear**: Create action items as tasks
2. **Slack**: Post summary to team channel
3. **Confluence**: Export formatted retrospective page
4. **GitHub**: Create issues for technical debt items
5. **Calendar**: Schedule action item check-ins
