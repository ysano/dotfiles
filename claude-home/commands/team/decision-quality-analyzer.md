---
description: "Analyze decision quality with scenario testing, bias detection, and process optimization"
---

## Instructions

Systematically analyze and improve team decision quality. Ultrathink about cognitive biases and process gaps.

Decision to analyze: `$ARGUMENTS`

If no specific decision is provided, ask the user to describe the decision context.

### Analysis Framework

1. **Decision Context**: Identify type (strategic/operational/technical), stakeholders, success metrics, constraints
2. **Bias Detection**: Screen for confirmation bias, anchoring, groupthink, sunk cost fallacy, survivorship bias
3. **Scenario Testing**: Test the decision against best-case, worst-case, and most-likely scenarios
4. **Process Evaluation**: Assess information quality, stakeholder inclusion, time pressure, reversibility

### Output Format

```
## Decision Quality Analysis: [Decision]

### Context
- Type: [Strategic/Operational/Technical]
- Reversibility: [High/Medium/Low]
- Time pressure: [High/Medium/Low]

### Bias Risk Assessment
| Bias | Risk Level | Evidence | Mitigation |
|------|-----------|----------|------------|
| [Name] | [H/M/L] | [Indicator] | [Action] |

### Scenario Analysis
- Best case: [Outcome + probability estimate]
- Worst case: [Outcome + probability estimate]
- Most likely: [Outcome + probability estimate]

### Recommendations
1. [Specific actionable improvement]
2. [Process change for future decisions]
```
