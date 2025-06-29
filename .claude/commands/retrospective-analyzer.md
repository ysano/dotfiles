# レトロスペクティブアナライザー

GitHub Projects V2、Issues、PR、Actionsのメトリクスを分析し、改善のためのパターンを特定することで、データ駆動の洞察とアクションアイテムでスプリントレトロスペクティブを促進します。

## 実行手順

1. **レトロスペクティブの設定**
   - 分析するスプリント/マイルストーンの特定（デフォルト：最新）
   - GitHub Projects V2、APIアクセスの確認
   - レトロスペクティブ形式の設定
   - 分析時間範囲の設定

2. **スプリントデータ収集**

#### 定量的メトリクス
```
GitHub Projects V2/Issuesから：
- 計画対完了Issues数
- スプリントベロシティとキャパシティ
- Issueサイクルタイムとリードタイム
- バグラベルのIssues数
- 計画外Issueの割合

Git/GitHubから：
- コミット頻度と分布
- PRマージ時間統計
- コードレビューのターンアラウンド
- GitHub Actions成功率
- デプロイ頻度と成功率

GitHub Actions/Workflowsから：
- CI/CDパイプライン実行時間
- テストカバレッジトレンド
- ワークフロー失敗率
- デプロイメント頻度
```

#### 定性データソース
```
1. PRレビューコメントの感情
2. コミットメッセージパターン
3. GitHub Discussions/Issuesコメント
4. 前回のレトロスペクティブアクションアイテム
5. GitHub Issuesのバグレポートトレンド
6. GitHub Actionsログエラーパターン
7. Slack会話（利用可能な場合）
```

3. **Automated Analysis**

#### Sprint Performance Analysis
```markdown
# Sprint [Name] Retrospective Analysis

## Sprint Overview
- Duration: [Start] to [End]
- Team Size: [Number] members
- Sprint Goal: [Description]
- Goal Achievement: [Yes/Partial/No]

## Key Metrics Summary

### Delivery Metrics
| Metric | Target | Actual | Variance |
|--------|--------|--------|----------|
| Velocity | [X] pts | [Y] pts | [+/-Z]% |
| Completion Rate | 90% | [X]% | [+/-Y]% |
| Defect Rate | <5% | [X]% | [+/-Y]% |
| Unplanned Work | <20% | [X]% | [+/-Y]% |

### Process Metrics
| Metric | This Sprint | Previous | Trend |
|--------|-------------|----------|-------|
| Avg PR Review Time | [X] hrs | [Y] hrs | [↑/↓] |
| Avg Cycle Time | [X] days | [Y] days | [↑/↓] |
| CI/CD Success Rate | [X]% | [Y]% | [↑/↓] |
| Team Happiness | [X]/5 | [Y]/5 | [↑/↓] |
```

#### Pattern Recognition
```markdown
## Identified Patterns

### Positive Patterns 🟢
1. **Improved Code Review Speed**
   - Average review time decreased by 30%
   - Correlation with new review guidelines
   - Recommendation: Document and maintain process

2. **Consistent Daily Progress**
   - Even commit distribution throughout sprint
   - No last-minute rush
   - Indicates good sprint planning

### Concerning Patterns 🔴
1. **Monday Deploy Failures**
   - 60% of failed deployments on Mondays
   - Possible cause: Weekend changes not tested
   - Action: Implement Monday morning checks

2. **Increasing Scope Creep**
   - 35% unplanned work (up from 20%)
   - Source: Urgent customer requests
   - Action: Review sprint commitment process
```

4. **Interactive Retrospective Facilitation**

#### Pre-Retrospective Report
```markdown
# Pre-Retrospective Insights

## Data-Driven Discussion Topics

### 1. What Went Well 
Based on the data, these areas showed improvement:
- ✅ Code review efficiency (+30%)
- ✅ Test coverage increase (+5%)
- ✅ Zero critical bugs in production
- ✅ All team members contributed evenly

**Suggested Discussion Questions:**
- What specific changes led to faster reviews?
- How can we maintain zero critical bugs?
- What made work distribution successful?

### 2. What Didn't Go Well
Data indicates challenges in these areas:
- ❌ Sprint velocity miss (-15%)
- ❌ High unplanned work (35%)
- ❌ 3 rollbacks required
- ❌ Team overtime increased

**Suggested Discussion Questions:**
- What caused the velocity miss?
- How can we better handle unplanned work?
- What led to the rollbacks?

### 3. Action Items from Data
Recommended improvements based on patterns:
1. Implement feature flags for safer deployments
2. Create unplanned work budget in sprint planning
3. Add integration tests for [problem area]
4. Schedule mid-sprint check-ins
```

#### Live Retrospective Support
```
During the retrospective, I can help with:

1. **Fact Checking**: 
   "Actually, our velocity was 45 points, not 50"

2. **Pattern Context**:
   "This is the 3rd sprint with Monday deploy issues"

3. **Historical Comparison**:
   "Last time we had similar issues, we tried X"

4. **Action Item Tracking**:
   "From last retro, we completed 4/6 action items"
```

5. **Retrospective Output Formats**

#### Standard Retrospective Summary
```markdown
# Sprint [X] Retrospective Summary

## Participants
[List of attendees]

## What Went Well
- [Categorized list with vote counts]
- Supporting data: [Metrics]

## What Didn't Go Well  
- [Categorized list with vote counts]
- Root cause analysis: [Details]

## Action Items
| Action | Owner | Due Date | Success Criteria |
|--------|-------|----------|------------------|
| [Action 1] | [Name] | [Date] | [Measurable outcome] |
| [Action 2] | [Name] | [Date] | [Measurable outcome] |

## Experiments for Next Sprint
1. [Experiment description]
   - Hypothesis: [What we expect]
   - Measurement: [How we'll know]
   - Review date: [When to assess]

## Team Health Pulse
- Energy Level: [Rating]/5
- Clarity: [Rating]/5
- Confidence: [Rating]/5
- Key Quote: "[Notable team sentiment]"
```

#### Trend Analysis Report
```markdown
# Retrospective Trends Analysis

## Recurring Themes (Last 5 Sprints)

### Persistent Challenges
1. **Deployment Issues** (4/5 sprints)
   - Root cause still unresolved
   - Recommended escalation

2. **Estimation Accuracy** (5/5 sprints)
   - Consistent 20% overrun
   - Needs systematic approach

### Improving Areas
1. **Communication** (Improving for 3 sprints)
2. **Code Quality** (Steady improvement)

### Success Patterns
1. **Pair Programming** (Mentioned positively 5/5)
2. **Daily Standups** (Effective format found)
```

6. **Action Item Generation**

#### Smart Action Items
```
Based on retrospective discussion, here are SMART action items:

1. **Reduce Deploy Failures**
   - Specific: Implement smoke tests for Monday deploys
   - Measurable: <5% failure rate
   - Assignable: DevOps team
   - Relevant: Addresses 60% of failures
   - Time-bound: By next sprint

2. **Improve Estimation**
   - Specific: Use planning poker for all stories
   - Measurable: <20% variance from estimates
   - Assignable: Scrum Master facilitates
   - Relevant: Addresses velocity misses
   - Time-bound: Start next sprint planning
```

## Error Handling

### No GitHub Projects Data
```
"GitHub Projects V2アクセスが制限されています。GitデータとIssuesのみを使用します。

不足している洞察：
- Projects V2のカスタムフィールド分析
- プロジェクトレベルのメトリクス
- チームキャパシティデータ

以下のいずれかを希望しますか：
1. GitとIssuesデータのみで続行
2. スプリントメトリクスを手動入力
3. GitHub Projects V2権限を設定して再試行"
```

### Incomplete Sprint
```
"Sprint appears to be in progress. 

Current analysis based on:
- [X] days of [Y] total
- [Z]% work completed

Recommendation: Run full analysis after sprint ends
Proceed with partial analysis? [Y/N]"
```

## Advanced Features

### Sentiment Analysis
```python
# Analyze PR comments and commit messages
sentiment_indicators = {
    'positive': ['fixed', 'improved', 'resolved', 'great'],
    'negative': ['bug', 'issue', 'broken', 'failed', 'frustrated'],
    'neutral': ['updated', 'changed', 'modified']
}

# Generate sentiment report
"Team Sentiment Analysis:
- Positive indicators: 65%
- Negative indicators: 25%  
- Neutral: 10%

Trend: Improving from last sprint (was 55% positive)"
```

### Predictive Insights
```
"Based on current patterns:

⚠️ Risk Predictions:
- 70% chance of velocity miss if unplanned work continues
- Deploy failures likely to increase without intervention

💡 Opportunity Predictions:
- 15% velocity gain possible with proposed process changes
- Team happiness likely to improve with workload balancing"
```

### Experiment Tracking
```
"Previous Experiments Results:

1. 'No Meeting Fridays' (Sprint 12-14)
   - Result: 20% productivity increase
   - Recommendation: Make permanent

2. 'Pair Programming for Complex Tasks' (Sprint 15)
   - Result: 50% fewer defects
   - Recommendation: Continue with guidelines"
```

## Integration Options

1. **GitHub Issues**: アクションアイテムをIssuesとして作成
2. **GitHub Projects V2**: レトロスペクティブアイテムをプロジェクトに追加
3. **GitHub Discussions**: チームディスカッション投稿
4. **GitHub Actions**: レトロスペクティブレポート自動生成
5. **Slack**: チームチャンネルに概要投稿
6. **GitHub Pages**: フォーマットされたレトロスペクティブページをエクスポート
7. **Calendar**: アクションアイテムチェックインをスケジュール

## GitHub Actions統合

### 自動レトロスペクティブワークフロー
```yaml
# .github/workflows/retrospective.yml
name: Automated Retrospective Analysis
on:
  schedule:
    - cron: '0 16 * * 5'  # 毎週金曜日16時
  workflow_dispatch:
    inputs:
      sprint_number:
        description: 'Sprint/Milestone Number'
        required: false

jobs:
  retrospective-analysis:
    runs-on: ubuntu-latest
    steps:
      - name: Collect Sprint Data
        run: |
          # Get closed issues from last sprint
          gh issue list --state closed --milestone "${{ github.event.inputs.sprint_number || 'current' }}" --json number,title,closedAt,labels,assignees
          
          # Get merged PRs
          gh pr list --state merged --json number,title,mergedAt,additions,deletions,reviewDecision
          
          # Get workflow runs
          gh api /repos/${{ github.repository }}/actions/runs --jq '.workflow_runs[] | select(.created_at > "'$(date -d '2 weeks ago' --iso-8601)'")'

      - name: Generate Retrospective Report
        run: |
          # Analyze data and generate insights
          echo "## Sprint Retrospective Analysis" > retrospective.md
          echo "Date: $(date)" >> retrospective.md
          # Add analysis logic here

      - name: Create Issues for Action Items
        run: |
          # Create GitHub Issues for identified action items
          gh issue create --title "Retrospective Action: Improve CI/CD Pipeline" --body "Based on retrospective analysis..." --label "retrospective,improvement"

      - name: Update Project Board
        run: |
          # Add retrospective items to project
          gh project item-create --owner ${{ github.repository_owner }} --number 1 --title "Sprint Retrospective Completed"
```

### メトリクス収集スクリプト
```bash
#!/bin/bash
# collect-metrics.sh - GitHub レトロスペクティブメトリクス収集

REPO_OWNER="${1:-$(gh repo view --json owner --jq .owner.login)}"
REPO_NAME="${2:-$(gh repo view --json name --jq .name)}"
DAYS_BACK="${3:-14}"

echo "Collecting retrospective metrics for $REPO_OWNER/$REPO_NAME (last $DAYS_BACK days)"

# Issue metrics
echo "## Issue Metrics"
gh issue list --state closed --search "closed:>$(date -d "$DAYS_BACK days ago" --iso-8601)" --json number,title,closedAt,labels --jq '
  group_by(.labels[].name) | 
  map({label: .[0].labels[0].name, count: length}) |
  sort_by(.count) | reverse'

# PR metrics  
echo "## Pull Request Metrics"
gh pr list --state merged --search "merged:>$(date -d "$DAYS_BACK days ago" --iso-8601)" --json number,title,mergedAt,additions,deletions,comments --jq '
  {
    total_prs: length,
    total_additions: map(.additions) | add,
    total_deletions: map(.deletions) | add,
    avg_comments: (map(.comments | length) | add / length)
  }'

# Actions metrics
echo "## GitHub Actions Metrics"
gh api "/repos/$REPO_OWNER/$REPO_NAME/actions/runs?per_page=100" --jq '
  .workflow_runs[] | 
  select(.created_at > "'$(date -d "$DAYS_BACK days ago" --iso-8601)'") |
  {name: .name, status: .status, conclusion: .conclusion, run_started_at: .run_started_at}'
```

## Best Practices

1. **Data Before Discussion**: Review metrics first
2. **Focus on Patterns**: Look for recurring themes
3. **Action-Oriented**: Every insight needs action
4. **Time-boxed**: Keep retrospective focused
5. **Follow-up**: Track action item completion
6. **Celebrate Wins**: Acknowledge improvements
7. **Safe Space**: Encourage honest feedback
8. **Automate Collection**: Use GitHub Actions for consistent data gathering
9. **Version Control Insights**: Store retrospective outputs in repository for tracking