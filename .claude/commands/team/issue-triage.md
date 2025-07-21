# Issue優先順位付けとトリアージ

自動カテゴリ化と優先度付けでGitHubイシューをインテリジェントにトリアージします

## システム

GitHubイシューを分析し、適切なカテゴリ化、優先度付け、チーム割り当てでインテリジェントにルーティングするイシュートリアージスペシャリストです。コンテンツ分析、パターン、ルールを使用してスマートなトリアージ決定を行います。

## 実行手順

GitHubイシューをトリアージする際に:

1. **イシュー分析**
   ```javascript
   async function analyzeIssue(issue) {
     const analysis = {
       // Content analysis
       sentiment: analyzeSentiment(issue.title, issue.body),
       urgency: detectUrgency(issue),
       category: categorizeIssue(issue),
       complexity: estimateComplexity(issue),
       
       // User analysis
       authorType: classifyAuthor(issue.author),
       authorHistory: await getAuthorHistory(issue.author),
       
       // Technical analysis
       stackTrace: extractStackTrace(issue.body),
       affectedComponents: detectComponents(issue),
       reproducibility: assessReproducibility(issue),
       
       // Business impact
       userImpact: estimateUserImpact(issue),
       businessPriority: calculateBusinessPriority(issue)
     };
     
     return analysis;
   }
   ```

2. **カテゴリ化ルール**
   ```javascript
   const categorizationRules = [
     {
       name: 'Security Issue',
       patterns: [/security/i, /vulnerability/i, /CVE-/],
       labels: ['security'],
       priority: 1, // Urgent
       team: 'security',
       notify: ['security-lead'],
       project: 'Security Issues',
       milestone: 'Security Review'
     },
     {
       name: 'Bug Report',
       patterns: [/bug/i, /error/i, /crash/i, /broken/i],
       hasStackTrace: true,
       labels: ['bug'],
       priority: (issue) => issue.sentiment < -0.5 ? 2 : 3,
       team: 'engineering',
       project: 'Engineering Backlog'
     },
     {
       name: 'Feature Request',
       patterns: [/feature/i, /enhancement/i, /add/i, /implement/i],
       labels: ['enhancement'],
       priority: 4,
       team: 'product',
       requiresDiscussion: true,
       project: 'Product Roadmap'
     },
     {
       name: 'Documentation',
       patterns: [/docs/i, /documentation/i, /readme/i],
       labels: ['documentation'],
       priority: 4,
       team: 'docs',
       project: 'Documentation'
     }
   ];
   ```

3. **優先度計算**
   ```javascript
   function calculatePriority(issue, analysis) {
     let score = 0;
     
     // Urgency indicators
     if (analysis.urgency === 'immediate') score += 40;
     if (containsKeywords(issue, ['urgent', 'asap', 'critical'])) score += 20;
     if (issue.title.includes('🔥') || issue.title.includes('!!!')) score += 15;
     
     // Impact assessment
     score += analysis.userImpact * 10;
     if (analysis.affectedComponents.includes('core')) score += 20;
     if (analysis.reproducibility === 'always') score += 10;
     
     // Author influence
     if (analysis.authorType === 'enterprise') score += 15;
     if (analysis.authorHistory.issuesOpened > 10) score += 5;
     
     // Time decay
     const ageInDays = (Date.now() - new Date(issue.createdAt)) / (1000 * 60 * 60 * 24);
     if (ageInDays > 30) score -= 10;
     
     // Map score to priority
     if (score >= 70) return 1; // Urgent
     if (score >= 50) return 2; // High
     if (score >= 30) return 3; // Medium
     return 4; // Low
   }
   ```

4. **チーム割り当て**
   ```javascript
   async function assignTeam(issue, analysis) {
     // Rule-based assignment
     for (const rule of categorizationRules) {
       if (matchesRule(issue, rule)) {
         return rule.team;
       }
     }
     
     // Component-based assignment
     const componentTeamMap = {
       'auth': 'identity-team',
       'api': 'platform-team',
       'ui': 'frontend-team',
       'database': 'data-team'
     };
     
     for (const component of analysis.affectedComponents) {
       if (componentTeamMap[component]) {
         return componentTeamMap[component];
       }
     }
     
     // ML-based assignment (if available)
     if (ML_ENABLED) {
       return await predictTeam(issue, analysis);
     }
     
     // Default assignment
     return 'triage-team';
   }
   ```

5. **重複検出**
   ```javascript
   async function findDuplicates(issue) {
     // Semantic similarity search
     const similar = await searchSimilarIssues(issue, {
       threshold: 0.85,
       limit: 5
     });
     
     // Title similarity
     const titleMatches = await searchByTitle(issue.title, {
       fuzzy: true,
       distance: 3
     });
     
     // Stack trace matching (for bugs)
     const stackTrace = extractStackTrace(issue.body);
     const stackMatches = stackTrace ? 
       await searchByStackTrace(stackTrace) : [];
     
     return {
       likely: similar.filter(s => s.score > 0.9),
       possible: [...similar, ...titleMatches, ...stackMatches]
         .filter(s => s.score > 0.7)
         .slice(0, 5)
     };
   }
   ```

6. **自動ラベル付け**
   ```javascript
   function generateLabels(issue, analysis) {
     const labels = new Set();
     
     // Category labels
     labels.add(analysis.category.toLowerCase());
     
     // Priority labels
     labels.add(`priority/${getPriorityName(analysis.priority)}`);
     
     // Technical labels
     if (analysis.stackTrace) labels.add('has-stack-trace');
     if (analysis.reproducibility === 'always') labels.add('reproducible');
     
     // Component labels
     analysis.affectedComponents.forEach(c => 
       labels.add(`component/${c}`)
     );
     
     // Status labels
     if (analysis.needsMoreInfo) labels.add('needs-info');
     if (analysis.duplicate) labels.add('duplicate');
     
     return Array.from(labels);
   }
   ```

7. **トリアージワークフロー**
   ```javascript
   async function triageIssue(issue) {
     const workflow = {
       analyzed: false,
       triaged: false,
       actions: []
     };
     
     try {
       // Step 1: Analyze
       const analysis = await analyzeIssue(issue);
       workflow.analyzed = true;
       
       // Step 2: Check duplicates
       const duplicates = await findDuplicates(issue);
       if (duplicates.likely.length > 0) {
         return handleDuplicate(issue, duplicates.likely[0]);
       }
       
       // Step 3: Determine routing
       const triage = {
         team: await assignTeam(issue, analysis),
         priority: calculatePriority(issue, analysis),
         labels: generateLabels(issue, analysis),
         assignee: await suggestAssignee(issue, analysis)
       };
       
       // Step 4: Update GitHub issue with triage results
       await updateGitHubIssue(issue, triage, analysis);
       workflow.triaged = true;
       
       // Step 6: Notify stakeholders
       await notifyStakeholders(issue, triage, analysis);
       
       return workflow;
     } catch (error) {
       workflow.error = error;
       return workflow;
     }
   }
   ```

8. **バッチトリアージ**
   ```javascript
   async function batchTriage(filters) {
     const issues = await fetchUntriaged(filters);
     const results = {
       total: issues.length,
       triaged: [],
       skipped: [],
       failed: []
     };
     
     console.log(`Found ${issues.length} issues to triage`);
     
     for (const issue of issues) {
       try {
         // Skip if already triaged
         if (hasTriageLabel(issue)) {
           results.skipped.push(issue);
           continue;
         }
         
         // Triage issue
         const result = await triageIssue(issue);
         if (result.triaged) {
           results.triaged.push({ issue, result });
         } else {
           results.failed.push({ issue, error: result.error });
         }
         
         // Progress update
         updateProgress(results);
         
       } catch (error) {
         results.failed.push({ issue, error });
       }
     }
     
     return results;
   }
   ```

9. **トリアージテンプレート**
   ```javascript
   const triageTemplates = {
     bug: {
       issueTemplate: `
   ## Bug Report
   
   **Reported by:** {author}
   **Severity:** {severity}
   **Reproducibility:** {reproducibility}
   
   ### Description
   {description}
   
   ### Stack Trace
   \`\`\`
   {stackTrace}
   \`\`\`
   
   ### Environment
   {environment}
   
   ### Steps to Reproduce
   {reproSteps}
       `,
       requiredInfo: ['description', 'environment', 'reproSteps']
     },
     
     feature: {
       issueTemplate: `
   ## Feature Request
   
   **Requested by:** {author}
   **Business Value:** {businessValue}
   
   ### Description
   {description}
   
   ### Use Cases
   {useCases}
   
   ### Acceptance Criteria
   {acceptanceCriteria}
       `,
       requiresApproval: true
     }
   };
   ```

10. **トリアージメトリクス**
    ```javascript
    function generateTriageMetrics(period = '7d') {
      return {
        volume: {
          total: countIssues(period),
          byCategory: groupByCategory(period),
          byPriority: groupByPriority(period),
          byTeam: groupByTeam(period)
        },
        
        performance: {
          avgTriageTime: calculateAvgTriageTime(period),
          autoTriageRate: calculateAutoTriageRate(period),
          accuracyRate: calculateAccuracy(period)
        },
        
        patterns: {
          commonIssues: findCommonPatterns(period),
          peakTimes: analyzePeakTimes(period),
          teamLoad: analyzeTeamLoad(period)
        }
      };
    }
    ```

## 例

### 手動トリアージ
```bash
# 単一イシューのトリアージ
claude issue-triage 123

# オプション付きトリアージ
claude issue-triage 123 --team="backend" --priority="high"

# インタラクティブトリアージ
claude issue-triage 123 --interactive
```

### 自動トリアージ
```bash
# 未トリアージのすべてのイシューをトリアージ
claude issue-triage --auto

# フィルター付きトリアージ
claude issue-triage --auto --label="needs-triage"

# スケジュールされたトリアージ
claude issue-triage --auto --schedule="*/15 * * * *"
```

### トリアージ設定
```bash
# トリアージルールの設定
claude issue-triage --setup-rules

# トリアージルールのテスト
claude issue-triage --test-rules --dry-run

# トリアージ設定のエクスポート
claude issue-triage --export-config > triage-config.json
```

## 出力形式

```
イシュートリアージレポート
===================
処理日時: 2025-01-16 11:00:00
モード: 自動

トリアージ概要:
───────────────────────────────────
総イシュー数      : 47
成功トリアージ : 44 (93.6%)
重複検出数  : 3
手動レビュー     : 3
失敗           : 0

カテゴリ別:
- バグレポート     : 28 (63.6%)
- 機能リクエスト: 12 (27.3%)
- ドキュメント   : 4 (9.1%)

優先度別:
- 緊急 (P1)     : 3  ████
- 高 (P2)       : 12 ████████████
- 中 (P3)     : 24 ████████████████████████
- 低 (P4)        : 5  █████

チーム割り当て:
- バックエンド         : 18
- フロントエンド        : 15
- セキュリティ        : 3
- ドキュメント   : 4
- トリアージチーム     : 4

注目すべきイシュー:
🔴 #456: 認証システムのセキュリティ脆弱性 → セキュリティチーム (P1)
🟠 #789: データベース接続プールエラー → バックエンドチーム (P2)
🟡 #234: ダークモードサポート追加 → フロントエンドチーム (P3)

実行されたアクション:
✓ 44のGitHub Issuesを更新
✓ 156のラベルを適用
✓ 12名のチームメンバーに割り当て
✓ 3つの重複をリンク
✓ 8通の通知を送信

トリアージメトリクス:
- 1イシューあたり平均時間: 2.3秒
- 自動トリアージ精度: 94.2%
- 手動介入: 6.8%
```

## GitHub Actions統合

### 自動トリアージワークフロー
```yaml
# .github/workflows/issue-triage.yml
name: Automated Issue Triage
on:
  issues:
    types: [opened, labeled]
  workflow_dispatch:

jobs:
  triage-issue:
    runs-on: ubuntu-latest
    steps:
      - name: Analyze Issue Content
        id: analyze
        run: |
          ISSUE_BODY="${{ github.event.issue.body }}"
          ISSUE_TITLE="${{ github.event.issue.title }}"
          
          # Determine issue type
          if echo "$ISSUE_TITLE $ISSUE_BODY" | grep -qi "bug\|error\|crash\|broken"; then
            echo "issue_type=bug" >> $GITHUB_OUTPUT
            echo "priority=2" >> $GITHUB_OUTPUT
            echo "labels=bug,needs-investigation" >> $GITHUB_OUTPUT
            echo "project=Bug Triage" >> $GITHUB_OUTPUT
          elif echo "$ISSUE_TITLE $ISSUE_BODY" | grep -qi "feature\|enhancement\|add"; then
            echo "issue_type=feature" >> $GITHUB_OUTPUT
            echo "priority=4" >> $GITHUB_OUTPUT
            echo "labels=enhancement,needs-discussion" >> $GITHUB_OUTPUT
            echo "project=Feature Requests" >> $GITHUB_OUTPUT
          elif echo "$ISSUE_TITLE $ISSUE_BODY" | grep -qi "security\|vulnerability\|CVE"; then
            echo "issue_type=security" >> $GITHUB_OUTPUT
            echo "priority=1" >> $GITHUB_OUTPUT
            echo "labels=security,urgent" >> $GITHUB_OUTPUT
            echo "project=Security Issues" >> $GITHUB_OUTPUT
          else
            echo "issue_type=general" >> $GITHUB_OUTPUT
            echo "priority=3" >> $GITHUB_OUTPUT
            echo "labels=needs-triage" >> $GITHUB_OUTPUT
            echo "project=General Backlog" >> $GITHUB_OUTPUT
          fi

      - name: Apply Labels and Priority
        run: |
          # Add labels based on analysis
          IFS=',' read -ra LABELS <<< "${{ steps.analyze.outputs.labels }}"
          for label in "${LABELS[@]}"; do
            gh issue edit ${{ github.event.issue.number }} --add-label "$label"
          done
          
          # Add priority label
          gh issue edit ${{ github.event.issue.number }} --add-label "priority/P${{ steps.analyze.outputs.priority }}"

      - name: Assign Team Based on Content
        run: |
          ISSUE_BODY="${{ github.event.issue.body }}"
          ISSUE_TITLE="${{ github.event.issue.title }}"
          
          # Assign based on components mentioned
          if echo "$ISSUE_TITLE $ISSUE_BODY" | grep -qi "auth\|login\|jwt\|oauth"; then
            gh issue edit ${{ github.event.issue.number }} --add-assignee "@org/identity-team"
          elif echo "$ISSUE_TITLE $ISSUE_BODY" | grep -qi "api\|endpoint\|rest\|graphql"; then
            gh issue edit ${{ github.event.issue.number }} --add-assignee "@org/platform-team"
          elif echo "$ISSUE_TITLE $ISSUE_BODY" | grep -qi "ui\|frontend\|component\|css"; then
            gh issue edit ${{ github.event.issue.number }} --add-assignee "@org/frontend-team"
          elif echo "$ISSUE_TITLE $ISSUE_BODY" | grep -qi "database\|sql\|migration"; then
            gh issue edit ${{ github.event.issue.number }} --add-assignee "@org/data-team"
          fi

      - name: Add to Project Board
        run: |
          # Add issue to appropriate project
          gh project item-create --owner ${{ github.repository_owner }} --number 1 --url "${{ github.event.issue.html_url }}"

      - name: Check for Duplicates
        run: |
          # Search for similar issues
          SIMILAR_ISSUES=$(gh issue list --search "is:issue \"${{ github.event.issue.title }}\" -number:-${{ github.event.issue.number }}" --json number,title --limit 5)
          
          if [ "$(echo "$SIMILAR_ISSUES" | jq 'length')" -gt 0 ]; then
            echo "Found potential duplicates:"
            echo "$SIMILAR_ISSUES" | jq -r '.[] | "#\(.number): \(.title)"'
            
            # Comment on issue about potential duplicates
            gh issue comment ${{ github.event.issue.number }} --body "
            🤖 **Automated Duplicate Check**
            
            I found some potentially similar issues:
            $(echo "$SIMILAR_ISSUES" | jq -r '.[] | "- #\(.number): \(.title)"')
            
            Please check if this is a duplicate before proceeding.
            "
            
            gh issue edit ${{ github.event.issue.number }} --add-label "potential-duplicate"
          fi

      - name: Security Alert
        if: steps.analyze.outputs.issue_type == 'security'
        run: |
          # Create urgent notification for security issues
          gh issue comment ${{ github.event.issue.number }} --body "
          🚨 **Security Issue Detected**
          
          This issue has been flagged as security-related and requires immediate attention.
          - Priority: P1 (Urgent)
          - Team: @org/security-team
          - Next steps: Security team will review within 2 hours
          "
          
          # Notify security team (if Slack webhook is configured)
          if [ -n "${{ secrets.SLACK_WEBHOOK_URL }}" ]; then
            curl -X POST -H 'Content-type: application/json' \
              --data '{"text":"🚨 Security issue reported: ${{ github.event.issue.html_url }}"}' \
              ${{ secrets.SLACK_WEBHOOK_URL }}
          fi
```

### 自動ラベル管理
```yaml
# .github/workflows/label-management.yml
name: Label Management
on:
  workflow_dispatch:
  schedule:
    - cron: '0 2 * * 1'  # Weekly on Monday 2 AM

jobs:
  ensure-labels:
    runs-on: ubuntu-latest
    steps:
      - name: Create Standard Triage Labels
        run: |
          # Priority labels
          gh label create "priority/P1" --color "d73a4a" --description "Urgent - requires immediate attention" || true
          gh label create "priority/P2" --color "e99695" --description "High priority" || true
          gh label create "priority/P3" --color "fbca04" --description "Medium priority" || true
          gh label create "priority/P4" --color "0e8a16" --description "Low priority" || true
          
          # Type labels
          gh label create "type/bug" --color "d73a4a" --description "Something isn't working" || true
          gh label create "type/feature" --color "a2eeef" --description "New feature or request" || true
          gh label create "type/security" --color "b60205" --description "Security related issue" || true
          gh label create "type/docs" --color "0075ca" --description "Documentation" || true
          
          # Status labels
          gh label create "status/needs-triage" --color "ffffff" --description "Requires initial triage" || true
          gh label create "status/triaged" --color "c2e0c6" --description "Has been triaged" || true
          gh label create "status/blocked" --color "e4e669" --description "Blocked by external dependency" || true
          gh label create "status/duplicate" --color "cfd3d7" --description "Duplicate issue" || true
          
          # Component labels
          gh label create "component/auth" --color "f9d0c4" --description "Authentication/Authorization" || true
          gh label create "component/api" --color "c5def5" --description "API related" || true
          gh label create "component/ui" --color "fef2c0" --description "User Interface" || true
          gh label create "component/database" --color "d4c5f9" --description "Database related" || true

      - name: Clean Up Old Labels
        run: |
          # Archive or remove deprecated labels
          gh label edit "bug" --new-name "legacy-bug" --description "Deprecated: use type/bug" || true
          gh label edit "enhancement" --new-name "legacy-enhancement" --description "Deprecated: use type/feature" || true
```

## ベストプラクティス

1. **ルールの精緻**
   - トリアージ精度を定期的にレビュー
   - フィードバックに基づいてパターンを更新
   - GitHub Actions ワークフローでルールをテスト

2. **品質管理**
   - トリアージされたIssuesをサンプルレビュー
   - 偽陽性/偽陰性を追跡
   - GitHub Insights でフィードバックループを実装

3. **ステークホルダーコミュニケーション**
   - 新しい割り当てをチームに通知
   - GitHub Discussions でトリアージ概要を提供
   - 重要な問題をGitHub Actions経由でエスカレーション

4. **継続的改善**
   - GitHub API データでトリアージパターンを分析
   - ラベルとプロジェクト割り当てルールを最適化
   - GitHub Actions とWebhookで自動化を強化

5. **GitHub固有の最適化**
   - Projects V2 のカスタムフィールドを活用
   - GitHub Teams を使用したチーム管理
   - GitHub Apps を使用した高度な自動化
   - Webhooks によるリアルタイムトリアージ