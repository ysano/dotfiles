# スプリント計画

現在のGitHub Projects V2、Issues、PRを分析し、優先度、依存関係、チームキャパシティをレビューして最適化されたスプリント計画を作成することでスプリント計画を支援します。

## 実行手順

1. **GitHub統合の確認**
まず、GitHub CLIとGitHub Projects V2アクセスを確認します：
- GitHub CLI認証を確認
- GitHub Projects V2 APIアクセスを検証
- リポジトリの権限レベルを確認
- GraphQL APIの利用可能性をチェック

2. **スプリントコンテキストの収集**
以下の情報を収集します：
- スプリント期間（例：2週間）
- スプリント開始日
- 関係するチームメンバー
- スプリントの目標/テーマ
- 前回スプリントのベロシティ（利用可能な場合）

3. **現在の状態分析**

#### GitHub Projects V2統合：
```
1. GitHub Projects V2からバックログアイテムを取得
2. 進行中のIssuesとそのステータスを取得
3. Issue優先度とProject依存関係を分析
4. アサイニーとキャパシティをチェック
5. ブロックされたIssuesと障害を確認
```

#### 追加のGitHubデータソース：
```
1. ラベルとマイルストーンでGitHub Issuesを分析
2. オープンなPull Requestsとそのステータスをレビュー
3. 最近のコミット活動を確認
4. GitHub Actionsワークフローステータスを確認
5. Project Boardsのカスタムフィールドデータを活用
```

4. **スプリント計画分析**

以下を含む包括的なスプリント計画を生成します：

```markdown
# Sprint Planning Report - [Sprint Name]

## Sprint Overview
- Duration: [Start Date] to [End Date]
- Team Members: [List]
- Sprint Goal: [Description]

## Capacity Analysis
- Total Available Hours: [Calculation]
- Previous Sprint Velocity: [Points/Hours]
- Recommended Capacity: [80-85% of total]

## Proposed Sprint Backlog

### High Priority Issues
1. [Issue #] - [Title]
   - Estimate: [Points/Hours]
   - Assignee: @[Username]
   - Dependencies: [Linked Issues]
   - Labels: [Priority/Category Labels]
   - Project Status: [Column/Status]
   - Acceptance Criteria: [Details]

### Medium Priority Issues
[Similar format]

### Nice-to-Have Issues
[Similar format]

## Risk Assessment
- Technical Risks: [List]
- Resource Risks: [List]
- Dependency Risks: [List]

## Recommendations
1. [Specific recommendations based on analysis]
2. [Resource allocation suggestions]
3. [Risk mitigation strategies]

## Metrics to Track
- Velocity (Issues completed per sprint)
- Burndown rate (GitHub Projects V2 insights)
- Blocked time (Issues with blocked labels)
- Cycle time (Issue creation to close)
- PR merge rate
- Code review turnaround time
- GitHub Actions success rate
```

5. **インタラクティブスプリント計画セッション**

以下の支援を提供します：
1. タスクの分解と見積もり
2. 依存関係のマッピング
3. リソースバランシング
4. リスク緩和計画
5. スプリントバックログの終了

## エラーハンドリング

### GitHub認証の問題
```
"GitHub CLIが認証されていないか、Projects V2へのアクセスが制限されています。スプリント計画の完全な機能を利用するには：

1. GitHub CLIを認証：
   gh auth login

2. 必要な権限を確認：
   - リポジトリへの読み取り/書き込み権限
   - Projects V2への読み取り権限
   - GraphQL APIアクセス

3. Organization/Enterprise設定を確認

利用可能なデータで続行します..."
```

### データの不足
```
"I need more information to create an effective sprint plan:
- [List specific missing data]
- [Provide templates for manual input]
```

## 使用例

### ユーザー入力：
"Help me plan our next 2-week sprint starting Monday"

### アシスタントの応答：
```
I'll help you plan your sprint. Let me first check your GitHub Projects integration...

[If connected]: Great! I can access your GitHub Projects. Let me analyze your backlog...

[Analysis results]

Based on my analysis, here's your proposed sprint plan:

**Sprint 24-01 (Jan 8-19)**

**Recommended Scope (32 story points):**
1. Issue #123: Implement user authentication (8 pts) - @john
2. Issue #124: Database migration scripts (5 pts) - @sarah
3. Issue #125: API rate limiting (5 pts) - @mike
...

**Key Risks:**
- Issue #123 blocks 3 other issues (dependency tracking)
- @sarah has 20% allocation to support tasks
- GitHub Actions quota limits may affect deployment

Would you like me to:
1. Adjust the scope based on different priorities?
2. Create a dependency visualization?
3. Generate sprint planning meeting agenda?
```

## ベストプラクティス

1. **常にキャパシティを確認する**: チームを過度にコミットさせない
2. **バッファ時間を含める**: 80-85%のキャパシティで計画する
3. **依存関係を考慮する**: タスクの関係をマッピングする
4. **ワークロードをバランスする**: タスクを均等に分配する
5. **明確な目標を定義する**: スプリントが集中した目標を持つことを確保する
6. **未知のことを計画する**: スパイク/調査時間を含める

## 統合ポイント

- GitHub Projects V2: タスク管理と追跡
- GitHub Issues: 詳細なタスク管理
- GitHub Actions: CI/CD パイプラインステータス
- GitHub Milestones: リリース管理
- GitHub Teams: チーム管理とレビュー
- Slack: チームコミュニケーション（利用可能な場合）
- Calendar: チームの利用可能性（アクセス可能な場合）

## GitHub Actions統合

### 自動化機能
```yaml
# .github/workflows/sprint-planning.yml
name: Sprint Planning Automation
on:
  schedule:
    - cron: '0 9 * * 1'  # 毎週月曜日9時
  workflow_dispatch:

jobs:
  sprint-planning:
    runs-on: ubuntu-latest
    steps:
      - name: Generate Sprint Report
        run: |
          gh project list --owner ${{ github.repository_owner }}
          gh issue list --milestone "Sprint $(date +%U)" --json number,title,assignees,labels
      
      - name: Update Project Status
        run: |
          # GitHub Projects V2 API calls
          gh api graphql -f query='
            query($owner: String!, $number: Int!) {
              repository(owner: $owner, name: $name) {
                projectV2(number: $number) {
                  items(first: 100) {
                    nodes {
                      id
                      content {
                        ... on Issue {
                          number
                          title
                          state
                        }
                      }
                    }
                  }
                }
              }
            }
          '
```

### Webhook統合
```json
{
  "name": "Sprint Planning Webhook",
  "active": true,
  "events": [
    "issues",
    "pull_request",
    "project_card",
    "milestone"
  ],
  "config": {
    "url": "https://your-webhook-endpoint.com/sprint-planning",
    "content_type": "json"
  }
}
```

## 出力形式

複数の出力オプションを提供します：
1. Markdownレポート（デフォルト）
2. スプレッドシートインポート用CSV
3. 自動化ツール用JSON
4. GitHub Projects V2インポート用YAML
5. GitHub Actions ワークフロー定義
6. GitHub Issue テンプレート生成