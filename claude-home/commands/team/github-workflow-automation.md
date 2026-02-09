---
description: "GitHub Actions、Webhooks、Project automationを統合したネイティブ自動化システムで、シンプルなワークフローを実現します。"
---

## 実行手順

1. **自動化基盤の構築**
   - GitHub Actions workflowの設計: `$ARGUMENTS`
   - Webhook endpointsの設定
   - Project automation rulesの構成
   - Custom GitHub App準備（必要時）

2. **Issue生命周期の自動化**
   - Issue作成時の自動Project追加
   - ラベルベースの自動ルーティング
   - 担当者不在時のEscalation
   - Stale issue detection and cleanup

3. **プルリクエストワークフロー**
   - Draft PR作成時のIssue連携
   - Code review完了時のステータス更新
   - Merge時の関連Issueクローズ
   - Release noteの自動生成

4. **デプロイメント連携自動化**
   - Staging deploy時のProject更新
   - Production deploy時のMilestone完了
   - Rollback時の自動Issue作成
   - Hot fix deploymentの緊急ワークフロー

5. **通知とコミュニケーション**
   - Slack/Teams統合によるリアルタイム通知
   - Daily standup用情報の自動集約
   - Weekly progress reportの自動生成
   - Stakeholder向けサマリーの配信

6. **品質ゲート自動化**
   - Security scannerとIssue連携
   - Performance regression検出
   - Code coverage threshold enforcement
   - Dependency vulnerability tracking

7. **高度な分析ワークフロー**
   - Velocity calculation automation
   - Burndown chart自動更新
   - Team performance insights
   - Predictive analytics for planning

## シンプル化されたGitHubネイティブ機能

### 自動化ワークフロー
- **GitHub Actions** → Native CI/CD automation
- **Project Automation** → Built-in workflow management
- **Issue Templates** → Standardized process
- **PR Templates** → Consistent review workflow

### 統合機能
- **PR-Issue linking** → Native GitHub references  
- **Project Boards** → Direct project management
- **Milestones** → Built-in progress tracking

## GitHub Actions活用例

### Issue Automation Workflow
```yaml
name: Issue Automation
on:
  issues:
    types: [opened, labeled]
jobs:
  auto-assign:
    runs-on: ubuntu-latest
    steps:
      - name: Auto-assign to project
      - name: Set priority labels
      - name: Notify relevant team
```

### Project Update Workflow  
```yaml
name: Project Updates
on:
  pull_request:
    types: [closed]
jobs:
  update-project:
    if: github.event.pull_request.merged
    steps:
      - name: Move linked issues to Done
      - name: Update project timeline
      - name: Generate completion metrics
```

## 自動化のメリット

### 技術的優位性
- **Zero external dependencies**: GitHub ecosystem内完結
- **Native reliability**: Platform-level SLA保証
- **Real-time updates**: Webhook即座反映
- **Mobile compatibility**: 全自動化がモバイルで確認可能

### 運用効率
- **Setup complexity**: 70%削減
- **Maintenance overhead**: 最小限
- **Debugging**: 単一プラットフォーム内でトレース可能
- **Cost efficiency**: 追加ツールライセンス不要
