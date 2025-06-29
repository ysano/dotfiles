# GitHub Issue自動化コマンド

GitHub Issueの作成、トリアージ、自動化を統合し、Linear変換機能を不要にするネイティブGitHubワークフローシステムです。

## 実行手順

1. **Issue自動化テンプレートの設定**
   - プロジェクト要件に基づくIssueテンプレート作成: `$ARGUMENTS`
   - カスタムラベル体系の構築
   - 自動アサインルールの設定
   - プライオリティとSeverityの標準化

2. **プルリクエスト連携自動化**
   - PR作成時の自動Issue作成
   - Conventional commitsによる自動分類
   - Code reviewと連携したIssue更新
   - Merge時の自動Issue完了処理

3. **CI/CD統合による自動Issue作成**
   - Build失敗時の自動Bug Issue作成
   - セキュリティスキャン結果のIssue化
   - Performance regression検出時のタスク作成
   - Dependency update失敗時の自動対応Issue

4. **トリアージ自動化システム**
   - 新規Issue受信時の自動分類
   - 類似Issue検出とリンク
   - SLA設定と期限管理
   - Escalation automaticルールの実装

5. **バルクオペレーション機能**
   - 複数Issue一括操作
   - ラベルとマイルストーンの一括更新
   - 重複Issue検出とマージ
   - Archive処理の自動化

6. **高度なワークフロー自動化**
   - GitHub Actions連携によるカスタムワークフロー
   - Webhook設定による外部ツール統合
   - Notification routingシステム
   - Custom GitHub App開発用基盤

7. **分析とレポーティング**
   - Issue解決パフォーマンス分析
   - Team responsiveness metrics
   - Backlog health monitoring
   - Customer satisfaction tracking

## 従来Linear機能の置き換え

### Issue変換（Linear不要）
- **issue-to-linear-task** → Issue Template automation
- **linear-task-to-issue** → Native Issue creation
- **bulk-import-issues** → GitHub Migration API

### 同期機能（不要）
- **bidirectional-sync** → Single source of truth
- **sync-conflict-resolver** → Conflict elimination
- **sync-status** → Native reliability

### ワークフロー統合
- **task-from-pr** → PR-Issue auto-linking
- **code-to-task** → Commit-Issue automation
- **cross-reference-manager** → GitHub References

## GitHub特有の優位性

- **Native Mobile Support**: iOS/Android app完全対応
- **Enterprise Security**: SSO, 2FA, Audit logs
- **Global Search**: Code + Issues統合検索
- **API Rate Limits**: 単一プラットフォームで効率的
- **Marketplace Integration**: 豊富な拡張機能エコシステム