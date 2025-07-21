# GitHub Project統合管理コマンド

GitHub Projects V2を活用したスプリント管理、ワークロード分散、マイルストーン追跡を統合したプロジェクト管理システムです。

## 実行手順

1. **プロジェクト構造の分析**
   - 現在のGitHub Projectsの構成を確認
   - Sprint/Milestone構造を分析: `$ARGUMENTS`
   - チームメンバーのアサイン状況を評価
   - カスタムフィールドと自動化設定を検証

2. **Sprint管理の最適化**
   - GitHub Projects V2のIteration機能でスプリント設定
   - Milestone連携によるリリース計画作成
   - Sprint backlogの優先度マトリックス作成
   - Velocity tracking用のカスタムフィールド設定

3. **自動化ワークフローの設定**
   - Issue作成時の自動Project追加
   - Pull Request連携による進捗更新
   - ステータス変更トリガーの設定
   - Sprint完了時の自動アーカイブ

4. **チームワークロード分散**
   - GitHub GraphQL APIによるアサイン状況分析
   - Individual velocityの計算とバランシング
   - Capacity planningダッシュボードの作成
   - Over/Under allocation alertsの設定

5. **パフォーマンス監視**
   - Sprint burndown chartsの自動生成
   - Team velocity trendingの実装
   - Issue cycle timeの計測
   - Bottleneck detection systemの構築

6. **レポーティング機能**
   - Daily standup用サマリーの自動生成
   - Sprint retrospective用データ収集
   - Stakeholder向け進捗レポート作成
   - Custom KPIダッシュボードの構築

## GitHub Projects V2活用機能

- **Table View**: スプリントタスクの詳細管理
- **Board View**: Kanbanスタイルワークフロー
- **Timeline View**: マイルストーンとデッドライン管理  
- **Custom Fields**: チーム固有のメタデータ追加
- **Automation**: ワークフロー自動化ルール
- **Insights**: ネイティブ分析機能活用