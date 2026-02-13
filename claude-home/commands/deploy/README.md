# Deploy Commands

デプロイ、リリース管理、CI/CD 設定のためのコマンド群。

> **Knowledge Base**: 各コマンドの詳細な手順は `deploy` Skill (`skills/deploy/`) に集約されている。
> コマンドは Skill への薄いエントリーポイントとして機能する。

## Available Commands

- **ci-setup.md** - CI パイプラインを構築
- **containerize-application.md** - アプリケーションのコンテナ化
- **setup-kubernetes-deployment.md** - Kubernetes デプロイ設定
- **setup-automated-releases.md** - 自動リリースワークフロー構築
- **rollback-deploy.md** - デプロイのロールバック
- **add-changelog.md** - CHANGELOG 生成・管理
- **hotfix-deploy.md** - 緊急 Hotfix デプロイ
- **prepare-release.md** - リリース準備・検証

## Related

- **Skill**: `deploy` - デプロイ戦略クイックリファレンス、CI/CD ステージ、ロールバックタイプ
- **Agent**: `deploy-operator` - CI/CD アーキテクチャ設計の専門エージェント
- **Agent**: `release-operator` - リリース準備とロールバックの専門エージェント
