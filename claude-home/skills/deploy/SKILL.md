---
name: deploy
description: >
  Deployment, release management, and CI/CD domain knowledge and procedures.
  Use when setting up CI/CD pipelines, containerizing applications, deploying to Kubernetes,
  automating releases, performing rollbacks, or managing hotfixes.
  Referenced by deploy-operator and release-operator Agents.
user-invocable: true
---

デプロイ、リリース管理、CI/CD パイプラインの構築・運用手順知識。

> **Agent 連携**: `deploy-operator` Agent は CI/CD アーキテクチャ設計の行動原則、
> `release-operator` Agent はリリース準備とロールバックの行動原則を持つ。
> 具体的な手順は本 Skill の `references/` を参照して実行する。

## リファレンスガイド

| ドキュメント | 内容 | 参照タイミング |
|---|---|---|
| `references/ci-setup.md` | 10 ステップ CI パイプライン構築 | CI/CD 初期設定・改善時 |
| `references/containerize-application.md` | 10 ステップアプリケーションコンテナ化 | Docker イメージ作成・最適化時 |
| `references/setup-kubernetes-deployment.md` | 10 ステップ Kubernetes デプロイ設定 | Kubernetes 環境構築時 |
| `references/setup-automated-releases.md` | 10 ステップ自動リリースワークフロー | リリース自動化導入時 |
| `references/rollback-deploy.md` | 12 ステップデプロイロールバック | 本番問題発生・緊急復旧時 |
| `references/add-changelog.md` | CHANGELOG 管理と自動生成 | リリースノート作成時 |
| `references/hotfix-deploy.md` | 14 ステップ緊急 Hotfix デプロイ | Critical/High 問題の緊急対応時 |
| `references/prepare-release.md` | 12 ステップリリース準備・検証 | 計画的リリース実施前 |

## デプロイ戦略クイックリファレンス

| 戦略 | 特徴 | ダウンタイム | ロールバック | 適用場面 |
|------|------|-------------|-------------|---------|
| Blue-Green | 2環境切り替え | なし | 即時 | 本番リリース |
| Canary | 段階的ロールアウト | なし | 段階的 | リスク高い変更 |
| Rolling | 順次更新 | なし | 時間要 | 通常デプロイ |
| Recreate | 停止→更新→起動 | あり | 手動 | 開発環境 |

## CI/CD ステージクイックリファレンス

| ステージ | 主要アクション | 失敗時の対応 |
|---------|--------------|-------------|
| Lint | ESLint, Prettier, 型チェック | コード修正必須 |
| Test | Unit, Integration, E2E | テスト修正またはコード修正 |
| Security | npm audit, Snyk, SAST | 脆弱性修正または例外承認 |
| Build | Production ビルド、最適化 | ビルド設定確認 |
| Deploy | 環境別デプロイ、ヘルスチェック | ロールバック |

## ロールバックタイプ

| タイプ | 対象 | 所要時間 | データ影響 |
|--------|------|---------|----------|
| Kubernetes | Pod/Deployment | 2-5分 | なし |
| コンテナイメージ | Docker tag | 3-8分 | なし |
| データベース | スキーマ/データ | 10-30分 | **あり** (要注意) |
| CDN キャッシュ | 静的アセット | 1-15分 | なし |

## コマンドとの関係

以下の Commands は本 Skill への薄いエントリーポイント:

| Command | 対応リファレンス |
|---------|-----------------|
| `/deploy:ci-setup` | `references/ci-setup.md` |
| `/deploy:containerize-application` | `references/containerize-application.md` |
| `/deploy:setup-kubernetes-deployment` | `references/setup-kubernetes-deployment.md` |
| `/deploy:setup-automated-releases` | `references/setup-automated-releases.md` |
| `/deploy:rollback-deploy` | `references/rollback-deploy.md` |
| `/deploy:add-changelog` | `references/add-changelog.md` |
| `/deploy:hotfix-deploy` | `references/hotfix-deploy.md` |
| `/deploy:prepare-release` | `references/prepare-release.md` |

<constraints>
## 行動制約

- **本番デプロイ前検証必須**: 本番デプロイ前にステージング環境で検証を完了すること
- **ロールバック計画必須**: デプロイ実行前に必ずロールバック手順を確認・準備すること
- **段階的デプロイ推奨**: リスクの高い変更は Canary または段階的ロールアウトを採用すること
- **監視アラート設定**: デプロイ後は最低 2時間の強化監視を実施し、異常を即時検出すること
</constraints>
