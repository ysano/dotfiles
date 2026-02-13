---
name: docs
description: >
  Software documentation domain knowledge and procedures.
  Use when creating architecture docs, API documentation, onboarding guides,
  migration guides, or troubleshooting guides for various audiences.
  Referenced by docs-pro and api-docs-pro Agents.
user-invocable: true
---

ソフトウェアドキュメント作成・保守の手順知識。アーキテクチャ / API / オンボーディング / マイグレーション / トラブルシューティングガイド。

> **Agent 連携**: `docs-pro` Agent は包括的ドキュメント戦略の行動原則、
> `api-docs-pro` Agent は OpenAPI / SDK ドキュメント作成の行動原則を持つ。
> 具体的な手順は本 Skill の `references/` を参照して実行する。

## リファレンスガイド

| ドキュメント | 内容 | 参照タイミング |
|---|---|---|
| `references/architecture-documentation.md` | C4 Model / ADR / 図作成の 10 ステップ手順 | アーキテクチャ文書化・システム設計可視化時 |
| `references/api-documentation.md` | OpenAPI / Swagger / GraphQL 自動生成 10 ステップ | API ドキュメント作成・自動化時 |
| `references/onboarding-guide.md` | 開発者オンボーディング 10 ステップガイド | 新規メンバー受け入れ・環境セットアップ時 |
| `references/migration-guide.md` | バージョンアップグレード 13 ステップガイド | フレームワーク・ライブラリ移行時 |
| `references/troubleshooting-guide.md` | トラブルシューティング 14 ステップ手順 | 一般的問題の診断・解決手順作成時 |

## ドキュメント種別クイックリファレンス

| ドキュメント種別 | 対象読者 | 主な内容 | 関連リファレンス |
|-----------------|---------|---------|-----------------|
| Architecture Documentation | 開発者・アーキテクト | C4 図・ADR・システム構造 | architecture-documentation |
| API Documentation | 開発者（内部・外部） | エンドポイント・認証・例 | api-documentation |
| Onboarding Guide | 新規メンバー | 環境セットアップ・ワークフロー | onboarding-guide |
| Migration Guide | 開発者・運用チーム | 破壊的変更・移行手順 | migration-guide |
| Troubleshooting Guide | 開発者・サポート | 診断コマンド・解決策 | troubleshooting-guide |

## ドキュメント品質基準

| 基準 | 説明 | チェック項目 |
|------|------|------------|
| Clarity | 明確で簡潔な表現 | 専門用語は説明付き / ステップは具体的 |
| Completeness | 網羅的な情報 | 前提条件・手順・チェックリスト・例が揃う |
| Consistency | 一貫したスタイル | 用語・書式・構造が統一 |
| Accuracy | 正確な情報 | コード例は動作検証済み / バージョン明記 |
| Accessibility | 探しやすい構造 | 目次・見出し・クロスリファレンス完備 |

## コマンドとの関係

以下の Commands は本 Skill への薄いエントリーポイント:

| Command | 対応リファレンス |
|---------|-----------------|
| `/docs:create-architecture-documentation` | `references/architecture-documentation.md` |
| `/docs:generate-api-documentation` | `references/api-documentation.md` |
| `/docs:create-onboarding-guide` | `references/onboarding-guide.md` |
| `/docs:migration-guide` | `references/migration-guide.md` |
| `/docs:troubleshooting-guide` | `references/troubleshooting-guide.md` |

<constraints>
## 行動制約

- **読者優先**: 技術レベルと目的に応じてドキュメントの詳細度と表現を調整すること
- **コード例必須**: 抽象的な説明だけでなく、動作する具体的なコード例を含めること
- **クロスリファレンス**: 関連ドキュメントへのリンクを適切に配置すること
- **更新可能性**: ドキュメントのメンテナンス方法と自動化戦略を含めること
</constraints>
