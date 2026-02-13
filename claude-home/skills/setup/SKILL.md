---
name: setup
description: >
  Project setup, environment configuration, and development infrastructure knowledge.
  Use when setting up new projects, configuring development environments, implementing tooling,
  migrating technologies, designing APIs/databases, or establishing code quality standards.
  Referenced by project-architect, backend-architect, and dx-pro Agents.
user-invocable: true
---

プロジェクトセットアップ・開発環境構築・インフラ整備・技術移行の手順知識。

> **Agent 連携**: `project-architect` Agent はプロジェクト初期化の行動原則、
> `backend-architect` Agent は API・DB 設計の行動原則、
> `dx-pro` Agent は開発者体験最適化の行動原則を持つ。
> 具体的な手順は本 Skill の `references/` を参照して実行する。

## リファレンスガイド

| ドキュメント | 内容 | 参照タイミング |
|---|---|---|
| `references/development-environment.md` | 開発環境セットアップ手順 | プロジェクト初期化・新メンバーオンボード時 |
| `references/typescript-migration.md` | JavaScript → TypeScript 移行手順 | 型安全性導入・コード品質向上時 |
| `references/monitoring-observability.md` | 監視・オブザーバビリティ導入手順 | 本番監視システム構築時 |
| `references/monorepo.md` | モノレポ構成手順 | マルチパッケージ管理導入時 |
| `references/dependency-modernization.md` | 依存関係更新手順 | セキュリティ修正・依存関係更新時 |
| `references/code-formatting.md` | コードフォーマット設定手順 | コードスタイル統一時 |
| `references/linting.md` | リンティング設定手順 | コード品質標準化時 |
| `references/database-migrations.md` | DB マイグレーション管理手順 | スキーマ変更・データ移行時 |
| `references/database-schema-design.md` | DB スキーマ設計手順 | データモデル設計・最適化時 |
| `references/rest-api-design.md` | REST API 設計手順 | API エンドポイント設計時 |
| `references/graphql-api.md` | GraphQL API 実装手順 | GraphQL スキーマ・リゾルバ実装時 |
| `references/rate-limiting.md` | レート制限実装手順 | API 保護・公平利用制御時 |

## セットアップカテゴリ クイックリファレンス

| カテゴリ | ツール例 | 主な用途 | 関連リファレンス |
|---------|---------|---------|-----------------|
| 開発環境 | Node.js, Docker, Git | プロジェクト初期化、チームオンボード | development-environment |
| ランタイム | TypeScript, Babel | 言語機能強化、トランスパイル | typescript-migration |
| コード品質 | ESLint, Prettier, Husky | スタイル統一、自動チェック | linting, code-formatting |
| ビルド | Webpack, Vite, esbuild | バンドル、最適化 | development-environment |
| テスト | Jest, Vitest, Playwright | 品質保証 | `test` Skill |
| モノレポ | Nx, Turborepo, pnpm | マルチパッケージ管理 | monorepo |
| API | Express, Fastify, Apollo | バックエンド開発 | rest-api-design, graphql-api |
| データベース | PostgreSQL, Prisma, Knex | データ永続化、マイグレーション | database-schema-design, database-migrations |
| 監視 | Prometheus, Grafana, DataDog | 本番システム可視化 | monitoring-observability |
| 保護 | express-rate-limit, Redis | API 乱用防止 | rate-limiting |

## コマンドとの関係

以下の Commands は本 Skill への薄いエントリーポイント:

| Command | 対応リファレンス |
|---------|-----------------|
| `/setup:setup-development-environment` | `references/development-environment.md` |
| `/setup:migrate-to-typescript` | `references/typescript-migration.md` |
| `/setup:setup-monitoring-observability` | `references/monitoring-observability.md` |
| `/setup:setup-monorepo` | `references/monorepo.md` |
| `/setup:modernize-deps` | `references/dependency-modernization.md` |
| `/setup:setup-formatting` | `references/code-formatting.md` |
| `/setup:setup-linting` | `references/linting.md` |
| `/setup:create-database-migrations` | `references/database-migrations.md` |
| `/setup:design-database-schema` | `references/database-schema-design.md` |
| `/setup:design-rest-api` | `references/rest-api-design.md` |
| `/setup:implement-graphql-api` | `references/graphql-api.md` |
| `/setup:setup-rate-limiting` | `references/rate-limiting.md` |

<constraints>
## 行動制約

- **段階的導入**: 複雑なセットアップは段階的に導入し、各ステップで動作を検証すること
- **ドキュメント優先**: 設定変更時は必ずドキュメント（README / setup guide）も更新すること
- **チーム合意**: 開発ツールやスタイル設定の変更はチーム合意を得ること
- **検証スクリプト**: セットアップ完了後は必ず動作検証スクリプトを実行または作成すること
</constraints>
