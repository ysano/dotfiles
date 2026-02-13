---
name: performance
description: >
  Application performance optimization domain knowledge and procedures.
  Use when auditing performance, optimizing builds/bundles/databases,
  implementing caching/CDN strategies, setting up monitoring, or simulating system behavior.
  Referenced by performance-reviewer and performance-architect Agents.
user-invocable: true
---

アプリケーションパフォーマンスの監査・最適化・監視・キャパシティプランニングの手順知識。

> **Agent 連携**: `performance-reviewer` Agent はボトルネック特定の行動原則、
> `performance-architect` Agent は戦略設計の行動原則を持つ。
> 具体的な手順は本 Skill の `references/` を参照して実行する。

## リファレンスガイド

| ドキュメント | 内容 | 参照タイミング |
|---|---|---|
| `references/performance-audit.md` | 11 ステップ包括的パフォーマンス監査 | ボトルネック特定・ベースライン計測時 |
| `references/monitoring.md` | APM/RUM/サーバー/DB 監視セットアップ | パフォーマンス監視導入時 |
| `references/caching.md` | 多層キャッシュ戦略設計・実装 | キャッシュ導入・最適化時 |
| `references/build-optimization.md` | ビルド速度・プロセス最適化 | ビルド時間短縮時 |
| `references/bundle-optimization.md` | バンドルサイズ削減・コード分割 | ロード時間改善時 |
| `references/database-optimization.md` | クエリ・インデックス・スキーマ最適化 | DB パフォーマンス改善時 |
| `references/cdn-optimization.md` | CDN 設定・アセット配信最適化 | コンテンツ配信最適化時 |
| `references/system-simulation.md` | 負荷シミュレーション・キャパシティプランニング | スケーラビリティ検証・容量計画時 |

## パフォーマンス指標クイックリファレンス

| カテゴリ | 指標 | 目標値 | 関連リファレンス |
|---------|------|--------|-----------------|
| Web Vitals | LCP (Largest Contentful Paint) | < 2.5s | audit, bundle, cdn |
| Web Vitals | INP (Interaction to Next Paint) | < 200ms | audit, bundle |
| Web Vitals | CLS (Cumulative Layout Shift) | < 0.1 | audit, bundle |
| API | レスポンスタイム (p95) | < 500ms | audit, database, caching |
| API | スループット | SLA 依存 | audit, caching, cdn |
| DB | クエリ実行時間 | < 100ms | database |
| Build | ビルド時間 (incremental) | < 10s | build |
| Bundle | 初期ロードサイズ | < 200KB (gzip) | bundle, cdn |

## コマンドとの関係

| Command | 対応リファレンス |
|---------|-----------------|
| `/performance:performance-audit` | `references/performance-audit.md` |
| `/performance:add-performance-monitoring` | `references/monitoring.md` |
| `/performance:implement-caching-strategy` | `references/caching.md` |
| `/performance:optimize-build` | `references/build-optimization.md` |
| `/performance:optimize-bundle-size` | `references/bundle-optimization.md` |
| `/performance:optimize-database-performance` | `references/database-optimization.md` |
| `/performance:setup-cdn-optimization` | `references/cdn-optimization.md` |
| `/performance:system-behavior-simulator` | `references/system-simulation.md` |

<constraints>
## 行動制約

- **計測必須**: 最適化の前後で必ずベースラインと改善値を計測すること
- **高インパクト優先**: 低労力・高インパクトの最適化を優先すること
- **推測で最適化しない**: プロファイリング結果に基づいて最適化すること
- **回帰防止**: パフォーマンス改善が機能の正確性を損なわないことを確認すること
</constraints>
