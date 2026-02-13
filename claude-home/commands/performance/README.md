# Performance Commands

パフォーマンス最適化・監視・分析のコマンド群。各コマンドは `performance` Skill の references/ に手順を委譲する薄いエントリーポイント。

## Skill との関係

知識本体は `skills/performance/` に集約。コマンドは明示的な起動インターフェースとして機能する。

## Available Commands

| Command | Description | Reference |
|---------|-------------|-----------|
| performance-audit | パフォーマンス包括監査 | `performance-audit.md` |
| add-performance-monitoring | 監視システム構築 | `monitoring.md` |
| implement-caching-strategy | キャッシュ戦略設計・実装 | `caching.md` |
| optimize-build | ビルド速度最適化 | `build-optimization.md` |
| optimize-bundle-size | バンドルサイズ削減 | `bundle-optimization.md` |
| optimize-database-performance | DB クエリ・インデックス最適化 | `database-optimization.md` |
| setup-cdn-optimization | CDN 設定・配信最適化 | `cdn-optimization.md` |
| system-behavior-simulator | 負荷シミュレーション・キャパシティプランニング | `system-simulation.md` |

## Related

- `performance` Skill: `skills/performance/SKILL.md`
- `performance-reviewer` Agent: コードレベルのパフォーマンスレビュー
- `performance-architect` Agent: システムレベルのパフォーマンス戦略
