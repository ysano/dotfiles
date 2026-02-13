---
name: simulation
description: >
  Business and technical scenario simulation domain knowledge.
  Use for modeling multiple futures, compressing timelines, testing decisions,
  creating digital twins, and optimizing strategic choices through systematic scenario analysis.
  Referenced by strategy-architect and decision-navigator Agents.
user-invocable: true
---

AI agents を reality simulator として活用し、実行から指数的なモデリング優位性へ移行する。

> **Agent 連携**: `strategy-architect` Agent はシナリオモデリングとタイムライン圧縮の行動原則、
> `decision-navigator` Agent は意思決定検証の行動原則を持つ。
> 具体的な手順は本 Skill の `references/` を参照して実行する。

## リファレンスガイド

| ドキュメント | 内容 | 参照タイミング |
|---|---|---|
| `references/examples.md` | 実践ガイドと業界事例 | シミュレーション初回実施・パターン学習時 |
| `references/constraint-modeler.md` | 制約モデリング手順 (10ステップ) | 意思決定環境の境界定義時 |
| `references/decision-tree-explorer.md` | 意思決定ツリー探索手順 (10ステップ) | 複雑な選択肢の期待値分析時 |
| `references/business-scenario-explorer.md` | ビジネスシナリオ探索手順 (10ステップ) | 複数タイムラインの戦略評価時 |
| `references/market-response-modeler.md` | 市場反応モデリング手順 (10ステップ) | 顧客・市場反応の予測時 |
| `references/simulation-calibrator.md` | シミュレーション較正手順 (9ステップ) | シミュレーション精度向上・検証時 |
| `references/digital-twin-creator.md` | デジタルツイン作成手順 (9ステップ) | システム・プロセスの仮想モデル構築時 |
| `references/future-scenario-generator.md` | 将来シナリオ生成手順 (9ステップ) | 長期的な可能性空間の探索時 |
| `references/timeline-compressor.md` | タイムライン圧縮手順 (9ステップ) | 急速な反復テスト・学習加速時 |

## シミュレーション種別クイックリファレンス

| 種別 | 用途 | 主要出力 | 関連リファレンス |
|------|------|---------|-----------------|
| 意思決定ツリー | 選択肢の最適化 | 期待値・推奨アクション | decision-tree-explorer |
| ビジネスシナリオ | 戦略評価 | シナリオマトリクス・確率分析 | business-scenario-explorer |
| 市場反応 | 顧客行動予測 | セグメント別反応率・収益影響 | market-response-modeler |
| デジタルツイン | システム最適化 | パフォーマンス指標・改善推奨 | digital-twin-creator |
| 制約モデリング | 境界定義 | 制約カタログ・最適化戦略 | constraint-modeler |
| タイムライン圧縮 | 学習加速 | 最適アプローチ・実装ロードマップ | timeline-compressor |
| 将来シナリオ | 長期計画 | シナリオポートフォリオ・戦略推奨 | future-scenario-generator |
| 較正 | 精度向上 | バイアス検出・改善ロードマップ | simulation-calibrator |

## 価値レバー

### 1. 代替タイムライン優位性
- コミットメント前に複数ビジネスシナリオを探索
- 製品ローンチ、マーケティングキャンペーン、戦略決定をテスト
- 異なる市場条件での "what-if" 分析を実行

### 2. 時間圧縮
- 競合が3回反復する間に300回反復
- 10年の市場サイクルを10時間のシミュレーションに圧縮
- 戦略の急速プロトタイピング (製品だけでなく)

### 3. 複利的インテリジェンス
- 各シミュレーションが意思決定のより良い事前分布を開発
- 価格設定の境界、隠れた市場セグメント、ブレークスルー機会を発見
- 線形実行アプローチでは見えないパターンを発見

## コマンドとの関係

以下の Commands は本 Skill への薄いエントリーポイント:

| Command | 対応リファレンス |
|---------|-----------------|
| `/simulation:constraint-modeler` | `references/constraint-modeler.md` |
| `/simulation:decision-tree-explorer` | `references/decision-tree-explorer.md` |
| `/simulation:business-scenario-explorer` | `references/business-scenario-explorer.md` |
| `/simulation:market-response-modeler` | `references/market-response-modeler.md` |
| `/simulation:simulation-calibrator` | `references/simulation-calibrator.md` |
| `/simulation:digital-twin-creator` | `references/digital-twin-creator.md` |
| `/simulation:future-scenario-generator` | `references/future-scenario-generator.md` |
| `/simulation:timeline-compressor` | `references/timeline-compressor.md` |

<constraints>
## 行動制約

- **事前検証必須**: 主要前提は必ず歴史的データまたは専門家知識で検証すること
- **信頼区間提示**: すべての予測に信頼度レベルと不確実性範囲を含めること
- **複数シナリオ原則**: 単一シナリオ分析を避け、最低3つの異なる未来を探索すること
- **較正サイクル確立**: シミュレーション精度を実世界結果と継続的に比較し改善すること
- **バイアス検出**: 楽観バイアス、確証バイアス、アンカリングバイアスを体系的にチェックすること
- **実装フォーカス**: シミュレーションは必ず実行可能な推奨と具体的な次ステップに接続すること
</constraints>
