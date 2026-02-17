---
name: model-selection
description: |
  Claude Code サブエージェントのモデル選択戦略とコスト最適化の知識ベース。
  Use when designing agents, reviewing model assignments, estimating costs,
  or evaluating task complexity for model routing decisions.
  Referenced by prompt-engineering Skill (agent.md).
user-invocable: true
disable-model-invocation: false
---

Claude Code サブエージェント（Task ツール）のモデル選択に関する知識ベース。
エージェント設計、コストレビュー、タスク複雑性評価に活用する。

## ハイブリッド・ルーティング戦略

タスクの性質に応じて `model:` フィールドを戦略的に固定（Pinning）する。
`inherit`（デフォルト）は万能ではなく、コスト不可視化のリスクがある。

| カテゴリ | 固定モデル | 該当タスク | 理由 |
|----------|-----------|-----------|------|
| **Utility** | `haiku` | 検索、リント、フォーマット変換、CI/CD | 親が Opus でも常に最安・最速で実行 |
| **Critical** | `opus` / `sonnet` | セキュリティ監査、アーキテクチャレビュー | コスト削減よりも品質を強制的に優先 |
| **Contextual** | `inherit` | 計画、相談、対話型レビュー | 親セッションの推論レベルに合わせる |

> 詳細: `references/hybrid-routing-strategy.md`

## タスク複雑性 × モデル選択マトリクス（凝縮版）

| タスクカテゴリ | NLCC | 推奨モデル | 根拠 |
|--------------|------|-----------|------|
| コードベース探索 | 低 | Haiku | 幻覚リスク低、速度優先 |
| 単体テスト生成 | 中 | Sonnet | ロジック理解が必要だが局所的 |
| バグ修正 | 高 | Sonnet (Thinking) | 実行パス追跡、思考モードで試行削減 |
| アーキテクチャ設計 | 超高 | Opus | 失敗時のシステム全体手戻りを回避 |
| セキュリティレビュー | 高 | Opus / Sonnet | False Negative が許容不可 |
| CI/CD 自動化 | 低 | Haiku | 定型的、自律判断不要 |

> 完全版マトリクス・各モデル詳細: `references/model-characteristics.md`

## コスト判断の原則

**「安物買いの銭失い」を避ける**: 単価だけでなく、手戻り確率を含む期待コスト（TCO）で比較する。

- 成功率 P_s = 50% → コスト 2倍、P_s = 20% → コスト 5倍
- Thinking Mode は P_s を向上させる「デフレ的投資」
- 人間介入コスト（時給）を加算すると、低性能モデルのリスクは指数的に増大

> 数式・損益分岐点: `references/backtracking-cost-model.md`

## タスク複雑性の測定: NLCC

自然言語サイクロマティック複雑度（NLCC）の3要素でタスクを評価:

1. **曖昧性**: プロンプトの解釈分岐数
2. **依存性の幅**: 参照ファイル・ドキュメント数
3. **推論の深度**: 最終アクションまでの論理ステップ数

Traffic Light System: **Green**（即時実行）→ **Yellow**（Thinking/Opus）→ **Red**（人間に差し戻し）

> 詳細・Scope Check パターン: `references/nlcc-complexity.md`

## リファレンスガイド

| ファイル | 内容 |
|---------|------|
| `references/hybrid-routing-strategy.md` | 3分類定義、inherit の功罪、Pinning ルール、判定フロー |
| `references/model-characteristics.md` | Haiku/Sonnet/Opus 特性比較、完全版マトリクス、AI-DLC マッピング |
| `references/backtracking-cost-model.md` | 手戻りコスト数式、不信頼税、損益分岐点、Thinking Mode 経済効果 |
| `references/nlcc-complexity.md` | NLCC 3要素、Traffic Light System、Scope Check パターン |

## 使い方

- **エージェント設計時**: ハイブリッド・ルーティング表 + 判定フローで `model:` を決定
- **コストレビュー時**: 手戻りコストモデルで TCO を試算し、モデル変更の損益分岐点を確認
- **タスク着手前**: NLCC の Traffic Light System でタスクを分類し、適切なモデルを選択
- **AI-DLC フェーズ設計時**: 3フェーズ × モデルのマッピングで役割分担を定義

## クロスリファレンス

- **prompt-engineering** Skill (`references/agent.md`): エージェント定義の `model:` フィールド設計
- **ai-dlc-ceremonies** Skill: AI-DLC 3フェーズ × モデル配置の運用面
- **ai-dlc-estimate** Skill: 見積もり時のコスト試算への応用
