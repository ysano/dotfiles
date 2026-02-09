---
description: "Explore architectural decisions through systematic scenario analysis with trade-off evaluation"
---

## Instructions

Systematically evaluate architectural options for: **$ARGUMENTS**

Think harder about this analysis.

### 1. Context Gathering

- **System Scope**: 対象システム/コンポーネントを特定
- **Constraints**: 技術的・ビジネス・リソース制約を確認
- **Success Criteria**: 何をもって成功とするかを定義

不明な点があればユーザーに質問してから進める。

### 2. Architecture Option Generation

最低3つの異なるアーキテクチャアプローチを特定し、各オプションについて:
- 構造特性（コンポーネント構成、通信パターン）
- 品質属性（パフォーマンス、スケーラビリティ、保守性）
- 実装コスト・複雑度

### 3. Scenario-Based Evaluation

各アーキテクチャを以下のシナリオで検証:
- **負荷シナリオ**: 通常/ピーク/障害時の挙動
- **進化シナリオ**: 要件変更・技術変化への適応性
- **コストシナリオ**: インフラ・運用・開発コスト推移

### 4. Trade-off Analysis

品質属性間のトレードオフをマトリックスで可視化:
- 各属性のビジネス優先度に基づく重み付け
- Pareto フロンティアの特定

### 5. Recommendation

ADR (Architecture Decision Record) 形式で出力:
- **Context**: 問題の背景
- **Decision**: 推奨アプローチと根拠
- **Consequences**: 受け入れるトレードオフ
- **Alternatives**: 却下した候補とその理由
