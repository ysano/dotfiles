---
description: "Build complex features in parallel using multiple agents with dependency-aware batching"
---

## Instructions

Build features in parallel using agent orchestration for: **$ARGUMENTS**

`/dev:incremental-feature-build` の Phase 1 を実行した上で、以下の並列拡張を適用する。

### Phase 1: Dependency Analysis

- features.json に `dependencies` と `batch` フィールドを追加
- Topological sort でバッチを計算:
  - Batch 1: 依存なし（並列実行可能）
  - Batch 2: Batch 1 のみに依存
  - Batch N: Batch 1..N-1 に依存
- 循環依存を検出した場合は機能を分割して解消

### Phase 2: Agent Orchestration

バッチごとに並列 Task を起動:

```
Task tool:
  subagent_type: general-purpose
  prompt: |
    You are implementing feature FEAT-XXX for project: [name]

    Branch: feat/agent-N-FEAT-XXX (feature/[project]-main から分岐)

    Requirements:
    - [acceptance_criteria from features.json]

    完了時: ブランチ名、commit hash、実装メモを報告
```

- メインブランチ: `feature/[project]-main` を作成
- 各 agent は個別ブランチで作業

### Phase 3: Merge Coordination

バッチ内の全 agent 完了後:
1. Feature ID 順に sequential merge
2. 各 merge 後にテスト実行で検証
3. Conflict 発生時: より完全な実装を優先して解決

### Phase 4: Batch Progression

```
While batches remain:
  1. 現バッチの agents を並列起動
  2. 全 agent 完了を待機
  3. Sequential merge + 検証
  4. 次バッチへ進行
```

### Phase 5: Completion

全機能の `passes: true` を確認し、パフォーマンスレポート（並列化の効果）を生成。

See also: `/dev:incremental-feature-build` for building dependent features sequentially.
