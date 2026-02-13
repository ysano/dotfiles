---
description: "Build complex features incrementally with structured tracking to prevent premature completion"
---

## Instructions

Build a feature incrementally for: **$ARGUMENTS**

### Phase 1: Feature Expansion

ユーザーのリクエストを包括的な機能リストに展開:

1. `.feature-tracking/features.json` を作成
2. 各機能は `{id, category, title, acceptance_criteria, dependencies, passes: false}` の構造
3. カテゴリ: functional, ui, integration, performance, security, accessibility
4. 規模の目安: Small 10-30, Medium 30-60, Large 60-100 features

### Phase 2: Progress Initialization

- `.feature-tracking/PROGRESS.md` を作成（進捗追跡用）
- 初期状態を git commit

### Phase 3: Implementation Loop

**Critical Rules**:
- **1機能ずつ** 順番に実装
- features.json の機能を **削除・編集しない**（`passes: false→true` のみ許可）
- 各機能完了後に **git commit**

各機能のワークフロー:
1. `passes: false` の最初の機能を選択（依存関係を確認）
2. 実装 → テスト → 既存機能が壊れていないことを確認
3. features.json の `passes` を `true` に更新、`implementedAt` を記録
4. commit: `feat(FEAT-XXX): [description]`

### Phase 4: Recovery

- 実装失敗時: `git restore` で戻し、PROGRESS.md にブロッカーを記録、次の機能へ
- コード破損時: `git stash` → baseline確認 → `git stash pop` で修正

### Phase 5: Completion

```bash
# 全機能完了の確認（0を返すこと）
cat .feature-tracking/features.json | jq '[.features[] | select(.passes == false)] | length'
```

最終サマリーを PROGRESS.md に記録し、完了 commit を作成。

See also: `/dev:parallel-feature-build` for building independent features in parallel.
