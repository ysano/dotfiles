Diagnostic Session: AI-DLC のデータ駆動型レトロスペクティブモデル。

## Why

従来のレトロスペクティブは「グラウンドホッグ・デー（同じことの繰り返し）」問題を抱えていた。
同じ議論が繰り返され、アクションアイテムが実行されない。
AI-DLC では AI がスプリント診断レポートを事前生成し、「意見ではなくデータで始める」文化を確立する。
AI 追跡によりアクションアイテム完了率が約 60% → 約 92% に改善した事例がある。

## What: Before / During / After モデル

### Before: AI スプリント診断レポート

レトロの**前に** AI が自動生成する定量データ:

| メトリクス | 内容 | 目的 |
|---|---|---|
| **ベロシティ推移** | 計画 vs 実績のポイント比較 | スコープ見積もり精度の評価 |
| **品質メトリクス** | バグ密度、テストカバレッジ変化 | コード品質トレンド |
| **Interaction Churn** | 3 回超チケット一覧と原因分類 | Spec 品質の改善ポイント特定 |
| **AI-Confidence 分布** | スコア帯別のチケット数（利用可能な場合） | AI 有効性の定量評価 |
| **過去アクション追跡** | 前回レトロのアクションアイテム完了状況 | 継続的改善の追跡 |

### During: 人間主導の診断と意思決定

AI データを基盤に、**人間が主導して**議論する項目:

1. **AI 有効性評価**: 「AI は今スプリントで助けになったか、邪魔になったか」
2. **Spec 品質振り返り**: Churn が高かったチケットの原因分析（粒度超過? 仕様曖昧?）
3. **CLAUDE.md / SKILL 改善提案**: エージェントが繰り返し間違えるパターンの特定と対策
4. **過去アクション効果測定**: 前回のアクションと今スプリントの成果の相関分析

### After: アクション追跡

レトロで合意されたアクションアイテムの自動チケット化パターン:
- Issue 化して次スプリントバックログに投入
- 未着手のものを次回レトロ前にリマインド

## How (for Claude Code)

### `/ai-dlc:diagnose` が実行するステップ

**Before: データ収集**:

```bash
# ベロシティ: 直近スプリントの完了 Issue
gh issue list --state closed --json number,title,closedAt,labels --limit 100

# 品質: テストカバレッジの変化（プロジェクト依存）
# git diff で test ファイル増減を概算
git log --since="[sprint_start]" --stat -- "**/*test*" "**/*spec*"

# PR 統計: reject 率、修正ラウンド数
gh pr list --state merged --json number,title,reviews,commits --limit 50

# Churn 分析: 高チャーンチケットの特定（Turns-Used フィールドがある場合）
gh issue list --state closed --json number,title,labels,body --limit 100
```

**Before: 診断レポートテンプレート**:

```markdown
## Sprint Diagnostic Report

### Velocity
- Planned: [N] issues | Completed: [N] issues | Completion rate: [%]
- Trend: [improving / stable / declining] vs previous [N] sprints

### Quality Metrics
- PRs merged: [N] | Avg review rounds: [N]
- Test file changes: +[N] / -[N] files
- Bug issues opened this sprint: [N]

### Interaction Churn Analysis
| Issue | Turns | Root Cause | Category |
|---|---|---|---|
| #[N] | [N] | [ambiguous spec / scope too large / missing constraints] | [spec / scope / technical] |

High-churn pattern: [summary of most common root cause]

### Past Action Items
| Action | Status | Impact |
|---|---|---|
| [action from previous retro] | [done / in-progress / not started] | [observed effect] |
```

**During: 議論アジェンダテンプレート**:

```markdown
### Discussion Agenda

1. **AI Effectiveness**
   - Did AI help or hinder this sprint?
   - Tasks where AI excelled: [list]
   - Tasks where AI struggled: [list]

2. **Spec Quality Review**
   - High-churn tickets: [list with root causes]
   - Recommended Spec improvements: [list]

3. **CLAUDE.md / SKILL Improvements**
   - Recurring agent mistakes: [patterns]
   - Proposed additions: [specific rules or knowledge]

4. **Action Items from Previous Retro**
   - Completed: [list with observed impact]
   - Outstanding: [list with blockers]
```

**After: アクションアイテムテンプレート**:

```markdown
### Action Items

| # | Action | Owner | Target | Ticket |
|---|---|---|---|---|
| 1 | [specific action] | [person] | [next sprint] | [create: gh issue create] |
```

## Cross-ref

- Interaction Churn / AI-Confidence → `ticket-management` > `references/ai-native-practices.md`
- Churn 3 回ルール → `ticket-management` SKILL.md
- CLAUDE.md / SKILL 改善 → `/ai-dlc:calibrate`
