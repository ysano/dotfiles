3-Phase Planning: AI-DLC のスプリント計画モデル。

## Why

従来のスプリントプランニングは 2-4 時間の合議で、見積もりは直感とチームの記憶に依存していた。
AI-DLC では開発サイクルが劇的に圧縮されるため、計画自体も「AI 事前分析 + 人間の短時間承認」に再設計する必要がある。
ボトルネックが「コードを書く力」から「何を作るべきかの判断力」に移行している。

## What: 3 フェーズモデル

### Phase 1: AI Pre-Planning（非同期・自動）

ミーティング**前**に AI が自動実行する分析。

| 分析項目 | データソース | 出力 |
|---|---|---|
| Spec 品質チェック | バックログの Issue/チケット | 5 要素充足率レポート |
| 依存関係検出 | `gh issue list` + label/milestone | 依存グラフ + ブロッカー一覧 |
| ベロシティ推定 | `git log` 完了ペース | スコープ推定値 |
| キャパシティ計算 | チーム規模 x 稼働日数 | 推奨スプリントスコープ |

**Spec 品質チェック**: Atomic Spec 5 要素（背景・現状・あるべき姿・制約・検証方法）の充足を検証。
不備があるチケットは Spec Definition ステータスに差し戻し候補としてマーク。

> Atomic Spec 詳細 → `ticket-management` > `references/atomic-spec.md`

### Phase 2: 人間の戦略判断（同期・30分-1時間）

AI 分析結果を受けて人間が決定する事項:

1. **スコープ承認**: AI 提案のスプリントスコープを承認または修正
2. **AI/人間タスク分担**: 「何を AI に任せるか」「何を人間が直接実装するか」の判断
3. **検証時間の確保**: AI 出力の検証に充てる時間を明示的に計画
4. **ハイリスク領域**: セキュリティ・アーキテクチャ変更の人間レビュー方針

核心的な問い: 「AI が作れるか」ではなく「**作るべきか（そしてなぜか）**」

### Phase 3: 配信準備

承認後のタスク配信:

1. チケットを Agent Loop の Triage → Spec Definition に遷移
2. AI エージェントへのタスク割当
3. マイクロスプリント長の確定（1-3日 / 3-5日 / 1週間）

> Agent Loop 詳細 → `ticket-management` > `references/agent-loop.md`

## How (for Claude Code)

### `/ai-dlc:plan` が実行するステップ

**Phase 1 データ収集**:

```bash
# バックログ取得
gh issue list --state open --label "sprint-candidate" --json number,title,body,labels,milestone

# ベロシティ算出（直近 N 日の完了 Issue）
gh issue list --state closed --json number,closedAt,labels

# PR ベースの完了ペース
git log --since="30 days ago" --merges --format="%h|%ad|%s" --date=short

# 依存関係（milestone ベース）
gh issue list --state open --json number,title,milestone,labels
```

**Phase 1 出力テンプレート**:

```markdown
## Sprint Pre-Planning Report

### Spec Quality
| Issue | 5要素充足 | 不足要素 | 推奨アクション |
|---|---|---|---|

### Scope Estimate
- Previous velocity: [N] issues/sprint
- Available capacity: [N] person-days
- Recommended scope: [N] issues

### Dependencies & Blockers
- [Issue] blocks [Issue]: [reason]

### Risks
- [risk description]: [mitigation]
```

**Phase 2 ブリーフィング出力**:

```markdown
## Decision Points

1. **Scope**: AI recommends [N] issues. Approve/adjust?
2. **AI vs Human**: [list of high-complexity items requiring human implementation]
3. **Review budget**: [N] hours allocated for AI output verification
4. **High-risk items**: [items requiring senior review]
```

## Cross-ref

- Atomic Spec 5 要素 → `ticket-management` > `references/atomic-spec.md`
- Agent Loop ステータス遷移 → `ticket-management` > `references/agent-loop.md`
- スケール別頻度 → `ai-dlc-ceremonies` SKILL.md
