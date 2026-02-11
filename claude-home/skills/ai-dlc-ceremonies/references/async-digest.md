Async Digest: AI-DLC の 2 層デイリーダイジェストモデル。

## Why

従来のデイリースクラム（15 分スタンドアップ）は「45 分に膨張」「同じブロッカーが繰り返し報告」という構造的問題を抱えていた。
AI-DLC ではステータス報告の約 70% を AI が自動化し、人間は AI が解決できない判断のみに集中する。
40 名以上の分散チームで週 5-7 時間を回収した事例もある。

## What: 2 層構造

### Layer 1: AI Digest（非同期・自動生成）

毎朝（または設定タイミング）AI が自動コンパイルする情報:

| カテゴリ | 内容 | データソース |
|---|---|---|
| **コミット要約** | 前日の活動サマリー | `git log --since` |
| **PR 状況** | オープン/マージ/レビュー待ち | `gh pr list` |
| **進捗 vs トレンド** | スプリント進捗とベロシティの比較 | Issue 完了数 vs 計画 |
| **ブロッカー検出** | stale PR、長期未更新 Issue | PR/Issue メタデータ |
| **4 時間ルール違反** | AI Implementation 滞留チケット | ステータス + タイムスタンプ |
| **レビューボトルネック** | 未マージ PR のエイジング | `gh pr list --state open` |

### Layer 2: 判断アジェンダ（同期・5-10分）

AI ダイジェストを前提に、人間が集中すべき事項:

1. **AI で解決不能なブロッカー**: 技術的判断、組織的調整が必要な項目
2. **エスカレーション対象**: 4 時間ルールで差し戻されたチケット、Churn Alert 発生チケット
3. **クロスチーム依存**: 人間同士での調整が必要な外部依存

### 設計上の注意

**儀式の空洞化（Ritual Hollowing）防止**: AI が自動化しすぎるとチームメンバーがプロセスから心理的に離脱するリスクがある。AI は儀式を補強するが、チームの存在や省察を代替してはならない。

## How (for Claude Code)

### `/ai-dlc:digest` が実行するステップ

**データ収集**:

```bash
# コミット要約（デフォルト: 過去 24 時間）
git log --since="24 hours ago" --all --format="%h|%an|%ad|%s" --date=short

# PR 状況
gh pr list --state open --json number,title,author,createdAt,reviewDecision
gh pr list --state merged --json number,title,mergedAt --limit 20

# Issue 進捗
gh issue list --state open --json number,title,labels,assignees,updatedAt
gh issue list --state closed --json number,title,closedAt --limit 20
```

**Layer 1 出力テンプレート**:

```markdown
## AI Digest - [Date]

### Activity Summary
- Commits: [N] by [N] authors
- PRs merged: [N] | PRs opened: [N] | PRs pending review: [N]

### Sprint Progress
- Completed: [N]/[Total] issues ([%])
- Velocity trend: [on track / behind / ahead]

### Blockers
| Type | Item | Age | Action Needed |
|---|---|---|---|
| Stale PR | #[N] | [days] | Review needed |
| 4h violation | #[N] | [hours] | Spec revision |
| Churn alert | #[N] | [turns] | Spec clarification |

### Review Queue
| PR | Author | Age | Status |
|---|---|---|---|
```

**Layer 2 出力テンプレート**:

```markdown
### Human Decision Agenda

1. **Blockers requiring human judgment**
   - [item]: [context and options]

2. **Escalations**
   - [ticket]: [reason for escalation]

3. **Cross-team coordination**
   - [dependency]: [parties involved, action needed]
```

**出力バリアント**:
- **Markdown**（デフォルト）: 上記テンプレート
- **Slack 形式**: `*Section:*` + bullet points、コンパクトな 1 メッセージ

## Cross-ref

- 4 時間ルール / Churn 3 回ルール → `ticket-management` > `references/ai-native-practices.md`
- Agent Loop ステータス → `ticket-management` > `references/agent-loop.md`
