---
name: github-projects-v2
description: >
  GitHub Projects V2 のデータモデル、ID体系、制約事項の知識ベース。
  詳細な CLI コマンドや GraphQL パターンは references/ を参照。
  チケット管理・ボード管理エージェントが参照。
---

GitHub Projects V2 を `gh` CLI と GraphQL API で操作するための知識ベース。

## リファレンスガイド

| ファイル | 内容 | 参照タイミング |
|---|---|---|
| `references/cli-commands.md` | `gh project` サブコマンド一覧、引数、ページネーション | CLI で操作するとき |
| `references/graphql-api.md` | クエリ・ミューテーション・ページネーションパターン | CLI で不可能な操作、バッチ処理、高度な取得 |
| `references/filtering.md` | 修飾子・演算子・特殊値 | ビューフィルタ設定・Issue 検索時 |

## データモデル

### Project (ProjectV2)
- スコープ: User-level または Organization-level
- 最大 **50,000** アイテム（アクティブ + アーカイブ合計）
- 最大 **50** カスタムフィールド
- 可視性: PUBLIC / PRIVATE
- Node ID: opaque 文字列（例: `PVT_kwDOABCDEF4AGHIJ`）

### Item (ProjectV2Item)
3種類のコンテンツ:
- **Issue** --- リポジトリに紐付く Issue
- **PullRequest** --- リポジトリに紐付く PR
- **DraftIssue** --- プロジェクト内のみ（リポジトリ・labels・milestones なし、通知なし）

権限のないアイテムは `REDACTED` 型として返される。

### Field Types

#### 組み込みメタデータフィールド（Issue/PR のプロパティ）
`updateProjectV2ItemFieldValue` では**変更不可**。専用ミューテーションを使う:
- Assignees → `addAssigneesToAssignable` / `removeAssigneesFromAssignable`
- Labels → `addLabelsToLabelable` / `removeLabelsFromLabelable`
- Milestone → `updateIssue` / `updatePullRequest`
- Repository → `transferIssue`

#### カスタムフィールド

| 型 | GraphQL 値の指定 | gh CLI フラグ |
|---|---|---|
| **Text** | `{text: "..."}` | `--text "..."` |
| **Number** | `{number: 5}` | `--number 5` |
| **Date** | `{date: "YYYY-MM-DD"}` | `--date "YYYY-MM-DD"` |
| **Single Select** | `{singleSelectOptionId: "ID"}` | `--single-select-option-id "ID"` |
| **Iteration** | `{iterationId: "ID"}` | `--iteration-id "ID"` |

- Single Select: 最大 **50** オプション（各オプションに color/description 設定可）
- Iteration: 日数/週数で期間設定、ブレーク挿入可能

### Views
- **Table** --- 高密度テーブル
- **Board** (Kanban) --- カラムベース
- **Roadmap** --- タイムライン（date/iteration フィールド必須）

### Built-in Workflows

| トリガー | アクション |
|---|---|
| Issue/PR クローズ時 | Status → "Done" |
| PR マージ時 | Status → "Done" |
| アイテム追加時 | Status → "Todo"（設定可能） |
| 条件合致 | 自動アーカイブ |
| リポジトリフィルタ合致 | 自動追加 |

## 認証

トークンスコープ `project` が必要。確認: `gh auth status`、更新: `gh auth refresh -s project`

<id_system>
## ID 体系

2種類の識別子が使い分けられる:

| 識別子 | 説明 | 用途 |
|---|---|---|
| **プロジェクト番号** (`NUMBER`) | オーナー内で一意の連番 | `gh project list/view/item-list/field-list` 等の位置引数 |
| **Node ID** (GraphQL ID) | グローバル一意の opaque 文字列 | `item-edit` の `--project-id`, `--id`, `--field-id` |

`item-edit` は他コマンドと異なり `--owner` がなく、`--project-id`（Node ID）を使用する。

### ID 取得フロー
```bash
# 1. Project の Node ID
gh project view NUMBER --owner OWNER --format json --jq '.id'

# 2. Field の Node ID と Option ID
gh project field-list NUMBER --owner OWNER --format json

# 3. Item の Node ID
gh project item-list NUMBER --owner OWNER --format json --jq '.items[].id'
```
</id_system>

## レート制限

| 認証方式 | 上限/時 |
|---|---|
| ユーザー (PAT/OAuth) | 5,000 ポイント |
| Enterprise Cloud ユーザー | 10,000 ポイント |
| GitHub App | 5,000 〜 12,500（スケーリング） |
| `GITHUB_TOKEN` (Actions) | 1,000（**Project アクセス不可**） |

- GraphQL クエリ = 1 ポイント、ミューテーション = 5 ポイント
- 二次制限: 同時100リクエスト、2,000ポイント/分
- ミューテーション間は最低 **1秒** 間隔推奨
- 確認: `gh api rate-limit`

<constraints>
## 制約・Gotchas

1. **追加と更新は同一コールで不可**: `addProjectV2ItemById` 後、別コールで `updateProjectV2ItemFieldValue`
2. **組み込みフィールドは `updateProjectV2ItemFieldValue` で変更不可**: Assignees, Labels, Milestone, Repository は専用ミューテーション
3. **`item-edit` は1回1フィールドのみ**: 複数フィールド更新は複数回実行
4. **`GITHUB_TOKEN` では Project アクセス不可**: PAT か GitHub App が必須
5. **`field-create` で ITERATION タイプ作成不可**: UI か GraphQL API を使用
6. **Single Select は最大50オプション**
7. **Draft Issue は通知を送信しない**
8. **テンプレートの auto-add ワークフローはコピーされない**
9. **`item-edit` に `--owner` フラグなし**: `--project-id`（Node ID）を使用
10. **GraphQL タイムアウト: 10秒**（超過時は追加ポイント消費）
</constraints>
