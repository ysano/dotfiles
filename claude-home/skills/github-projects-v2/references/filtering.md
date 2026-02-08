# フィルタリング構文リファレンス

## 修飾子

| 修飾子 | 例 |
|---|---|
| `assignee:USERNAME` | `assignee:octocat` |
| `label:LABEL` | `label:bug` |
| `milestone:"NAME"` | `milestone:"QA release"` |
| `repo:OWNER/REPO` | `repo:octocat/game` |
| `is:STATE` | `is:open`, `is:closed`, `is:merged` |
| `is:TYPE` | `is:issue`, `is:pr`, `is:draft` |
| `has:FIELD` / `no:FIELD` | `has:assignee`, `no:milestone` |
| `updated:EXPR` | `updated:>@today-30d` |

## 演算子

- **AND**: スペース区切り（`label:bug status:"In progress"`）
- **OR**: カンマ区切り（`label:bug,support`）
- **否定**: ハイフン（`-assignee:octocat`）
- **比較**: `>`, `>=`, `<`, `<=`, `..`（範囲）
- **特殊値**: `@me`, `@today`, `@today+7`, `@today-30d`, `@current`, `@next`, `@previous`
