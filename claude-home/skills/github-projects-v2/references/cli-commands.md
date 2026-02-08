# gh CLI コマンドリファレンス

## 認証

トークンスコープ `project` が必要。確認: `gh auth status`、更新: `gh auth refresh -s project`

## Project ライフサイクル

```bash
# 一覧（デフォルト30件、--closed でクローズ済み含む）
gh project list --owner OWNER --limit 100 --format json

# 詳細
gh project view NUMBER --owner OWNER --format json

# 作成
gh project create --owner OWNER --title "PROJECT_NAME"

# 編集
gh project edit NUMBER --owner OWNER --title "New Title" --visibility PRIVATE

# クローズ / 再オープン
gh project close NUMBER --owner OWNER
gh project close NUMBER --owner OWNER --undo

# 削除
gh project delete NUMBER --owner OWNER
```

## Item 操作

```bash
# 一覧（デフォルト30件）
gh project item-list NUMBER --owner OWNER --limit 200 --format json

# Issue/PR を追加
gh project item-add NUMBER --owner OWNER --url https://github.com/OWNER/REPO/issues/1

# Draft Issue 作成
gh project item-create NUMBER --owner OWNER --title "TITLE" --body "BODY"

# フィールド編集（1回1フィールドのみ）
gh project item-edit --id ITEM_ID --project-id PROJECT_ID --field-id FIELD_ID --text "value"
gh project item-edit --id ITEM_ID --project-id PROJECT_ID --field-id FIELD_ID --number 5
gh project item-edit --id ITEM_ID --project-id PROJECT_ID --field-id FIELD_ID --date "2025-01-15"
gh project item-edit --id ITEM_ID --project-id PROJECT_ID --field-id FIELD_ID --single-select-option-id OPTION_ID
gh project item-edit --id ITEM_ID --project-id PROJECT_ID --field-id FIELD_ID --iteration-id ITER_ID
gh project item-edit --id ITEM_ID --project-id PROJECT_ID --field-id FIELD_ID --clear

# アーカイブ / 解除
gh project item-archive NUMBER --owner OWNER --id ITEM_ID
gh project item-archive NUMBER --owner OWNER --id ITEM_ID --undo

# 削除
gh project item-delete NUMBER --owner OWNER --id ITEM_ID
```

## Field 操作

```bash
# 一覧
gh project field-list NUMBER --owner OWNER --format json

# 作成（TEXT, NUMBER, DATE, SINGLE_SELECT のみ。ITERATION は UI か GraphQL）
gh project field-create NUMBER --owner OWNER --name "Priority" --data-type "SINGLE_SELECT" --single-select-options "High,Medium,Low"

# 削除
gh project field-delete --id FIELD_ID
```

## Link 操作

```bash
# リポジトリをプロジェクトにリンク
gh project link NUMBER --owner OWNER --repo REPO_NAME
gh project unlink NUMBER --owner OWNER --repo REPO_NAME
```

## gh CLI での変数の渡し方

- `-f key=value` --- 文字列変数
- `-F key=value` --- 数値・ブール・null（JSON 型推論）

## ページネーション

- `--limit N` で取得数を指定（デフォルト30件）
- CLI が内部的にカーソルページネーションを処理
