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

## jq フィルタパターン

```bash
# 特定フィールドの ID を取得
gh project field-list NUMBER --owner OWNER --format json \
  --jq '.fields[] | select(.name == "Status") | .id'

# 特定オプションの ID を取得
gh project field-list NUMBER --owner OWNER --format json \
  --jq '.fields[] | select(.name == "Status") | .options[] | select(.name == "In progress") | .id'

# 特定 Issue の Item ID を URL で検索
gh project item-list NUMBER --owner OWNER --format json \
  --jq '.items[] | select(.content.url == "https://github.com/OWNER/REPO/issues/123") | .id'
```

## 複数フィールド一括更新

```bash
PROJECT_ID="PVT_kwDOA3jeEM4BNkoc"
ITEM_ID="PVTI_..."

# Status を "In progress" に
gh project item-edit --project-id "$PROJECT_ID" --id "$ITEM_ID" \
  --field-id "PVTSSF_lADOA3jeEM4BNkoczg8hxsU" \
  --single-select-option-id "47fc9ee4"

# Priority を "P1" に
gh project item-edit --project-id "$PROJECT_ID" --id "$ITEM_ID" \
  --field-id "PVTSSF_lADOA3jeEM4BNkoczg8hxzQ" \
  --single-select-option-id "0a877460"

# Size を "M" に
gh project item-edit --project-id "$PROJECT_ID" --id "$ITEM_ID" \
  --field-id "PVTSSF_lADOA3jeEM4BNkoczg8hxzU" \
  --single-select-option-id "86db8eb3"
```

## 新規プロジェクト XML タグ生成

CLAUDE.md に追加する XML タグを生成する手順:

```bash
# 1. プロジェクトの Node ID と URL を取得
gh project view NUMBER --owner OWNER --format json --jq '.id'
gh project view NUMBER --owner OWNER --format json --jq '.url'

# 2. フィールドとオプションを JSON で取得
gh project field-list NUMBER --owner OWNER --format json
# => Single Select: options[] に {id, name} 配列
# => Iteration: options なし（空要素 <field .../> で記述）

# 3. 取得した情報で XML タグを構成
# <github-project id="NODE_ID" url="URL">
#   <field name="FIELD_NAME" id="FIELD_ID">
#     <option name="OPTION_NAME" id="OPTION_ID"/>
#   </field>
# </github-project>
```

`--format json` を付けないと Option ID が表示されない。
JSON で取得できない場合は [graphql-api.md](graphql-api.md) のフィールド情報取得クエリを使用。

## ページネーション

- `--limit N` で取得数を指定（デフォルト30件）
- CLI が内部的にカーソルページネーションを処理
