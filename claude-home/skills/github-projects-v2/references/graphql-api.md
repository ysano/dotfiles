# GraphQL API リファレンス

## プロジェクト取得

```bash
# Organization
gh api graphql -f query='
query($org: String!, $num: Int!) {
  organization(login: $org) {
    projectV2(number: $num) { id title }
  }
}' -f org=ORG -F num=NUMBER

# User
gh api graphql -f query='
query($user: String!, $num: Int!) {
  user(login: $user) {
    projectV2(number: $num) { id title }
  }
}' -f user=USER -F num=NUMBER
```

## フィールド情報取得（型による分岐）

```bash
gh api graphql -f query='
query($id: ID!) {
  node(id: $id) {
    ... on ProjectV2 {
      fields(first: 50) {
        nodes {
          ... on ProjectV2Field { id name dataType }
          ... on ProjectV2SingleSelectField {
            id name
            options { id name color description }
          }
          ... on ProjectV2IterationField {
            id name
            configuration { duration startDay }
            iterations { id title startDate duration }
          }
        }
      }
    }
  }
}' -f id=PROJECT_ID
```

## アイテム + フィールド値の取得

```bash
gh api graphql -f query='
query($id: ID!, $cursor: String) {
  node(id: $id) {
    ... on ProjectV2 {
      items(first: 100, after: $cursor) {
        nodes {
          id
          fieldValues(first: 10) {
            nodes {
              ... on ProjectV2ItemFieldTextValue {
                text
                field { ... on ProjectV2FieldCommon { name } }
              }
              ... on ProjectV2ItemFieldNumberValue {
                number
                field { ... on ProjectV2FieldCommon { name } }
              }
              ... on ProjectV2ItemFieldDateValue {
                date
                field { ... on ProjectV2FieldCommon { name } }
              }
              ... on ProjectV2ItemFieldSingleSelectValue {
                name
                field { ... on ProjectV2FieldCommon { name } }
              }
              ... on ProjectV2ItemFieldIterationValue {
                iterationId startDate duration
                field { ... on ProjectV2FieldCommon { name } }
              }
            }
          }
          content {
            ... on Issue { title number url state assignees(first: 5) { nodes { login } } labels(first: 5) { nodes { name } } }
            ... on PullRequest { title number url state assignees(first: 5) { nodes { login } } }
            ... on DraftIssue { title body }
          }
        }
        pageInfo { hasNextPage endCursor }
      }
    }
  }
}' -f id=PROJECT_ID
```

## ミューテーション

```bash
# アイテム追加（既存 Issue/PR）--- 既に追加済みの場合は既存 item ID が返る
gh api graphql -f query='
mutation($project: ID!, $content: ID!) {
  addProjectV2ItemById(input: {projectId: $project, contentId: $content}) {
    item { id }
  }
}' -f project=PROJECT_ID -f content=ISSUE_NODE_ID

# Draft Issue 追加
gh api graphql -f query='
mutation($project: ID!, $title: String!, $body: String) {
  addProjectV2DraftIssue(input: {projectId: $project, title: $title, body: $body}) {
    projectItem { id }
  }
}' -f project=PROJECT_ID -f title="TITLE" -f body="BODY"

# フィールド値更新（Single Select の例）
gh api graphql -f query='
mutation($project: ID!, $item: ID!, $field: ID!, $value: ProjectV2FieldValue!) {
  updateProjectV2ItemFieldValue(input: {
    projectId: $project, itemId: $item, fieldId: $field, value: $value
  }) {
    projectV2Item { id }
  }
}' -f project=PROJECT_ID -f item=ITEM_ID -f field=FIELD_ID -f value='{"singleSelectOptionId":"OPTION_ID"}'

# フィールド値クリア
gh api graphql -f query='
mutation($project: ID!, $item: ID!, $field: ID!) {
  clearProjectV2ItemFieldValue(input: {projectId: $project, itemId: $item, fieldId: $field}) {
    projectV2Item { id }
  }
}' -f project=PROJECT_ID -f item=ITEM_ID -f field=FIELD_ID

# アイテム削除
gh api graphql -f query='
mutation($project: ID!, $item: ID!) {
  deleteProjectV2Item(input: {projectId: $project, itemId: $item}) {
    deletedItemId
  }
}' -f project=PROJECT_ID -f item=ITEM_ID
```

## ページネーション

- Relay Cursor Connection 仕様
- `first: 1-100` + `after: endCursor` で前方ページネーション
- `hasNextPage` が `false` になるまで繰り返し
- 1コールあたり最大 **500,000** ノード

```graphql
items(first: 100, after: $cursor) {
  nodes { ... }
  pageInfo { hasNextPage endCursor }
}
```
