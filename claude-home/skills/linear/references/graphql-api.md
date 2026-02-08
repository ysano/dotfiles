# Linear GraphQL API リファレンス

## エンドポイント

```
POST https://api.linear.app/graphql
Authorization: lin_api_*
Content-Type: application/json
```

## Issue 取得

### 自分にアサインされた Issue
```graphql
query {
  viewer {
    assignedIssues(
      filter: { state: { type: { nin: ["completed", "canceled"] } } }
      first: 50
    ) {
      nodes {
        id identifier title description priority priorityLabel
        state { name type }
        assignee { name email }
        labels { nodes { name } }
        project { name }
        team { key name }
        estimate dueDate url
      }
      pageInfo { hasNextPage endCursor }
    }
  }
}
```

### フィルタ付き Issue 検索
```graphql
query($teamId: String, $assigneeId: String, $stateType: String) {
  issues(
    filter: {
      team: { id: { eq: $teamId } }
      assignee: { id: { eq: $assigneeId } }
      state: { type: { eq: $stateType } }
    }
    first: 50
    orderBy: updatedAt
  ) {
    nodes { id identifier title priority state { name } }
    pageInfo { hasNextPage endCursor }
  }
}
```

### フィルタオプション
- `assignee`: `{ id: { eq: "..." } }` / `{ null: true }`（未アサイン）
- `state`: `{ type: { eq: "started" } }` / `{ name: { eq: "In Progress" } }`
- `priority`: `{ gte: 1, lte: 2 }`（Urgent + High）
- `team`: `{ key: { eq: "ENG" } }`
- `project`: `{ id: { eq: "..." } }`
- `labels`: `{ name: { in: ["bug", "feature"] } }`
- `dueDate`: `{ lt: "2026-03-01" }`

## ミューテーション

### Issue 作成
```graphql
mutation($input: IssueCreateInput!) {
  issueCreate(input: $input) {
    success
    issue { id identifier title url }
  }
}
# variables: { "input": { "teamId": "...", "title": "...", "description": "...", "priority": 3 } }
```

### Issue 更新
```graphql
mutation($id: String!, $input: IssueUpdateInput!) {
  issueUpdate(id: $id, input: $input) {
    success
    issue { id identifier title state { name } }
  }
}
# variables: { "id": "issue-uuid", "input": { "stateId": "...", "priority": 2 } }
```

### コメント追加
```graphql
mutation($input: CommentCreateInput!) {
  commentCreate(input: $input) {
    success
    comment { id body }
  }
}
# variables: { "input": { "issueId": "...", "body": "Comment text" } }
```

### Issue アーカイブ
```graphql
mutation($id: String!) {
  issueArchive(id: $id) { success }
}
```

## ページネーション

Relay Connection パターン（カーソルベース）:
- `first`: 取得件数（最大 250）
- `after`: 次ページカーソル（`pageInfo.endCursor`）
- `pageInfo.hasNextPage`: 次ページの有無

## Team / Project / WorkflowState 取得

```graphql
# Team 一覧
query { teams { nodes { id key name } } }

# Project 一覧
query { projects(first: 50) { nodes { id name state } } }

# Team の WorkflowState 一覧
query($teamId: String!) {
  team(id: $teamId) {
    states { nodes { id name type position } }
  }
}
```
