# Jira MCP ツールリファレンス

Atlassian MCP サーバーが提供するツール一覧。Plugin により自動接続される。

## Jira ツール

| ツール | 引数 | 用途 |
|---|---|---|
| `searchJiraIssuesUsingJql` | cloudId, jql, fields, maxResults | JQL 検索 |
| `getJiraIssue` | cloudId, issueIdOrKey | Issue 詳細 |
| `createJiraIssue` | cloudId, projectKey, issueTypeName, summary, description, parent, assignee_account_id | Issue 作成 |
| `addCommentToJiraIssue` | cloudId, issueIdOrKey, commentBody | コメント追加 |
| `getVisibleJiraProjects` | cloudId, action | Project 一覧 |
| `getJiraProjectIssueTypesMetadata` | cloudId, projectIdOrKey | Issue Type 一覧 |
| `lookupJiraAccountId` | cloudId, searchString | ユーザー検索 |

## Confluence ツール

| ツール | 引数 | 用途 |
|---|---|---|
| `getConfluencePage` | cloudId, pageId, contentFormat | ページ取得 |
| `createConfluencePage` | cloudId, spaceId, title, body, contentFormat, parentId | ページ作成 |
| `updateConfluencePage` | cloudId, pageId, body, contentFormat, versionMessage | ページ更新 |
| `searchConfluenceUsingCql` | cloudId, cql | CQL 検索 |
| `getConfluenceSpaces` | cloudId | スペース一覧 |

## 横断ツール

| ツール | 用途 |
|---|---|
| `search` | Jira + Confluence 横断検索 |

## cloudId

全ツールに `cloudId` パラメータが存在するが、Atlassian Claude Plugin 経由では自動解決される。

手動取得が必要な場合（Plugin 未使用時）:
```bash
curl -s https://your-org.atlassian.net/_edge/tenant_info | jq '.cloudId'
```
