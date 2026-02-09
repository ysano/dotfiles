# Jira MCP ツールリファレンス

Atlassian MCP サーバーが提供するツール一覧。Plugin により自動接続される。

## Discovery-First原則

**重要**: REST APIが利用できない環境では、すべての操作前にメタデータを動的に探索する必要がある。

### 推奨ワークフロー（課題作成・編集時）
1. **Context Check**: `getAccessibleAtlassianResources` で cloudId 取得
2. **Resource Verification**: `getVisibleJiraProjects` でプロジェクトKey確認
3. **Schema Learning**: `getJiraIssueTypeMetaWithFields` でフィールド構成を確認
4. **Execution**: 正しいフィールドIDと必須フィールドを満たして実行

### エラー回復パターン（JQL検索失敗時）
1. **エラー解析**: `Field 'xyz' does not exist` エラーを検知
2. **実データ参照**: `searchJiraIssuesUsingJql(jql="project=DEV order by created desc", maxResults=1)` で最新課題を取得
3. **フィールドマッピング**: 取得した課題の `fields` プロパティから正しいカスタムフィールドID（例: `customfield_10050`）を特定
4. **クエリ再構築**: JQLをカスタムフィールドIDに書き換えて再実行

## Jira ツール

| ツール | 引数 | 用途 | 必須タイミング |
|---|---|---|---|
| `getAccessibleAtlassianResources` | なし | アクセス可能なサイト（cloudId）一覧を取得 | **セッション開始時に必須** |
| `getVisibleJiraProjects` | cloudId, action | Project 一覧 | 課題作成前、プロジェクト特定時 |
| `getJiraProjectIssueTypesMetadata` | cloudId, projectIdOrKey | Issue Type 一覧 | 課題作成前、タイプID特定時 |
| `getJiraIssueTypeMetaWithFields` | cloudId, projectIdOrKey, issueTypeId | フィールド構成（スキーマ） | **課題作成・編集前に必須** |
| `searchJiraIssuesUsingJql` | cloudId, jql, fields, maxResults | JQL 検索 | 課題検索時 |
| `getJiraIssue` | cloudId, issueIdOrKey | Issue 詳細 | 詳細情報取得時 |
| `createJiraIssue` | cloudId, projectKey, issueTypeName, summary, description, parent, assignee_account_id | Issue 作成 | スキーマ確認後 |
| `editJiraIssue` | cloudId, issueIdOrKey, fields | Issue 編集 | スキーマ確認後 |
| `transitionJiraIssue` | cloudId, issueIdOrKey, transitionId | ステータス変更 | 遷移可能ID確認後 |
| `getTransitionsForJiraIssue` | cloudId, issueIdOrKey | 遷移可能なステータス一覧 | ステータス変更前 |
| `addCommentToJiraIssue` | cloudId, issueIdOrKey, commentBody | コメント追加 | 任意 |
| `lookupJiraAccountId` | cloudId, searchString | ユーザー検索 | Assignee設定時 |

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
