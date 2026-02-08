# Linear MCP セットアップ

## 接続の安定性

Linear 公式の注意事項:
> Remote MCP connections are still early and the connection may fail or require multiple attempts.
> If you experience issues, try restarting your client or disabling and re-enabling the Linear MCP server.

接続失敗時は GraphQL API をフォールバックとして使用する（`references/graphql-api.md` 参照）。

## サーバー選択

| サーバー | 形式 | 認証 | 特徴 |
|---|---|---|---|
| **公式クラウド** | SSE (HTTP) | OAuth（ブラウザ） | Linear 公式推奨。API Token 不要 |
| **@tacticlaunch/mcp-linear** | stdio (npx) | API Token (`lin_api_*`) | 47ツール。公式より多機能だが非公式 |

## Claude Code セットアップ

### 公式クラウド（推奨）
```bash
claude mcp add --transport sse linear-server https://mcp.linear.app/sse
```
初回接続時に `/mcp` でブラウザ OAuth 認可が必要。

### @tacticlaunch/mcp-linear（多機能）
```json
{
  "mcpServers": {
    "linear": {
      "command": "npx",
      "args": ["-y", "@tacticlaunch/mcp-linear"],
      "env": {
        "LINEAR_API_TOKEN": "lin_api_xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"
      }
    }
  }
}
```
API Token は https://linear.app/settings/api → Personal API Keys で取得。

## 利用可能なツール（@tacticlaunch/mcp-linear: 47個）

公式クラウドのツール数は限定的（Issue 検索・作成・更新・コメント）。
以下は @tacticlaunch/mcp-linear の全ツール。

### Issue 取得・検索（4個）
| ツール | 必須引数 | 任意引数 | 用途 |
|---|---|---|---|
| `linear_getIssues` | - | limit | 最近の Issue 一覧 |
| `linear_getIssueById` | id (`ABC-123` 形式) | - | Issue 詳細 + コメント + 履歴 |
| `linear_searchIssues` | - | query, teamId, assigneeId, projectId, states[], limit | フィルタ検索 |
| `linear_getComments` | issueId | limit | コメント一覧 |

### Issue 作成・更新（3個）
| ツール | 必須引数 | 任意引数 |
|---|---|---|
| `linear_createIssue` | title, teamId | description, assigneeId, priority (0-4), projectId, cycleId, estimate, dueDate, labelIds[], parentId, stateId |
| `linear_updateIssue` | id | title, description, stateId, priority, projectId, assigneeId, cycleId, estimate, dueDate, labelIds[], addedLabelIds[], removedLabelIds[] |
| `linear_createComment` | issueId, body | parentId (スレッド返信) |

### Issue 管理（11個）
| ツール | 必須引数 | 用途 |
|---|---|---|
| `linear_assignIssue` | issueId, assigneeId | アサイン |
| `linear_setIssuePriority` | issueId, priority | 優先度設定 |
| `linear_addIssueLabel` | issueId, labelId | ラベル追加 |
| `linear_removeIssueLabel` | issueId, labelId | ラベル削除 |
| `linear_archiveIssue` | issueId | アーカイブ |
| `linear_transferIssue` | issueId, teamId | チーム移動 |
| `linear_duplicateIssue` | issueId | 複製 |
| `linear_convertIssueToSubtask` | issueId, parentIssueId | サブタスク化 |
| `linear_createIssueRelation` | issueId, relatedIssueId, type | 関連付け (blocks/related/duplicate) |
| `linear_getIssueHistory` | issueId | 変更履歴 |
| `linear_subscribeToIssue` | issueId | 購読（開発中） |

### Team・組織（4個）
| ツール | 用途 |
|---|---|
| `linear_getTeams` | Team 一覧（id, key, name, states） |
| `linear_getWorkflowStates` | Team の WorkflowState 一覧（teamId 必須） |
| `linear_getViewer` | 認証ユーザー情報 |
| `linear_getUsers` | 組織内ユーザー一覧 |

### Project（5個）
| ツール | 必須引数 | 用途 |
|---|---|---|
| `linear_getProjects` | - | 一覧 |
| `linear_createProject` | name, teamIds | 作成 |
| `linear_updateProject` | id | 更新 |
| `linear_addIssueToProject` | issueId, projectId | Issue 追加 |
| `linear_getProjectIssues` | projectId | Project 内 Issue 一覧 |

### Cycle（3個）
| ツール | 必須引数 | 用途 |
|---|---|---|
| `linear_getCycles` | - | 一覧（teamId で絞込可） |
| `linear_getActiveCycle` | teamId | アクティブ Cycle + 進捗 |
| `linear_addIssueToCycle` | issueId, cycleId | Cycle に追加 |

### Initiative（10個）
| ツール | 必須引数 | 用途 |
|---|---|---|
| `linear_getInitiatives` | - | 一覧 |
| `linear_getInitiativeById` | initiativeId | 詳細 |
| `linear_createInitiative` | name | 作成 |
| `linear_updateInitiative` | initiativeId | 更新 |
| `linear_archiveInitiative` / `unarchive` / `delete` | initiativeId | ライフサイクル管理 |
| `linear_getInitiativeProjects` | initiativeId | 配下 Project 一覧 |
| `linear_addProjectToInitiative` | initiativeId, projectId | Project 追加 |
| `linear_removeProjectFromInitiative` | initiativeId, projectId | Project 削除 |

### Label・その他
| ツール | 用途 |
|---|---|
| `linear_getLabels` | Workspace/Team ラベル一覧 |
| `linear_getOrganization` | 組織情報 |
