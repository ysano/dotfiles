# Atlassian MCP アーキテクチャ

Atlassian Model Context Protocol (MCP) サーバーを利用したJira Cloud統合の技術的制約と設計パターン。

## APIレス環境の制約

### 制約事項
1. **エンドポイント不可視**: エージェントは `GET /rest/api/3/field` を呼び出してフィールドリストを取得できない
2. **エラー抽象化**: HTTPステータスコード（400, 404, 403）はMCP層でラップされ、自然言語エラーとしてエージェントに返される
3. **データモデル依存**: APIバージョニング保護が薄いため、Jiraのデータモデル変更（例: 階層構造統一）に対して敏感

### 機会
- APIの詳細から解放され、MCPが提供する抽象度の高いツールで人間の意図に近い操作が可能
- プロジェクトごとのカスタム設定に対して、従来のハードコーディングより柔軟に適応可能

## MCPサーバーアーキテクチャ

### 通信プロトコル
- **プロトコル**: HTTPS（TLS 1.2以上）
- **認証**: OAuth 2.1
- **権限**: ユーザーの既存Jira権限スコープ内でのみ動作
- **対象**: Atlassian Cloud専用（Data Center/Server非対応）

### CloudIDの重要性
- すべてのMCPツール呼び出しに `cloudId` が必須
- Atlassianエコシステムでは、1アカウントが複数サイトにアクセス可能なため、サイトを一意に識別する
- エージェントはセッション開始時に `getAccessibleAtlassianResources` で取得・キャッシュする必要がある

## Discovery-Firstアーキテクチャ

### 推論ロジック（Discovery Loop）
課題作成・編集時のワークフロー:

```
1. Intent Analysis
   ↓ ユーザーの意図を解析（例: "プロジェクトDEVにバグ報告を作成"）

2. Resource Verification
   ↓ getVisibleJiraProjects → プロジェクト "DEV" の存在確認・Key取得
   ↓ getJiraProjectIssueTypesMetadata → "Bug" 課題タイプのID取得

3. Schema Learning
   ↓ getJiraIssueTypeMetaWithFields → "Bug" タイプの必須フィールド確認
   ↓ [分岐] 未知のカスタムフィールド（例: "発生環境"）がある場合、ユーザーに質問

4. Execution
   ↓ createJiraIssue → 収集した情報と正しいフィールドIDで実行
```

### 自己修復パターン（JQLエラー回復）
`Field 'Team' does not exist` エラー発生時:

```
1. エラー解析
   ↓ エラーメッセージから存在しないフィールド名 "Team" を特定

2. 実データ参照
   ↓ searchJiraIssuesUsingJql(jql="project=DEV order by created desc", maxResults=1)
   ↓ 最新課題を1件取得

3. フィールドマッピング
   ↓ 取得した課題の fields プロパティを走査
   ↓ "Team" という名前を持つカスタムフィールド（例: customfield_10050）を探す

4. クエリ再構築
   ↓ Team = 'A' を cf[10050] = 'A' に書き換えて再実行
```

## 2026年データモデル変更への対応

### 階層構造統一（Parent Link統合）
Atlassianは課題の親子関係を単一の `parent` フィールドに統合（2025年後半〜2026年）:

| 階層関係 | 従来 (Legacy) | 新 (Unified) |
|---|---|---|
| Story → Epic | Epic Link | **parent** |
| Epic → Initiative | Parent Link | **parent** |
| Subtask → Story | Parent | **parent** |

### JQL構文への影響
- ❌ **非推奨**: `"Epic Link" = PROJ-123` → エラーになる可能性
- ✅ **推奨**: `parent = PROJ-123` → エピック配下のストーリー検索
- ✅ **推奨**: `parent = STORY-50` → ストーリー配下のサブタスク検索

### API操作への影響
階層紐付け時のフィールド指定:

```json
{
  "parent": {
    "key": "EPIC-KEY-123"
  }
}
```

**重要**: Epic Link フィールドIDを直接指定してはいけない。常に統一された `parent` フィールドを使用する。

## ツール実行の注意事項

### ページネーション
- `searchJiraIssuesUsingJql` は `nextPageToken` または `startAt` を返す
- 一度に大量データ（maxResults=100以上）を取得せず、デフォルト（通常50件）で取得し必要に応じてページ送り
- トークン消費を抑えるため、`fields` 引数で取得フィールドを絞り込む（例: `summary,status,assignee`）

### Markdown対応
- `editJiraIssue` のDescriptionフィールドはADF（Atlassian Document Format）を要求
- MCPサーバーはMarkdown→ADF変換をサポートする場合がある
- まずMarkdownで試行し、失敗した場合はプレーンテキストに切り替え

### issueIdOrKey
- `getJiraIssue` 等でissueIdOrKeyはどちらでも動作するが、人間が読みやすいKey（例: PROJ-123）を優先使用

## トラブルシューティング

### エラーメッセージと対策

| エラーメッセージ | 原因 | 対策 |
|---|---|---|
| `Field 'xyz' does not exist` | 指定フィールドがJQLで認識されない（カスタムフィールドの可能性） | 最新課題を1件取得し、`fields` プロパティから正しいID（`customfield_xxxxx`）を探し、JQLを書き換え |
| `Operation value must be a string` | オブジェクト型・配列型フィールドに文字列を渡している | メタデータ (`getJiraIssueTypeMetaWithFields`) を確認。例: `components` は `[{"name": "ComponentA"}]` 形式 |
| `jqlTooComplex` | JQLクエリが複雑すぎるか検索対象が多すぎる | OR条件の数を減らすか、`project` で範囲を絞り込む |
| `Valid cloudId required` | cloudId引数が不足または間違っている | `getAccessibleAtlassianResources` を再実行して正しいIDを取得 |

## セキュリティ・監査

- **権限遵守**: ユーザーの既存Jira権限を超えた操作は不可
- **監査ログ**: 全MCPツール呼び出しは監査ログに記録
- **IP Allowlisting**: エンタープライズ設定でIP制限可能

## 参考情報
- Atlassian MCP Server公式ドキュメント: https://support.atlassian.com/atlassian-rovo-mcp-server/
- 階層構造変更: https://support.atlassian.com/jira-software-cloud/docs/upcoming-changes-epic-link-replaced-with-parent/
