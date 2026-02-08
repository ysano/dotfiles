# Atlassian MCP 組み込み Skills

Atlassian MCP サーバーに付属する 5 つのワークフロー Skill。
ソース: https://github.com/atlassian/atlassian-mcp-server/tree/main/skills

## spec-to-backlog

Confluence の仕様ページから Jira バックログを自動生成する。

**ワークフロー**:
1. `getConfluencePage` で仕様を取得
2. 仕様を分析し Epic + 子 Issue を設計
3. `createJiraIssue` で Epic を先に作成
4. 子 Issue を `parent` 指定で作成

**注意**: Epic を先に作成しないと子 Issue が孤立する。

## triage-issue

バグ報告を受けて重複検査・トリアージを行う。

**ワークフロー**:
1. `searchJiraIssuesUsingJql` で類似 Issue を複数回検索
2. 重複候補を評価（タイトル・説明の類似度）
3. 重複なら既存 Issue にコメント追加、新規なら Bug Issue 作成

**注意**: 複数の検索クエリで網羅的にチェックしないと重複を見落としやすい。

## search-company-knowledge

Jira と Confluence を横断して組織の知識を検索・合成する。

**ワークフロー**:
1. `search`（横断）+ `searchJiraIssuesUsingJql` + `searchConfluenceUsingCql` を並行実行
2. 検索結果から関連ドキュメントを取得
3. 情報を統合し、引用付きで回答。矛盾があれば明示

## generate-status-report

Jira の Issue データから Confluence にステータスレポートを自動生成する。

**ワークフロー**:
1. `searchJiraIssuesUsingJql` で対象 Issue を抽出
2. メトリクス計算（完了率、ブロッカー数等）
3. `createConfluencePage` または `updateConfluencePage` で公開

## capture-tasks-from-meeting-notes

Confluence の会議メモからアクションアイテムを抽出し Jira タスクを作成する。

**ワークフロー**:
1. `getConfluencePage` で会議メモ取得
2. @mention 形式等からアクションアイテムを解析
3. `lookupJiraAccountId` でユーザー名 → Account ID を解決
4. `createJiraIssue` でタスク作成（assignee 付き）

**注意**: 同名ユーザーが存在する場合はユーザー確認が必要。
