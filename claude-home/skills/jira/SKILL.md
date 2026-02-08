---
name: jira
description: >
  Jira Cloud のデータモデル、MCP ツール、組み込み Skill の知識ベース。
  Atlassian Claude Plugin 経由で MCP ツールが利用可能。Confluence 連携も対応。
---

## リファレンスガイド

| ファイル | 内容 | 参照タイミング |
|---|---|---|
| `references/mcp-tools.md` | MCP ツール一覧、引数、cloudId 取得 | ツールの引数を確認するとき |
| `references/built-in-skills.md` | 組み込み Skill の詳細と使い分け | ワークフロー自動化時 |

## データモデル

### Issue
- 所属: Project（必須）
- Issue Type: Story / Task / Bug / Epic / Sub-task（Project ごとにカスタマイズ可能）
- Priority: Highest / High / Medium / Low / Lowest
- Assignee: 単一ユーザー
- 検索: JQL（Jira Query Language）による強力なフィルタリング

### Project
- プロジェクトキー（例: `PROJ`）で識別
- ボードタイプ: Scrum / Kanban / Team-managed
- Workflow: カスタマイズ可能な状態遷移

### Sprint
- Scrum ボード専用の固定期間イテレーション
- Active Sprint は Team あたり1つ

### Epic
- 大きな作業単位。複数 Issue をグループ化
- Epic Link で子 Issue を紐付け

### Status
- カテゴリ: To Do / In Progress / Done
- ワークフロー遷移ルールでカスタマイズ可能

## 認証

- **方式**: OAuth 2.1（Plugin が自動処理）
- **権限**: ユーザーの既存 Jira 権限を遵守。権限を超えた操作は不可
- **対象**: Atlassian Cloud のみ（Data Center / Server は非対応）
- **監査**: 監査ログ対応、IP allowlisting 対応

## MCP ツール

Jira Issue の検索・作成・更新、Project 情報取得、ユーザー検索、Confluence 横断検索が可能。
ツール名と引数の詳細は `references/mcp-tools.md` を参照。

## 組み込み Skill（5個）

Atlassian MCP サーバーに付属するワークフロー Skill:

| Skill | 用途 | 連携 |
|---|---|---|
| `spec-to-backlog` | Confluence 仕様 → Jira Epic + Issue 自動生成 | Confluence → Jira |
| `triage-issue` | バグ報告の重複検査・トリアージ | Jira |
| `search-company-knowledge` | 組織知識の横断検索・合成 | Jira + Confluence |
| `generate-status-report` | JQL → Confluence ステータスレポート自動生成 | Jira → Confluence |
| `capture-tasks-from-meeting-notes` | 会議メモ → Jira タスク自動抽出 | Confluence → Jira |

各 Skill のワークフロー詳細は `references/built-in-skills.md` を参照。

## フィールドマッピング骨格

| フィールド | GitHub | Linear | Jira |
|---|---|---|---|
| 優先度 | Labels (critical, ...) | Priority (0-4) | Priority (Highest/.../Lowest) |
| 状態 | open / closed | WorkflowState type | Status category (To Do/In Progress/Done) |
| 担当者 | Assignees (複数可) | Assignee (単一) | Assignee (単一) |
| ラベル | Labels | Labels | Labels / Components |
| 検索言語 | GitHub search syntax | GraphQL filter | JQL |

<constraints>
## 制約・Gotchas

1. **Cloud のみ**: Atlassian Cloud 専用。Data Center / Server は非対応
2. **権限遵守**: ユーザーの既存 Jira 権限を超えた操作は不可
3. **Epic 作成順序**: Epic を先に作成してから子 Issue を作成する必要がある
4. **Account ID**: ユーザー指定は表示名ではなく Account ID（`lookupJiraAccountId` で検索）
5. **JQL 必須**: 高度な検索には JQL の知識が必要
6. **cloudId**: Plugin 経由では自動解決される。手動 MCP 設定時は明示的に指定が必要（`references/mcp-tools.md` 参照）
</constraints>
