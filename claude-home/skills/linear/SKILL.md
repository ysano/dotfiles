---
name: linear
description: >
  Linear のデータモデル、MCP セットアップ、GraphQL API の知識ベース。
  詳細な API パターンや MCP ツール一覧は references/ を参照。
  チケット管理・同期エージェントが参照。
---

## リファレンスガイド

| ファイル | 内容 | 参照タイミング |
|---|---|---|
| `references/mcp-setup.md` | MCP セットアップ、サーバー選択、ツール一覧 | MCP 導入・ツール引数確認時 |
| `references/graphql-api.md` | クエリ・ミューテーション・フィルタ・ページネーション | MCP ツールでカバーできない高度な操作時 |

## データモデル

### Issue
- 所属: Team（必須）、Project（任意）、Cycle（任意）
- 識別子: `TEAM-123` 形式（Team prefix + 連番）
- Assignee: **単一ユーザーのみ**（GitHub と異なり複数不可）
- Priority: 0（None）/ 1（Urgent）/ 2（High）/ 3（Medium）/ 4（Low）
- Estimate: ポイントベース（チーム設定で選択肢をカスタマイズ可能）

### Project
- 複数 Team の Issue を横断的にグループ化
- Status: planned / started / paused / completed / cancelled
- マイルストーン機能あり

### Team
- Issue の必須所属先。ワークフロー状態（WorkflowState）は Team ごとに定義
- Team key がチケット識別子のプレフィックス（例: `ENG`-123）

### Cycle（スプリント）
- Team 単位の固定期間イテレーション
- 自動スケジューリング対応

### Label
- Workspace-level または Team-level で定義
- 階層構造（親ラベル → 子ラベル）対応

### WorkflowState
- Team ごとに定義される Issue のステータス
- タイプ: `backlog` / `unstarted` / `started` / `completed` / `cancelled`

## 認証

2つの認証方式がサーバーにより異なる:

| サーバー | 認証方式 | セットアップ |
|---|---|---|
| **公式クラウド** (`mcp.linear.app`) | OAuth（ブラウザ認可） | `claude mcp add` 後に `/mcp` で認可 |
| **@tacticlaunch/mcp-linear** | API Token (`lin_api_*`) | env に `LINEAR_API_TOKEN` を設定 |

API Token 取得: https://linear.app/settings/api → Personal API Keys

## スクリプト

### `scripts/sync_linear_tasks.py`
**用途**: MCP 未導入時のデバッグ・個人タスク確認用。
自分にアサインされた未完了 Issue のみ取得する簡易ツール。
チーム全体の Issue 取得や同期処理には MCP または GraphQL を使用すること。

```bash
cp .claude/skills/linear/.env.example .env  # API キーを設定
pip install requests mdutils python-dotenv
python .claude/skills/linear/scripts/sync_linear_tasks.py
```

## レート制限

| 項目 | 値 |
|---|---|
| リクエスト上限 | 2,000 / 時（API Key 単位） |
| 429 応答時 | 60秒待機後リトライ |
| ページネーション最大 | `first: 250`（Relay Connection 形式） |

<constraints>
## 制約・Gotchas

1. **Team 必須**: Issue 作成時に `teamId` が必須。省略するとエラー
2. **単一 Assignee**: 1つの Issue に割り当て可能なユーザーは1人のみ
3. **Priority は数値体系**: 0=None, 1=Urgent, 2=High, 3=Medium, 4=Low（直感と逆で 1 が最高）
4. **WorkflowState は Team 固有**: 同じ名前の状態でも Team が異なれば ID が異なる
5. **GraphQL のみ**: REST API は提供されていない
6. **identifier は読み取り専用**: `ENG-123` 形式の識別子は自動生成され変更不可
7. **Cycle は Team 単位**: プロジェクト横断のスプリントは Cycle では表現できない
8. **アーカイブは復元可能**: `issueArchive` で削除ではなくアーカイブ
9. **MCP 接続不安定**: 公式クラウドは接続失敗の可能性あり。リトライまたは GraphQL フォールバック推奨
</constraints>
