---
description: "Create a new ticket within an epic following the Product as Code specification"
---

## Instructions

PAC 仕様に従って Epic 内に新しい Ticket を作成する: **$ARGUMENTS**

1. **環境検証**: `.pac/` ディレクトリと設定の存在を確認
2. **Epic 選択**: `--epic` 指定があれば検証、なければ既存 Epic を一覧表示して対話選択
3. **情報収集**: チケット名、タイプ（feature/bug/task/spike）、説明、担当者、優先度を取得
4. **ID 生成**: `ticket-[epic-short]-[sequence]` 形式、全チケット間で一意性を検証
5. **受け入れ基準・タスク定義**: 最低2つの受け入れ基準と実装タスクをユーザーと定義
6. **YAML 作成**: PAC v0.1.0 仕様に準拠した Ticket YAML を `.pac/tickets/[ticket-id].yaml` に書き出し
7. **Epic リンク**: 親 Epic のチケットリストにエントリを追加
8. **サマリー表示**: 作成したチケット情報と次のアクション（ブランチ作成、ステータス更新等）を提示

### 引数

- `--epic <epic-id>`: 親 Epic ID（必須）
- `--name <name>`: チケット名
- `--type <feature|bug|task|spike>`: タイプ
- `--assignee <assignee>`: 担当者
- `--priority <low|medium|high|critical>`: 優先度
- `--create-branch`: git ブランチを自動作成
