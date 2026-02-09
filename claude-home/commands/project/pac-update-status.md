---
description: "Update ticket status and track progress in Product as Code workflow"
---

## Instructions

PAC チケットのステータスを更新し進捗を追跡する: **$ARGUMENTS**

1. **チケット選択**: `--ticket` 指定があれば検証、なければステータス別にグルーピングして対話選択
2. **現在の状態表示**: チケット名、ステータス、担当者、受け入れ基準の進捗を表示
3. **ステータス遷移検証**: 有効な遷移のみ許可
   - `backlog` → `in-progress`, `cancelled`
   - `in-progress` → `review`, `blocked`, `backlog`
   - `review` → `done`, `in-progress`
   - `blocked` → `in-progress`, `cancelled`
   - `cancelled` → `backlog`
4. **遷移時の特別処理**:
   - → `in-progress`: 担当者未設定なら確認、フィーチャーブランチ提案
   - → `review`: 全タスク完了チェック、受け入れ基準警告
   - → `done`: 受け入れ基準すべてチェック済みか検証、完了タイムスタンプ記録
5. **親 Epic 更新**: Epic のチケットリスト内ステータスを同期
6. **サマリー表示**: 変更前後のステータス、タスク進捗、次の推奨アクションを提示

### 引数

- `--ticket <ticket-id>`: チケット ID
- `--status <backlog|in-progress|review|blocked|done|cancelled>`: 新ステータス
- `--assignee <assignee>`: 担当者更新
- `--comment <comment>`: コメント追加
- `--epic <epic-id>`: Epic でフィルタ（対話選択時）
- `--force`: 検証警告を無視
