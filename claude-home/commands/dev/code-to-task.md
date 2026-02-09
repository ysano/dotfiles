---
description: "Scan codebase for TODO/FIXME comments and create Linear tasks"
---

## Instructions

Scan for task markers and create Linear tasks: **$ARGUMENTS**

### 1. Scan

以下のパターンをコードベースから検索:
- `TODO`, `FIXME`, `HACK`, `XXX`, `OPTIMIZE`, `REFACTOR`
- `@deprecated`, `TEMPORARY`, `REMOVE BEFORE`

対象ディレクトリが指定されていない場合は `src/` またはプロジェクトルートを使用。

### 2. Parse & Group

各マーカーから情報を抽出:
- ファイルパス、行番号、コメント内容
- 優先度の推定（FIXME > TODO > OPTIMIZE）
- モジュール/機能別にグループ化
- 重複を除去

### 3. Create Linear Tasks

Linear MCP を使用してタスク作成:
- タイトル: `[TYPE] file:line - description`
- 優先度: マーカー種別に基づいて設定
- 既存タスクとの重複チェック（タイトル類似度で判定）

### 4. Report

スキャン結果のサマリーを表示:
- 発見数（種別別）、作成タスク数、スキップ数（重複）
