---
description: "Structured workflow to transform vague todos into implemented features. Works on current branch with single commit at completion."
---

## Workflow

**CRITICAL**
- フェーズ順序を厳守: INIT → SELECT → REFINE → IMPLEMENT → COMMIT
- 各 STOP でユーザー確認を取得
- REFINE の STOP はユーザーが承認するまで繰り返す
- コミットメッセージに自分自身への言及を含めない
- 予期しないエラーはユーザーに相談
- IMPLEMENT で追加/削除/変更したファイルのステージを忘れない

### INIT

1. `todos/project-description.md` を読む
   - 存在しない場合:
     - 並列 Task エージェントでコードベースを分析（目的、技術スタック、構造、コマンド、テスト設定）
     - 以下のテンプレートで提案:
       ```markdown
       # Project: [Name]
       [Concise description]
       ## Features / ## Tech Stack / ## Structure / ## Architecture / ## Commands / ## Testing
       ```
     - STOP → "修正はありますか？ (y/n)"
     - 承認後 `todos/project-description.md` に書き出し

2. 孤立タスクをチェック:
   ```bash
   mkdir -p todos/work todos/done && orphaned_count=0 && for d in todos/work/*/task.md; do [ -f "$d" ] || continue; pid=$(grep "^**Agent PID:" "$d" | cut -d' ' -f3); [ -n "$pid" ] && ps -p "$pid" >/dev/null 2>&1 && continue; orphaned_count=$((orphaned_count + 1)); task_name=$(basename $(dirname "$d")); task_title=$(head -1 "$d" | sed 's/^# //'); echo "$orphaned_count. $task_name: $task_title"; done
   ```
   - 孤立タスクがあれば一覧表示 → STOP → "再開しますか？ (番号/ignore)"
   - 再開: task.md と analysis.md を読み、PID を更新、Status に応じたフェーズへ
   - 無視: SELECT へ

### SELECT

1. `todos/todos.md` を読み、番号付き一覧を表示
2. STOP → "どの todo に取り組みますか？ (番号)"
3. タスクフォルダ作成: `TASK_DIR="todos/work/$(date +%Y%m%d-%H%M%S)-[slug]"` → 以降このパスを使用
4. `${TASK_DIR}/task.md` を初期化:
   ```markdown
   # [Task Title]
   **Status:** Refining
   **Agent PID:** [Bash(echo $PPID)]
   ## Original Todo / ## Description / ## Implementation Plan / ## Notes
   ```
5. 選択した todo を `todos/todos.md` から削除

### REFINE

1. 並列 Task エージェントでコードベースを調査（変更箇所、既存パターン、関連コード）
2. 分析結果を `${TASK_DIR}/analysis.md` に記録
3. 説明をドラフト → STOP → "この説明でよいですか？ (y/n)"
4. 実装計画をドラフト → STOP → "この実装計画でよいですか？ (y/n)"
5. `${TASK_DIR}/task.md` を更新、`**Status**: InProgress` に設定

### IMPLEMENT

1. 実装計画のチェックボックスを順に実行:
   - 未予見の作業が必要な場合: 新しいチェックボックスを提案 → STOP → "追加しますか？ (y/n)"
   - 各チェックボックスで: コード変更 → 変更サマリー → STOP → "承認しますか？ (y/n)" → 完了マーク → `git add -A`
2. 全チェックボックス完了後、プロジェクト検証（lint/test/build）実行
   - 失敗時: エラー報告 → 修正チェックボックスを提案 → STOP → ステップ1に戻る
3. ユーザーテスト手順を提示 → STOP → "全テストパスしましたか？ (y/n)"
   - No: 失敗内容を確認しステップ1に戻る
4. 実装で構造/機能/コマンドが変わった場合、`todos/project-description.md` の更新を提案 → STOP
5. `**Status**: AwaitingCommit` に設定

### COMMIT

1. 完了サマリーを表示
2. STOP → "コミットしますか？ (y/n)"
3. `**Status**: Done` に設定
4. タスクを done に移動:
   ```bash
   TASK_NAME=$(basename "${TASK_DIR}")
   mv "${TASK_DIR}/task.md" "todos/done/${TASK_NAME}.md"
   mv "${TASK_DIR}/analysis.md" "todos/done/${TASK_NAME}-analysis.md"
   rmdir "${TASK_DIR}"
   ```
5. `git add -A`
6. コミットメッセージを表示 → STOP → "このメッセージでよいですか？ (y/n)"
7. **1つのコミット**で全変更（コード + タスク管理）をコミット: `git commit -m "[task-title]: [summary]"`
8. STOP → "完了！次の todo に取り組みますか？ (y/n)"
