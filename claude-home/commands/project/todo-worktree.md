---
description: "Structured workflow to transform vague todos into implemented features using git worktrees and VS Code handoff."
---

## Workflow

**CRITICAL**
- フェーズ順序を厳守: INIT → SELECT → REFINE → IMPLEMENT → COMMIT
- 各 STOP でユーザー確認を取得
- REFINE の STOP はユーザーが承認するまで繰り返す
- コミットメッセージに自分自身への言及を含めない
- 予期しないエラーはユーザーに相談
- IMPLEMENT で追加/削除/変更したファイルのコミットを忘れない

### INIT

1. タスク再開チェック: カレントディレクトリに `task.md` が存在する場合:
   - task.md と `todos/project-description.md` を並列で読み、PID を更新
   - Status に応じたフェーズへ（Refining→REFINE, InProgress→IMPLEMENT, AwaitingCommit→COMMIT, Done→終了）

2. `.gitignore` に `/todos/worktrees/` を追加: `rg -q "/todos/worktrees/" .gitignore || echo -e "\n/todos/worktrees/" >> .gitignore`

3. `todos/project-description.md` を読む
   - 存在しない場合:
     - STOP → "エディタコマンドを教えてください（例: 'code', 'cursor'）"
     - 並列 Task エージェントでコードベースを分析
     - テンプレートで提案（Features / Tech Stack / Structure / Architecture / Commands / Testing / Editor）
     - STOP → "修正はありますか？ (y/n)"
     - 承認後書き出し

4. 孤立タスクをチェック:
   ```bash
   mkdir -p todos/worktrees todos/done && orphaned_count=0 && for d in todos/worktrees/*/task.md; do [ -f "$d" ] || continue; pid=$(grep "^**Agent PID:" "$d" | cut -d' ' -f3); [ -n "$pid" ] && ps -p "$pid" >/dev/null 2>&1 && continue; orphaned_count=$((orphaned_count + 1)); task_name=$(basename $(dirname "$d")); task_title=$(head -1 "$d" | sed 's/^# //'); echo "$orphaned_count. $task_name: $task_title"; done
   ```
   - 一覧表示 → STOP → "再開しますか？ (番号/ignore)"
   - 再開: エディタで worktree を開く → STOP → "worktree で `claude "/todo"` を実行してください"
   - 無視: SELECT へ

### SELECT

1. `todos/todos.md` を読み、番号付き一覧を表示
2. STOP → "どの todo に取り組みますか？ (番号)"
3. 選択した todo を削除してコミット: `git commit -am "Remove todo: [task-title]"`
4. Worktree 作成: `git worktree add -b [slug] todos/worktrees/$(date +%Y-%m-%d-%H-%M-%S)-[slug]/ HEAD`
5. CWD を worktree に変更
6. `task.md` を初期化（Status: Refining, Agent PID, Original Todo, Description, Implementation Plan, Notes）
7. コミット＆プッシュ: `git add . && git commit -m "[task-title]: Initialization" && git push -u origin [slug]`

### REFINE

1. 並列 Task エージェントでコードベースを調査
2. 分析結果を `analysis.md` に記録
3. 説明をドラフト → STOP → "この説明でよいですか？ (y/n)"
4. 実装計画をドラフト → STOP → "この実装計画でよいですか？ (y/n)"
5. task.md を更新、`**Status**: InProgress`
6. コミット: `git add -A && git commit -m "[task-title]: Refined plan"`
7. エディタで worktree を開く → STOP → "worktree で `claude "/todo"` を実行して実装を開始してください"

### IMPLEMENT

1. 実装計画のチェックボックスを順に実行:
   - 未予見の作業: 新チェックボックスを提案 → STOP → task.md に追加
   - 各チェックボックス: コード変更 → サマリー → STOP → "承認しますか？" → 完了マーク → `git add -A && git commit -m "[checkbox text]"`
2. 全完了後、プロジェクト検証（lint/test/build）
   - 失敗: エラー報告 → 修正チェックボックス提案 → STOP → ステップ1に戻る
3. ユーザーテスト → STOP → "全テストパスしましたか？ (y/n)"
4. project-description.md の更新が必要か確認 → STOP
5. `**Status**: AwaitingCommit`、コミット: `git add -A && git commit -m "Complete implementation"`

### COMMIT

1. 完了サマリーを表示
2. STOP → "PR を作成しますか？ (y/n)"
3. `**Status**: Done`
4. タスクを done に移動:
   ```bash
   git mv task.md todos/done/[timestamp]-[slug].md
   git mv analysis.md todos/done/[timestamp]-[slug]-analysis.md
   ```
5. コミット: `git add -A && git commit -m "Complete"`
6. リモートにプッシュし、GitHub CLI で PR を作成
7. STOP → "worktree を削除しますか？ (y/n)"
   - Yes: `git -C "$(git rev-parse --show-toplevel)" worktree remove todos/worktrees/[timestamp]-[slug]`
   - 注意: worktree 内で Claude を起動した場合、削除後に CWD が無効になる
