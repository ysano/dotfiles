# tmux Agent Workspace Implementation Plan

> **For agentic workers:** Use superpowers:subagent-driven-development to execute the approved tasks with isolated file ownership and review.

**Goal:** 承認済みの popup ツリー、状態検出、worktree 操作を実装し本機へ反映する。
**Architecture:** pane-local event state → registry snapshot → curses popup。worktree監視はsession-local optionで管理し明示的な作成元を記録する。
**Tech Stack:** Python 3.9+標準ライブラリ、Bash、tmux、git、GitHub Actions。
**Spec:** `docs/superpowers/specs/2026-09-13-tmux-agent-workspace-design.md`

## Global Constraints

- 既定OFF、tmux session 単位で切替。
- 自動管理 pane は session 内最大1つ。split-window -d でフォーカスを維持する。
- Python標準ライブラリ、tmux、git以外に必須依存を増やさない。
- 日本語UI、既存設定保持、破壊的操作はUIで明示確認。実装の確認待ちは不要（会話で承認済み）。
- 作業は専用worktree。実装担当のファイルを分離し、commitはrootがまとめて行う。

### Task 1: Claude/Codex状態処理（担当status）

Files: `.tmux/agents/codex_status.py`, 新規 `claude_status.py`, 状態テスト、`.tmux/claude/{functions.sh,polling_monitor.sh,dialog_detector.sh,error_detector.sh,hooks/*}`。
Consumes: specのstate/actor形式。Produces: pane-local states、CLI poll、hooks設定。

- [x] 回帰テストを追加し失敗確認: 親Stop+動く子、通知OFF質問、claude.exe、登録済みBusyをタイトルIdleが上書きしない。
- [x] `python3 -m unittest discover -s .tmux/agents` の既存コードとの互換性を維持して実装。
- [x] `cwd/name/provider` をstateに保持。子の実行先を親cwdで潰さない。
- [x] hook終了後に `worktrees.observe_hook(event,pane)` を利用可能なら呼ぶ（ロック外）。
- [x] shell単体テスト/実tmuxテストを実行し変更ファイルと結果を報告。

### Task 2: worktree一覧・作成・監視（担当worktrees）

Files: 新規 `.tmux/agents/worktrees.py`, `test_worktrees.py` のみ。
Consumes/Produces: specのworktrees公開APIおよびpane形式。

- [x] 一時Git repository+tmuxサーバーのテストを作る。既定OFF、baseline、1pane制限、focus維持、閉じた後の再生成防止、dirty削除拒否をREDで確認。
- [x] `git worktree list --porcelain -z` を解析し実体パスで既存paneと照合。空白パスも扱う。
- [x] APIを実装。Git subprocessはargv配列、起動コマンドはshlex.join、tmuxの対象はIDで明示。
- [x] 自動表示はhookの帰属候補またはcreate_worktreeの成功からのみ。判定不能は一覧登録に留める。
- [x] `python3 -m unittest discover -s .tmux/agents -p test_worktrees.py` でGREEN確認。

### Task 3: popupツリーUI（担当UI）

Files: 新規 `.tmux/agents/dashboard.py`, `test_dashboard.py` のみ。
Consumes: registry.snapshotとworktrees公開API。Produces: CLI `dashboard.py --session '$1' --pane '%1'`。

- [x] rows構築/更新のテスト: 親子階層、待ち件数、選択IDと展開維持、終了子の初期折りたたみ。
- [x] cursesで上下左右/Tab/Enter/Esc、詳細、worktree作成削除、shell/claude/codex起動、auto切替、旧popup実行を実装。
- [x] paneなしEnterで勝手に起動しない。削除は確認を表示。端末幅が狭くてもcurses.errorで落ちない。
- [x] 非TTY検証用 `--dump`（同じrowsを出力）を追加。
- [x] `python3 -m unittest discover -s .tmux/agents -p test_dashboard.py` で確認。

### Task 4: Registryと結合（担当root）

Files: 新規 `.tmux/agents/registry.py`, `test_registry.py`, `.tmux/claude-worktree.conf`, `.tmux/{status.conf,plugin-config/resurrect.conf}`, `.tmux/agents/README.md`。

- [x] pane-local statesからsnapshotを作るテストを追加。actor行をidで安定化、repo共通ディレクトリでgroup。
- [x] snapshot API実装、registry pollでprovider pollingとworktree pollingを統合。session件数をsession optionへ保存。
- [x] Prefix+wをdashboardへ接続し旧ランチャーを明示操作で維持。
- [x] docs更新、CIの全テストとconflicts/cross-platformチェック。
- [x] 一時tmux内でpopup操作・更新・worktreeを開く・autopane・hook実機発火を確認。

### Task 5: Review・取り込み

- [x] 各担当差分を仕様/品質レビュー、指摘は同種箇所を洗い出し修正。
- [ ] 全体レビュー→テスト→commit→pushし全ファイルをoriginから照合。
- [ ] PRを作成しCopilotレビューをpoll。指摘を解釈してinline返信、修正/resolve。
- [ ] CI成功後masterにmerge。本機のcheckoutを更新しtmux設定/hooksを反映。
- [ ] origin/masterと実行設定を照合し、検証範囲と残る製品仕様上の制限を報告。
