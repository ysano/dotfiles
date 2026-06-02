# tmux × Claude worktree ランチャー 設計

- 日付: 2026-06-02
- ステータス: ドラフト（spec レビュー待ち）
- 入口: tmux キーバインド（`prefix + w`）主体 / Approach A（display-popup ランチャー）
- 方針: Option 2 改（gwt の**配置規約のみ共有**し、ランタイム依存はしない）

## 1. 目的

git worktree を主軸に、複数の機能/ブランチを **tmux 上で並列に** Claude Code で進める。
監視あり（対話 pane）と監視なし（headless）の両モードを、tmux キーバインド一発の
popup から起動・切替・削除できるようにする。

非目標（YAGNI）:
- ステータスバーへの稼働エージェント数表示（将来拡張）
- `claude agents`（バックグラウンドエージェント・ビュー）の本格統合
- 公式 `claude -w/--tmux` への全面移行

## 2. 背景と実機検証で得た事実

公式 `claude -w/--tmux` を実機検証し、本リポジトリの運用と相性の悪い2点を確認した。

| 観点 | 公式 `claude -w/--tmux` の実挙動 | 本設計の対応 |
|---|---|---|
| worktree のベース | **`origin/master` 基点**（ローカル未 push を取りこぼす） | 既定 **HEAD**（上書き可） |
| worktree 配置 | `<repo>/.claude/worktrees/<name>`（main で `?? .claude/worktrees/` と**未追跡表示＝汚染**） | **外部 `../worktrees/<repo>-<name>`**（汚染なし、gwt と共通規約） |
| tmux セッション | `--tmux=classic` は**別セッション**を新規作成 | 既定は**現セッションに `new-window`**（重並列向けに専用セッションをオプション化） |

また Claude Voice 検出（`.tmux/claude/functions.sh` の `detect_claude_panes`、
`polling_monitor.sh`）は `tmux list-panes -a` で **全セッション横断**に走査する。
したがって session 配置は Voice 統合の制約ではなく、純粋に UX 上の選択である。

## 3. 設計判断

| # | 判断 | 根拠 |
|---|---|---|
| D1 | worktree のベースは**既定 HEAD・popup で上書き可**。曖昧化を避けるため main worktree のブランチ tip もしくは明示 ref を基点とする | 公式の origin/master 基点でローカル未 push を取りこぼす問題を回避 |
| D2 | 配置は**外部 `../worktrees/<repo>-<name>`**（`.claude/worktrees/` は使わない） | `.claude/worktrees/` は main リポジトリを汚染（実機確認）。gwt も外部配置で一致 |
| D3 | 監視あり = **現セッションの `new-window`**。重並列向けに専用セッションを設定で選択可 | Voice は全セッション横断のため session 縛りは不要。既定は摩擦最小の現セッション |
| D4 | 監視なし = `new-window -d` + `claude -p`。`-p` は**完走して終了**する。引き継ぎは `claude -c`（cwd 継続）。許可は既定 `--allowedTools` 厳選、`--dangerously-skip-permissions` は明示時のみ | `-p` は live プロセスではない。acceptEdits だけでは Bash 等で停止し無人化しない |
| D5 | 同一性は **tmux window option `@cc_worktree`=worktree パス**で担保。状態ファイルは作らない。`--session-id` は任意 | window option はほぼ無料のレジストリ。cwd 単独だと同 cwd 複数 session で `-c` が曖昧 |
| D6 | キーバインドは**新規 `.tmux/claude-worktree.conf`**（`.tmux.conf` から guarded source）。自動生成の `claude.conf` には触れない | 生成ファイル破壊回避・関心分離 |
| D7 | **lifecycle（削除）を MVP に含める**。worktree + branch + 関連 window を片付ける | 作るだけで消せないと運用破綻 |
| D8 | gwt は**配置規約のみ共有**し、ランタイム依存しない（素の `git worktree` で実装） | gwt は zsh 関数で tmux popup から呼ぶのは脆い。共有は規約に限定し自己完結・テスト可能に |

## 4. コンポーネント

### 4.1 `.tmux/claude/worktree_launch.sh`（コア）
- 純粋ヘルパ（副作用なし・単体テスト対象）:
  - `validate_name <name>` — 名前検証・sanitize（gwt の safe_name 相当）
  - `worktree_path <name>` — `../worktrees/<repo>-<name>` を構築
  - `resolve_base [ref]` — 既定 HEAD、引数で上書き
  - `build_claude_cmd <mode> <name> [task] [perm]` — 起動コマンド文字列を組立
- 副作用関数（`--dry-run` でコマンド出力のみ）:
  - worktree 作成: `git worktree add -b worktree-<name> <path> <base>`
  - 起動（監視あり）: `tmux new-window -c <path> -n <name> "<claude cmd>"` + `set-window-option @cc_worktree <path>`
  - 起動（監視なし）: `tmux new-window -d -c <path> -n <name> "<claude -p cmd>"` + `@cc_worktree`
  - 切替/前面化: `@cc_worktree` 一致 window があれば `select-window`、無ければ `new-window "claude -c"`
  - 削除: `git worktree remove <path>` + `git branch -D worktree-<name>` + 関連 window `kill-window`
- フォールバック: fzf/uuidgen 不在時は tmux `command-prompt` / `read` に劣化

### 4.2 `.tmux/claude-worktree.conf`（front-end）
```tmux
# Claude worktree ランチャー（prefix + w）
bind-key w display-popup -E -w 80% -h 60% "~/.tmux/claude/worktree_launch.sh popup"
```

### 4.3 `.tmux.conf`（1行追記）
`keybindings.conf` の後あたりに guarded source を追加:
```tmux
if-shell '[ -f ~/.tmux/claude-worktree.conf ]' \
  'source-file ~/.tmux/claude-worktree.conf'
```

### 4.4 配布
`.tmux/` はディレクトリ丸ごと symlink 済（link.sh の `dirs` に `.tmux`）。
新スクリプト・新 conf は自動配布される。`.tmux.conf` は個別 symlink 済でソースを編集。
link.sh の変更は不要。

## 5. データフロー

```
prefix + w
  → display-popup -E
    → worktree_launch.sh popup
      fzf[ 既存worktree一覧 | ＋新規作成 ]
       ├ 既存選択
       │   → [ 開く/前面化 | 削除 ]
       │       開く: @cc_worktree 一致 window → select-window
       │             無し → new-window "claude -c"（cwd 継続）
       │       削除: git worktree remove + branch -D + 関連 window kill
       └ ＋新規
           → name 入力 → base(既定HEAD,上書き可)
           → git worktree add -b worktree-<name> ../worktrees/<repo>-<name> <base>
           → モード選択
              監視あり: new-window(現セッション) "claude -n <name>" + @cc_worktree
              監視なし: task 入力 → new-window -d "claude -p '<task>' --allowedTools …" + @cc_worktree
  → 起動された claude pane は Voice(list-panes -a 全セッション走査)が自動監視
```

## 6. エラー処理 / グレースフル劣化

- git リポジトリ外 / tmux 外（`$TMUX` 未設定）/ claude 不在 → メッセージ表示して中断
- 名前衝突（worktree 既存）→「既存を開く」へ誘導
- 不正名（空白/スラッシュ等）→ sanitize して継続、不能なら中断
- base ref 不正 → 中断
- fzf / uuidgen 不在 → tmux `command-prompt` / `read` にフォールバック
- popup 途中閉じ → worktree 作成前なら副作用なし（作成が commit point）
- 監視なしの許可戦略: 既定 `--allowedTools` 厳選。`--dangerously-skip-permissions` は
  明示フラグ＋注意書きを表示した場合のみ

## 7. テスト戦略（TDD）

CLAUDE.md「不具合修正前にテストコードを書き、失敗を確認してから実装」に従う。

- **単体テスト（先に失敗を確認）**: 純粋ヘルパ
  - `validate_name`: 正常/空白/スラッシュ/空文字
  - `worktree_path`: 外部 `../worktrees/<repo>-<name>` を返すこと（絶対パスのハードコード禁止）
  - `resolve_base`: 引数なし→HEAD、引数あり→その ref
  - `build_claude_cmd`: 監視あり/なしで期待文字列（`-n` vs `-p … --allowedTools`）
- **`--dry-run`**: git/tmux/claude を実行せずコマンド文字列を出力し検証（環境非依存・CI 動作）
- **構文/衝突**: `.tmux/ci.sh`（.conf 構文）と `check_conflicts.sh`（`prefix + w` 衝突なし）を通す
- テストは `.tmux/claude/` 配下に配置し、絶対パスをハードコードしない（GitHub Actions で動作）

## 8. 未確定事項

- `claude -p` の acceptEdits 下での Bash 停止範囲は未実測（設計は「止まる前提」で保守的に組む）
- 公式 `-w` が `.claude/worktrees/` の汚染をどう回避しているか（本設計は外部配置のため影響なし）
- 専用セッション option（D3 の重並列向け）の既定 ON/OFF と命名規約は実装時に確定
