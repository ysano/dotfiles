# tmux のエージェント・worktree連携

Codex CLI の hooks を受け取り、window に `Codex:⚡`（作業中）、
`Codex:⌛`（承認待ち）、`Codex:✅`（待機中）、`Codex:?`（hook 未登録）を表示する。
Claude Code のアイコンと併存する。ステータスバーには現在のtmuxセッションの
作業中・対応待ち件数を表示する。paneの自動生成は既定OFF。

## ツリーとworktree操作

`Prefix+w`（既定は `Ctrl+z` → `w`）で現在のtmuxセッションのツリーを開く。
リポジトリ → 親エージェント → 子エージェントの順に、状態・作業ディレクトリ・表示先を確認できる。
子が親と同じpaneで動く場合も別の行になる。独立paneを持つTeamsメンバーは、
明示的な親子情報がなければ独立したセッションとして表示する。
子の階層は、hookの会話IDで確認したセッションへの所属を表す。
ネストした子同士の呼び出し元IDが得られない場合は同じ階層に並べ、呼び出し関係を推測しない。

| キー | 操作 |
|---|---|
| ↑↓ / ←→ | 選択 / 展開・折りたたみ |
| Tab | エージェントツリーとworktree一覧の切替 |
| Enter | 既存paneへ移動、paneがなければ詳細を表示 |
| p | 子エージェントの親paneへ移動 |
| s / c / x | 選択先でシェル / Claude Code / Codexを明示的に開く |
| n / d | worktree作成 / 削除（確認あり） |
| a | このtmuxセッションの自動pane生成を切替 |
| l | 従来のworktreeランチャー |
| Esc | 閉じる |

更新しても選択と展開を維持する。折りたたんだ子の対応待ち件数は親に表示する。
Enterでエージェントを自動起動することはない。

自動生成をONにすると、それ以降にこのUIまたは帰属を確認できる親のhook経由で
作成されたworktreeにシェルpaneを開く。ON時点の既存worktree、作成元不明、
子エージェントの一時worktreeは自動生成対象にしない。
hookからの作成元判定は、成功を確認できる単独の `git worktree add` に限定する。
複合シェルコマンドやバックグラウンド実行は一覧のみの扱いになる。
既存paneがあれば重複生成せず、現在のフォーカスを維持する。
自動管理paneはセッション内で最大1つ。手動で閉じたworktreeは自動で開き直さない。
分割後に各paneが80列×16行を確保できない場合は一覧に理由を表示する。

Codex CLI 0.154.0の実測hooksには実行コマンド・結果が含まれないため、
Codexがツールで作成したworktreeの帰属は確認できず、自動生成せず一覧に表示する。
Codex利用中に自動生成を試す場合は、このUIの `n` で作成する。
新UIの削除は未コミット変更・利用中pane・稼働中の子エージェントを確認し、
強制削除やブランチ削除は行わない。従来の操作が必要な場合は `l` で旧ランチャーへ移る。

## セットアップ

必要: tmux、Python 3.9 以上、hooks 対応の Codex CLI。
macOS / Codex CLI 0.154.0 で確認。Linux/WSL は同じプロセス確認方式を使用する。
Claudeを含む状態監視とツリーにはPythonが必須。未導入時はステータスバーに
`AI: Python未導入` と表示し、監視を実行しない。`Prefix+w` では従来ランチャーを利用できる。

```sh
python3 ~/.tmux/agents/setup_codex_hooks.py
bash ~/.tmux/claude/hooks/setup-hooks.sh
tmux source-file ~/.tmux/claude-worktree.conf
tmux source-file ~/.tmux/status.conf
tmux source-file ~/.tmux/plugin-config/resurrect.conf
```

次回の Codex 起動時、追加された hooks の内容を確認して信頼する。
インストーラーは既存の `hooks.json` をマージし、変更時のみバックアップを作る。
`config.toml` の権限・モデル・既存の通知設定には触れない。
`CODEX_HOME` を設定している場合は、そのディレクトリの `hooks.json` が対象。
`--output PATH` で別ファイルへの生成も可能。

通知音は既存の `@claude_voice_sound_enabled` と音源・音量設定を共有する。
起動時の Idle や中断では完了音を鳴らさず、状態遷移ごとに通知する。
既存の Codex `notify` 設定は保持するため、両方を有効にすると通知が重なる場合がある。
Codex側は状態表示と通知音を対象とし、読み上げ・画面からの質問／エラー検出は含まない。
Claude側の既存の通知・読み上げ設定は維持する。

## 状態の扱い

- `TMUX` でサーバー、`TMUX_PANE` で pane を特定する。tmux 外の hooks は何もしない。
- pane option `@codex_state` に session ID と各 actor の turn ID・状態を保存する。
- Claudeは `@claude_state` を正とし、画面からの質問・エラーは別の補助情報として保持する。
  登録済みhookのBusyをタイトルだけでIdleに変更しない。
- 親の `Stop` / `Interrupt` で子の状態を消さない。子が作業中なら Busy を維持する。
- 子は `agent_id` で区別し、子の `SubagentStop` はその子だけを待機状態にする。
- 古いターンの完了・終了イベントで、新しい作業を上書きしない。
- 別の Stop hook が継続を要求した場合、同じターンの再開始／ツール実行で Busy に戻す。
  別 hook の判断は並行実行中に直接観測できないため、Stop 直後に一時的な Idle 表示や
  完了音が出る場合がある。
- 同じサーバーの hook と polling をファイルロックで直列化する。
- polling は `ps` のプロセス親子関係から npm の node ラッパーにも対応する。
  タイトルや一定時間の経過から完了を推定しない。
- CLI が終了した場合は状態を消し、pane 移動・削除後は各 window のアイコンを再集約する。
- 権限待ちは actor とツール名で追跡する。同じ actor が同名ツールの承認を並行して
  複数要求する場合は区別できないため、最初の実行結果で待機表示が解除され得る。

診断・無効化:

```sh
python3 ~/.tmux/agents/codex_status.py status
python3 ~/.tmux/agents/dashboard.py --dump
tmux set-option -g @codex_enabled false
```

再有効化は `@codex_enabled true`。hook が再度届くまでは `?` を表示する。
無効化は表示と通知を止める。hooks 自体を削除する場合は `hooks.json` の
`statusMessage: "tmux Codex status"` のエントリーだけを削除する。

## 検証

```sh
python3 -m unittest discover -s .tmux/agents -p 'test_*.py'
bash .tmux/ci.sh
```

API を呼ばない状態遷移テストと、一時 tmux サーバーを使う結合テスト。
tmux のない環境では結合テストを skip する。CI は tmux をインストールして実行する。

仕様: [Codex hooks](https://learn.chatgpt.com/docs/hooks)

### 2026-09-13 の実機確認

専用 tmux サーバーと一時 `CODEX_HOME` で観測用 hooks と本実装を同時に実行した。
通常ターンの開始・終了、子の `SubagentStart` / `SubagentStop`、Esc による
`Interrupt`、npm ラッパー（`pane_current_command=node`）での polling 継続を確認。
親子は同じ pane / session ID を使い、子は別 agent ID / turn ID を持っていた。
承認待ちの遷移・古いイベントの無視・pane 移動は自動テストで検証している。
外部コマンドをバックグラウンドに残した場合、その存続は Busy の判定対象に含めない。
