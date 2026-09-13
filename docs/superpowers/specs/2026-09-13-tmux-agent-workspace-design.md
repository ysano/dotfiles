# tmux エージェント作業一覧

会話中に承認済みの UX 仕様を固定する。対象はローカル tmux 上の Claude Code と Codex CLI。

## 操作

- Prefix+w は Python curses の popup。現在の tmux session を対象とする。
- セッション表示はリポジトリ→親セッション→子エージェント。状態、cwd/worktree、表示先を列表示。
- Tab で worktree 表示に切替。エージェントのない作業領域も表示する。
- 上下で選択、左右で折りたたみ。Enter は既存 pane への移動または詳細。新規起動は別操作。
- 初期選択は元の pane。更新で選択ID、展開状態、既存の順序を維持する。
- 親行とリポジトリ行に隠れた承認/質問待ちの件数を表示。完了した子は初期折りたたみ。
- worktree の作成/安全な削除、シェルを開く、Claude/Codex の明示的起動を提供する。
- 旧ランチャーは明示操作で利用可能とし、既存の supervised/unsupervised/force remove 操作を失わない。
- ステータスバー件数は popup と同じ tmux session を対象とする。

## 自動シェル pane

既定OFF、tmux session 単位で切替。ON 時の一覧を baseline として既存 worktree を勝手に開かない。
新規作成の帰属が hook または本UIの作成操作から確認できるものだけ対象とする。帰属不明は一覧のみ。
子agentの一時worktreeは除外。既存 pane（Agent Teams含む）が作業先を表示していれば重複生成しない。
自動管理 pane は session 内最大1つ。split-window -d でフォーカスを維持する。
十分な幅/高さがない、既に1つある場合は未表示理由を一覧/詳細に残す。
手動で閉じた対象は再生成しない。削除済みworktreeのpaneを自動でkillしない。
既存paneのcwdを自動で切り替えない。Python標準ライブラリ、tmux、git以外に必須依存を増やさない。

## 状態

pane ID と session/agent ID を分離。親Stopで子の作業を完了扱いにしない。
Claudeの承認後ツールイベントで処理継続を反映。通知OFFでも質問検出を維持する。
hooks登録済み状態をタイトル推定が上書きしない。画面検出は根拠付き補助情報。
pane移動/終了に追従する。npm node wrapper と claude.exe をプロセス関係から検出する。
既存の音源・読み上げ設定を維持し、通知の二重発火を避ける。

## 結合インターフェース

状態保存は `@codex_state` / `@claude_state`、表示状態は `@codex_status` / `@claude_status`。
state = `{session_id, cwd, provider, actors}`。actor は ID(root または agent_id) をキーに
`{name,cwd,active,pending,turn_id}` を最低限含む（後方互換のため取得時デフォルト可）。

`registry.snapshot(session_id: str, origin_pane: str) -> dict`:

```
{session_id, origin_pane, repos: [{id,name,path}],
 agents: [{id,parent_id,repo_id,name,provider,status,cwd,pane_id,parent_pane,session_id,agent_id}],
 worktrees: [{id,path,repo,branch,temporary,panes,status,auto_reason}],
 summary: {busy,waiting}, auto: bool}
```

repo IDは共通Gitディレクトリの実体パス。子のpane_idは空、parent_paneは親のpane ID。
推測した親子リンクを作らない。hookに親を示す確実な情報がないTeamsセッションは独立行。

`worktrees` 公開API（失敗は例外、UIが人間向けに表示）:

```
list_worktrees(roots: list[str], panes: list[dict]) -> list[dict]
set_auto(session: str, enabled: bool, panes: list[dict]) -> None
poll_session(session: str, panes: list[dict]) -> None
observe_hook(event: dict, pane: str) -> None
create_worktree(root: str, name: str, base: str, session: str, origin: str) -> str
remove_worktree(path: str) -> None
open_worktree(path: str, session: str, origin: str, provider: str = 'shell') -> str
```

panes は `{pane_id,session_id,cwd,command,width,height,active}`。provider は shell/claude/codex。
observe_hook は重い git scan をしない。明示的な worktree 作成コマンドの完了から候補を記録する。
state保存のロック外で呼び出す。poll が候補の存在・帰属・一時作業領域除外を確認する。

## 完了条件

自動テスト、macOS実機操作、CI上Linuxテスト、PR/Copilotコメントへの返信・解消、master merge、
本機反映、origin/master各ファイルと実行設定の裏取り。WSL実機がなければ未検証と明記。

操作検証: 承認待ちの子を見つけて親へ移動、agentのないworktreeを開く、更新時に選択がずれない。
