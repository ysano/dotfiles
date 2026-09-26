## 基本ルール

- 不具合修正前にテストコードを書き、失敗を確認してから実装を変更する
- 日本語で受け答えする

## リポジトリ概要

クロスプラットフォーム（Windows/macOS/Linux・WSL）対応の統合開発環境dotfiles。Emacs中心、AI統合ワークフロー、日本語環境、Zsh + Zinit構成。

> Brewfile・マシン構築の運用ナレッジは [ysano/machine-management](https://github.com/ysano/machine-management) で管理。

## 開発コマンド

```bash
./link.sh                # シンボリックリンク作成・デプロイ（$HOME を変更する）
./test_aliases_claude.zsh && ./test_git_worktree.zsh  # Zsh エイリアス・gwt の検証
(cd .tmux && bash ci.sh) # tmux 設定・スクリプトの検証（CI と同じ入口）
make test                # 上記と bin のテストをまとめて実行
```

各テストの CI 上の入口は `.github/workflows/` を参照。

## 完了条件・作業上の注意

- **Done**: 変更したツールの構文チェック（`zsh -n` / tmux の設定読込 / Emacs の batch load）が通る ＋ 変更領域のテストが通る ＋ CI（GitHub Actions）が green。
- **`$HOME` への作用はリポジトリ外変更として扱う**: `./link.sh` の実行と `~` 配下への書き込みは事前に確認する。`~/.zshrc`・`~/.tmux`・`~/.tmux.conf`・`~/.emacs.d` は master の実体への symlink のため、master の作業ツリーを書き換えると稼働中の環境に即時反映される。実装は別 worktree で行う。
- **ツール横断の監査**: emacs / zsh / tmux / keyboard にまたがる検証は、ツールごとに `dotfiles-validator` に分けて渡し、根拠を確認してから「ツール / 影響有無 / 根拠」の表に統合する。

## 設計原則

- **グレースフル劣化**: モダンツール未導入時は自動フォールバック
- **遅延読み込み**: Zinit / use-package で段階的ロード（起動時間への影響を常に考慮）
- **クロスプラットフォーム**: OS固有機能は条件分岐で対応
- **環境非依存パス**: テスト・スクリプトに絶対パスをハードコードしない。CI（GitHub Actions）でも動作すること

## 構成とデプロイ

| パス | 役割 | `link.sh` での配備 |
|---|---|---|
| `.zshrc` `.zprofile` `.zsh/` | Zsh（OS 別 `aliases_{darwin,linux,msys,freebsd}.zsh`、Zinit） | `~/` に symlink |
| `.emacs.d/` | Emacs（`init.el` ＋ `inits/`）。固有ルールは `.emacs.d/CLAUDE.md` | `~/.emacs.d` |
| `.tmux.conf` `.tmux/` | tmux（モジュラー conf、Claude 連携 `claude/` `agents/`、検証入口 `ci.sh`） | `~/.tmux*` |
| `.skhdrc` `.yabairc` `.Xresources` `.xinitrc` `.aspell.conf` | OS 固有の単体設定 | `~/` に symlink |
| `.config/{bat,git,ripgrep}` | XDG 設定 | `$XDG_CONFIG_HOME/` |
| `.claude/statusline-command.sh` | ホスト固有の統合スクリプト | `~/.claude/` に個別 symlink |
| `.claude/{agents,commands,skills}` | dotfiles 固有の Claude Code 資産（このリポジトリで作業するときに読まれる） | 配備しない |
| `bin/` | 個人 CLI（`emoji-id`、`claude-doctor`） | 配備しない（`.zshrc` が `~/dotfiles/bin` を PATH に追加） |
| `karabiner/` `wsl/` `mayu/` `keyboard-maestro/` | OS 固有の設定 | 手動（`karabiner` と `wsl` は `README.org`、`mayu` と `keyboard-maestro` は `docs/managed-tools.md`） |

- **`link.sh` の挙動**: 配備対象は先頭の配列（`files` / `dirs` / `config_dirs` / `claude_files`）で宣言する。既存の実ファイルは `*.orig` に退避し、既存の symlink は張り直す。msys/cygwin では `cmd //c mklink` を使う（`ln -s` だと実体コピーになるため）。リンク元は `$HOME/dotfiles` 固定なので、clone 先は `~/dotfiles` を前提とする。
- **配備対象の追加**: 配列に名前を足すだけで済む。`config_dirs` / `claude_files` は、リンク元が存在しない場合は黙ってスキップされるので、追加後はリンクが張られたことを確認する。
- **CI**: workflow ごとに `paths` フィルタで対象を絞っている。`ci.yml` は `.tmux/**` の変更で `.tmux/ci.sh` を実行し、`shell-tests.yml` は `bin/**` と `test_*.sh` の変更で `test_emoji_id.sh` を実行する。ルート直下の Zsh テスト（`test_*.zsh`）と `test_resurrect.sh` は CI に乗っていないため、手元で実行する。

## Claude Code モジュール

| ディレクトリ | 用途 |
|---|---|
| `.claude/` | dotfiles固有（agents/commands/skills）: Emacs/Zsh/tmux/Keyboard設定 |

汎用ツール（agents, hooks, skills）は [claude-plugins](https://github.com/ysano/claude-plugins) リポジトリで管理。`/plugin install <name>@ysano-plugins` でインストール。

dotfiles設定変更 → `Task: dotfiles-engineer`、検証 → `Task: dotfiles-validator`

## ドキュメント索引

| パス | 内容 |
|---|---|
| `.claude/CLAUDE.md` | dotfiles固有モジュール詳細 |
| `.claude/skills/*/SKILL.md` | 設定アーキテクチャ知識（emacs/zsh/tmux/keyboard） |
| `docs/dotfiles-workflows.md` | ワークフロー |
| `docs/commands-reference.md` | Git Worktree/tmux コマンド |
| `docs/managed-tools.md` | 管理ツール一覧・デプロイ方法 |
| `docs/tmux-claude-voice-troubleshooting.md` | tmux Claude Voice の症状別調査・復旧手順 |
