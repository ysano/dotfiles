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
