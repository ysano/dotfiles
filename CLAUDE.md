## 基本ルール

- 不具合修正前にテストコードを書き、失敗を確認してから実装を変更する
- 日本語で受け答えする

## リポジトリ概要

クロスプラットフォーム（Windows/macOS/Linux・WSL）対応の統合開発環境dotfiles。Emacs中心、AI統合ワークフロー、日本語環境、Zsh + Zinit構成。

## 開発コマンド

```bash
./link.sh                # シンボリックリンク作成・デプロイ
./test_zsh_config.zsh    # Zsh設定の検証テスト
```

## 設計原則

- **グレースフル劣化**: モダンツール未導入時は自動フォールバック
- **遅延読み込み**: Zinit / use-package で段階的ロード（起動時間への影響を常に考慮）
- **クロスプラットフォーム**: OS固有機能は条件分岐で対応
- **環境非依存パス**: テスト・スクリプトに絶対パスをハードコードしない。CI（GitHub Actions）でも動作すること

## Claude Code モジュール

| ディレクトリ | 用途 | 判断基準 |
|---|---|---|
| `.claude/` | dotfiles固有（agents/commands/skills） | Emacs/Zsh/tmux/Keyboard設定に直接関連 |
| `claude-home/` | 汎用（agents, hooks, skills） | プロジェクト非依存。Commands は claude-plugins に移行済み |

dotfiles設定変更 → `Task: dotfiles-engineer`、検証 → `Task: dotfiles-validator`

## ドキュメント索引

| パス | 内容 |
|---|---|
| `.claude/CLAUDE.md` | dotfiles固有モジュール詳細 |
| `claude-home/CLAUDE.md` | 汎用モジュール詳細 |
| `docs/architecture.md` | アーキテクチャ概要 |
| `docs/commands-reference.md` | Emacs/Zsh/tmux/Git Worktree コマンド詳細 |
| `.claude/skills/*/SKILL.md` | 設定アーキテクチャ知識（emacs/zsh/tmux/keyboard） |
