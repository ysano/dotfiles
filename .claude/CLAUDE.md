**dotfiles固有の開発・管理モジュール**

> 汎用ツールは `claude-home/` を参照。判断基準: Emacs/Zsh/tmux/Keyboard設定に直接関連 → `.claude/`

## 構成

- **agents/**: `dotfiles-engineer`（設定変更）、`dotfiles-validator`（検証・読み取り専用）
- **commands/dev/**: `/dev:commit`（品質チェック統合コミット）、`/dev:pull-request`（PR作成）
- **skills/**: `emacs-config`、`zsh-config`、`tmux-config`、`keyboard-config`

## 基本ワークフロー

1. Skill で現状把握（例: `Skill: emacs-config`）
2. `Task: dotfiles-engineer` で設定変更
3. `Task: dotfiles-validator` で検証（任意）
4. `/dev:commit` でコミット

## 関連ドキュメント

- `docs/dotfiles-workflows.md` — ワークフロー詳細
