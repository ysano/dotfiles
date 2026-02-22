# dotfiles ワークフロー

## 基本フロー

```
1. Skill で現状把握     → Skill: emacs-config / zsh-config / tmux-config / keyboard-config
2. dotfiles-engineer で変更 → Task: dotfiles-engineer + プロンプト
3. 検証（任意）          → Task: dotfiles-validator / 各 Skill の scripts/
4. コミット             → /dev:commit
```

## 例: エイリアス追加

```bash
Skill: zsh-config              # Conventions 確認（has_command ガード等）
Task: dotfiles-engineer        # "aliases.zsh に exa エイリアスを追加"
Bash: .claude/skills/zsh-config/scripts/validate.sh aliases.zsh
/dev:commit
```

## 例: tmux キーバインド追加

```bash
Skill: tmux-config             # emacsCX テーブルの既存バインド確認
Task: dotfiles-engineer        # "Prefix x b で choose-tree を開く"
Bash: .claude/skills/tmux-config/scripts/check_conflicts.sh
/dev:commit
```

## ポイント

- **Skill 先行**: 既存パターンを確認してから実装（規約違反を防ぐ）
- **小さな変更**: 1機能ずつ追加 → 即テスト → コミット
- **クロスプラットフォーム変更は検証必須**: `cross_platform_check.sh` を実行
- **git add . 禁止**: 明示的なファイル指定のみ
