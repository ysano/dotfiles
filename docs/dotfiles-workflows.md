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

## 例: worktree で並列作業 (prefix + w)

複数の変更を同時に進めたいとき、tmux の Claude Worktree ランチャーで worktree を立てて
Claude を並列起動する。本流（master の作業ツリー）を汚さず、機能ごとに隔離して進められる。

```text
prefix + w                     # popup を開く
  → 名前を入力 (例 fix-zsh-path)  # 現在の pane の repo の HEAD を基点に worktree 作成
  → base ref は空 Enter (= HEAD)
  → 監視あり (supervised)        # 現 window を pane 分割して Claude 起動
# その pane で Skill 先行 → dotfiles-engineer → 検証 → /dev:commit（基本フローと同じ）
# 別の変更は再度 prefix + w で別 worktree を立てて並行で進める
```

- 作成時、`*.local.md`（git 管理外のローカルメモ等）は新 worktree へ自動複製される。
- 終わったら popup で対象を選んで「削除」。未マージのブランチは安全側で残る。
- 詳細な操作・オプション付け直し再起動は `docs/commands-reference.md` の「tmux Claude Worktree ランチャー」を参照。

## ポイント

- **Skill 先行**: 既存パターンを確認してから実装（規約違反を防ぐ）
- **小さな変更**: 1機能ずつ追加 → 即テスト → コミット
- **クロスプラットフォーム変更は検証必須**: `cross_platform_check.sh` を実行
- **git add . 禁止**: 明示的なファイル指定のみ
