# コマンドリファレンス

ツール固有のデバッグコマンドは各 SKILL.md の Debugging セクションを参照。

## Git Worktree 管理

```bash
# worktree 操作
gwt create <branch-name> [base-branch]   # 新規作成
gwt list                                  # 一覧表示
gwt switch                               # fzf で切り替え
gwt remove <worktree-name>               # 削除
gwt clean                                # クリーンアップ
gwt help                                 # ヘルプ

# 短縮エイリアス
gw       # gwt
gwc      # gwt create
gwl      # gwt list
gws      # gwt switch
gwr      # gwt remove
```

テスト: `./test_git_worktree.zsh`

## tmux resurrect/continuum

```bash
# TPM 初回セットアップ
git clone https://github.com/tmux-plugins/tpm ~/.tmux/plugins/tpm
tmux source ~/.tmux.conf
# Prefix + I  でプラグインインストール

# セッション操作
# Prefix + Ctrl-s  セッション保存
# Prefix + Ctrl-r  セッション復元

# 保存データ
ls -la ~/.local/share/tmux-resurrect/
```

詳細: `~/.tmux/plugin-config/README.md`

## tmux Claude Voice

```bash
# Prefix + v  で Claude Voice メニュー
```

詳細: `~/.tmux/docs/` 配下のドキュメント
