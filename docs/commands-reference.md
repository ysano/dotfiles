# コマンドリファレンス

CLAUDE.md から切り出した開発コマンドの詳細リファレンスです。

## Emacs設定のデバッグ

```bash
# 設定ファイルの構文チェック
emacs --debug-init --batch -l ~/.emacs.d/inits/init-ai.el | cat

# 特定モジュールのテスト
emacs --batch -l ~/.emacs.d/init.el | cat

# パッケージ依存関係の確認
ls -la ~/.emacs.d/elpa/ | grep <パッケージ名>
```

## Zsh設定のテストとパフォーマンス測定

```bash
# デバッグモードで設定検証
ZSH_DEBUG=1 zsh -c "source ~/.zprofile && echo 'Configuration validated'"

# 起動時間の測定
time zsh -i -c exit

# Zinit統計
zinit times

# キャッシュクリア（必要時）
unset ZSH_CMD_CACHE ZSH_OS_TYPE ZSH_IS_WSL
```

## Git Worktree管理（Claude Code複数起動対応）

```bash
# Git Worktree機能のテスト
./test_git_worktree.zsh

# 新しいworktreeを作成
gwt create <branch-name> [base-branch]

# worktree一覧表示
gwt list

# インタラクティブなworktree切り替え（fzf対応）
gwt switch

# worktreeを削除
gwt remove <worktree-name>

# メンテナンス・クリーンアップ
gwt clean

# ヘルプ表示
gwt help

# 短縮エイリアス
gw       # gwt（最短）
gwc      # gwt create
gwl      # gwt list
gws      # gwt switch
gwr      # gwt remove
```

## tmux操作

### resurrect/continuum

```bash
# TPMプラグインのインストール（初回のみ）
git clone https://github.com/tmux-plugins/tpm ~/.tmux/plugins/tpm
tmux source ~/.tmux.conf
Prefix + I  # プラグインインストール（tmux内で実行）

# 手動保存・復元
Prefix + Ctrl-s  # セッション保存
Prefix + Ctrl-r  # セッション復元

# 保存データの場所
ls -la ~/.local/share/tmux-resurrect/
```

詳細設定・トラブルシューティングは `~/.tmux/plugin-config/README.md` を参照してください。

### Claude Voice統合機能

```bash
# Claude Voice音声エンジンのテスト
~/.tmux/claude/core/wsl_voice_engine.sh test

# ステータス別効果音テスト
~/.tmux/claude/core/wsl_voice_engine.sh sound "⚡"  # 忙しい状態
~/.tmux/claude/core/wsl_voice_engine.sh sound "⌛"  # 待機状態
~/.tmux/claude/core/wsl_voice_engine.sh sound "✅"  # 完了状態

# 複合通知テスト（効果音+音声合成）
~/.tmux/claude/core/wsl_voice_engine.sh notify "処理完了" "✅" 1 "both"
```

#### キーバインド

```
Prefix + v + t       # Claude Voice統合テスト
Prefix + v + v       # Claude Voice自動要約 ON/OFF
Prefix + v + 1-3     # ステータス別音声 ON/OFF (1=完了, 2=待機, 3=忙しい)
Prefix + v + s + 1-3 # 音声合成テスト (s=speech)
Prefix + v + e + 1-3 # 効果音テスト (e=effects)
Prefix + v + n + 1-3 # 複合通知テスト (n=notification)
Prefix + v + p       # パンニング機能 ON/OFF
```

詳細は `~/.tmux/docs/` 配下のドキュメントを参照してください。

## Claude Commands管理

### コマンド実行

```bash
# フォルダ構造での実行
/dev:git-status
/team:issue-triage
/deploy:prepare-release
```

### 出典確認

```bash
# 出典統計（Commands/Agents/Hooks/Skills）
./scripts/check_command_sources.sh

# 新規追加
./scripts/add_new_command.sh <category> <name> <source>
```
