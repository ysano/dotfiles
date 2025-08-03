# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## 重要な注意事項

- 不具合を修正する前に、不具合を再現するテストコードを書き、失敗することを確認すること。その後、実装を変更すること。
- 日本語で受け答えすること

## リポジトリ概要

このdotfilesリポジトリは、クロスプラットフォーム（Windows、macOS、Linux/WSL）対応の統合開発環境設定です。Emacsを中心とした構成で、AI統合ワークフロー、日本語環境、高度なZsh設定を含んでいます。

## 主要な開発コマンド

### セットアップとリンク作成
```bash
# 設定ファイルのシンボリックリンク作成
./link.sh

# Zsh設定の検証とテスト
./test_zsh_config.zsh
```

### Emacs設定のデバッグ
```bash
# 設定ファイルの構文チェック
emacs --debug-init --batch -l ~/.emacs.d/inits/init-ai.el | cat

# 特定モジュールのテスト
emacs --batch -l ~/.emacs.d/init.el | cat

# パッケージ依存関係の確認
ls -la ~/.emacs.d/elpa/ | grep <パッケージ名>
```

### Zsh設定のテストとパフォーマンス測定
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

### Git Worktree管理（Claude Code複数起動対応）
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

### Claude Voice統合機能（tmux）

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

#### tmux Claude Voiceキーバインド
```
Prefix + v + t       # Claude Voice統合テスト
Prefix + v + v       # Claude Voice自動要約 ON/OFF
Prefix + v + 1-3     # ステータス別音声 ON/OFF (1=完了, 2=待機, 3=忙しい)
Prefix + v + s + 1-3 # 音声合成テスト (s=speech)
Prefix + v + e + 1-3 # 効果音テスト (e=effects)  
Prefix + v + n + 1-3 # 複合通知テスト (n=notification)
Prefix + v + p       # パンニング機能 ON/OFF
```

#### Claude Voiceステータス音声バリエーション
- **⚡ 忙しい状態**: 警告パターン (800Hz×2→600Hz)
- **⌛ 待機状態**: 上昇メロディー (659Hz→880Hz→1175Hz)  
- **✅ 完了状態**: 成功パターン (523Hz→659Hz→783Hz→1046Hz)
- **Equal Power Pan Law**: 3dBセンター法によるステレオ配置
- **音声合成**: Microsoft Haruka Desktopによる日本語読み上げ

## アーキテクチャ概要

### Emacs設定 (~2,500行)
- **モジュラー構成**: `~/.emacs.d/inits/` 以下に機能別設定
- **AI統合**: `init-ai.el` でOllama、GitHub Copilotを統合
- **ナレッジマネジメント**: `init-org-integrated.el` でOrg-roam + GTD
- **開発環境**: `init-dev.el` で多言語開発支援
- **カスタムLisp**: `~/.emacs.d/elisp/` にユーティリティ関数

### Zsh設定アーキテクチャ
- **ユーティリティライブラリ**: `~/.zsh/utils.zsh` - OS検出、コマンドキャッシュ
- **プラグイン管理**: `~/.zsh/zinit_setup.zsh` - 最適化されたZinit設定
- **エイリアス**: `~/.zsh/aliases.zsh` - モダンツール統合（exa, bat, fd, rg）
- **キーバインド**: `~/.zsh/keybindings.zsh` - Emacsライクナビゲーション

### キーバインド統一システム
- **macOS**: Karabiner-Elements設定 (`karabiner/`)
- **Windows**: Mayu設定 (`mayu/`)
- **クロスプラットフォーム**: Emacsライクキーバインド統一

### WSL最適化
- **フォント**: `wsl/etc_fonts/local.conf`
- **日本語入力**: `wsl/mnt_c_opt_mozc/`
- **X11設定**: `wsl/vcxsrv/config.xlaunch`

## 重要な設計原則

### パフォーマンス最適化
- **キャッシュ機構**: OS検出とコマンド存在チェックをキャッシュ
- **遅延読み込み**: Zinitとuse-packageで段階的ロード
- **PATH管理**: 重複チェックと安全な追加機能

### エラーハンドリング
- **グレースフル劣化**: モダンツール未導入時の自動フォールバック
- **設定検証**: デバッグモードでの包括的チェック
- **バックアップ**: 既存設定の自動保護

### AI統合ワークフロー
- **複数LLMプロバイダー**: Ollama（ローカル）、GitHub Copilot（クラウド）
- **専門化モデル**: コーディング用、翻訳用、汎用の使い分け
- **統合キーバインド**: `C-c a` プレフィックスで統一操作

## 主要ファイルの役割

### 設定の読み込み順序
1. `~/.zprofile` → OS検出とPATH設定
2. `~/.zshrc` → インタラクティブ設定
3. `~/.zsh/utils.zsh` → ユーティリティ関数
4. `~/.zsh/zinit_setup.zsh` → プラグイン読み込み

### Emacs初期化順序
1. `early-init.el` → パフォーマンス最適化
2. `init.el` → パッケージシステム初期化
3. `inits/` モジュール群 → 機能別設定

## トラブルシューティング

### よくある問題のデバッグ手順
1. **Zsh起動エラー**: `ZSH_DEBUG=1` でデバッグモード実行
2. **Emacs起動失敗**: `emacs --debug-init` で初期化デバッグ
3. **パッケージエラー**: `M-x package-refresh-contents` でリフレッシュ
4. **WSL表示問題**: `echo $DISPLAY` でX11設定確認

### パフォーマンス問題
- Emacs起動時間: `M-x emacs-init-time` で測定
- Zsh起動時間: `time zsh -i -c exit` で測定
- キャッシュクリア: 上記コマンド参照

## 開発時の注意点

### コード修正時のワークフロー
1. 該当ファイルのバックアップ作成
2. テストスクリプトで動作確認
3. 段階的デプロイ（新ファイル → リンク切り替え）
4. 設定検証スクリプト実行

### 新機能追加時
- OS固有機能は条件分岐で対応
- フォールバック機構を必ず実装
- デバッグ出力を含める
- パフォーマンス影響を測定

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
# 出典統計
./scripts/check_command_sources.sh

# 新規追加
./scripts/add_new_command.sh <category> <name> <source>
```