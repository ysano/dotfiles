# アーキテクチャ概要

CLAUDE.md から切り出したアーキテクチャ・トラブルシューティング情報です。

## Emacs設定 (~2,500行)

- **モジュラー構成**: `~/.emacs.d/inits/` 以下に機能別設定
- **AI統合**: `init-ai.el` でOllama、GitHub Copilotを統合
- **ナレッジマネジメント**: `init-org-integrated.el` でOrg-roam + GTD
- **開発環境**: `init-dev.el` で多言語開発支援
- **カスタムLisp**: `~/.emacs.d/elisp/` にユーティリティ関数

## Zsh設定アーキテクチャ

- **ユーティリティライブラリ**: `~/.zsh/utils.zsh` - OS検出、コマンドキャッシュ
- **プラグイン管理**: `~/.zsh/zinit_setup.zsh` - 最適化されたZinit設定
- **エイリアス**: `~/.zsh/aliases.zsh` - モダンツール統合（exa, bat, fd, rg）
- **キーバインド**: `~/.zsh/keybindings.zsh` - Emacsライクナビゲーション

## キーバインド統一システム

- **macOS**: Karabiner-Elements設定 (`karabiner/`)
- **Windows**: Mayu設定 (`mayu/`)
- **クロスプラットフォーム**: Emacsライクキーバインド統一

## WSL最適化

- **フォント**: `wsl/etc_fonts/local.conf`
- **日本語入力**: `wsl/mnt_c_opt_mozc/`
- **X11設定**: `wsl/vcxsrv/config.xlaunch`

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
- キャッシュクリア: `unset ZSH_CMD_CACHE ZSH_OS_TYPE ZSH_IS_WSL`
