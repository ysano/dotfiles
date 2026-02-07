---
name: Emacs Config
description: >
  Provides architecture knowledge and modification guidelines for the dotfiles Emacs
  configuration (~4,800 lines, 12 init-*.el modules, 8 custom elisp libraries).
  Use when modifying Emacs settings, adding use-package declarations, changing keybindings,
  debugging startup errors, editing init.el or early-init.el, working with .emacs.d/inits/
  files, or troubleshooting Emacs Lisp, MELPA packages, Copilot, Ellama, or org-roam.
allowed-tools: Read, Write, Edit, MultiEdit, Bash, Glob, Grep
---

# Emacs Config Skill

## Architecture & Loading Order

```
early-init.el (145行)  ← GC/UI最適化、package.el無効化
    ↓
init.el (249行)        ← straight.el/use-package初期化、load-path設定
    ↓ (require 順序)
init-ui-simple      (128行)  UI/テーマ/フォント
init-editor         (214行)  エディタ基本機能
init-navigation     (218行)  ivy/counsel/swiper/ace
init-completion     (173行)  company/copilot補完
init-dev-core       (210行)  magit/projectile/flycheck
init-dev-languages  (149行)  LSP/言語モード
init-dev-web        (240行)  Web開発
init-text-modes      (61行)  markdown/yaml
init-org-simple     (104行)  org-mode/org-roam
init-ai             (138行)  Copilot/Ellama/LLM統合
init-japanese        (52行)  mozc/フォント
init-platform       (120行)  OS固有設定
    ↓ (optional)
init-local                   ← ローカル設定(locate-libraryで存在時のみ)
```

**elisp/ カスタムライブラリ** (8ファイル, 2,624行):
`smart-font-scaling.el`(370行), `text-adjust.el`(479行), `cygwin-mount.el`(565行),
`mell.el`(574行), `cp5022x.el`(156行), `mozc-cursor-color.el`(129行),
`setup-cygwin.el`(278行), `eww-hatebu.el`(73行)

## Conventions

**詳細は `.emacs.d/CLAUDE.md` (105行) を Read すること。**

### use-package パターン
```elisp
(use-package PACKAGE-NAME
  :ensure t        ; straight.el で自動インストール
  :defer t         ; 遅延読み込み（基本）
  :hook (MODE . PACKAGE-mode)
  :bind (("KEY" . COMMAND))
  :config
  (setq ...))
```

### キーバインドプレフィックス体系
| プレフィックス | 用途 | 定義場所 |
|---|---|---|
| `C-c a` | AI統合 (Copilot/Ellama) | `init-ai.el` |
| `C-c n` | org-roam/Zettelkasten | `init-org-simple.el` |
| `C-c f` | フォントスケーリング | `init-platform.el` |
| `C-c e` | Ellama LLMコマンド | `init-ai.el` |

### モジュール終端
各 `init-*.el` は末尾に `(provide 'init-xxx)` を記載すること。

## OS Fixed Settings Pattern

```elisp
;; init-platform.el での分岐パターン
(cond
  ((eq system-type 'darwin)    ...)  ; macOS
  ((eq system-type 'gnu/linux) ...)  ; Linux/WSL
  ((eq system-type 'windows-nt) ...) ; Windows
  (t ...))                           ; フォールバック

;; WSL検出
(string-match-p "microsoft" (shell-command-to-string "uname -r"))
```

## New Package Addition Guide

1. **配置先の判定**: パッケージの機能カテゴリから対応する `init-*.el` を選択
2. **依存チェック**: 既存パッケージとのキーバインド競合を Grep で確認
3. **遅延読み込み**: `:defer t` または `:hook` / `:commands` で遅延化
4. **OS制約**: OS固有パッケージは `init-platform.el` に、または `when` ガードを追加

## Scripts

検証用スクリプトは `scripts/` に配置。Claude は**実行して結果を受け取る**（中身を読む必要なし）。

```bash
# 規約検証（provide, 括弧バランス, use-package パターン）
.claude/skills/emacs-config/scripts/validate.sh              # 全モジュール
.claude/skills/emacs-config/scripts/validate.sh init-ai      # 特定モジュール

# キーバインド重複検出
.claude/skills/emacs-config/scripts/check_keybindings.sh

# クロスプラットフォーム検証（system-type 分岐、OS固有パッケージ、フォント）
.claude/skills/emacs-config/scripts/cross_platform_check.sh
```

## Debugging

```bash
# 特定モジュールの構文チェック
emacs --batch -l ~/.emacs.d/inits/init-XXX.el 2>&1 | cat

# 全体の起動テスト
emacs --batch -l ~/.emacs.d/init.el 2>&1 | cat

# デバッグモードで起動
emacs --debug-init

# 起動時間測定
# Emacs内: M-x emacs-init-time
```

## Do Not Edit

- `custom-settings.el` — Emacs custom-set-variables が自動生成
- `straight/` — パッケージマネージャが管理
