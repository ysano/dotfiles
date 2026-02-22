---
name: emacs-config
description: >
  Emacs設定の変更・デバッグガイド（~4,800行, 12モジュール, straight.el/use-package）。
  init-*.el 編集、パッケージ追加、キーバインド変更、起動エラー修正時に使用。
allowed-tools: Read, Write, Edit, MultiEdit, Bash, Glob, Grep
---

# Emacs Config Skill

## Architecture & Loading Order

```
early-init.el (145行)  <- GC/UI最適化、package.el無効化
    |
init.el (249行)        <- straight.el/use-package初期化、load-path設定
    | (require 順序)
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
    | (optional)
init-local                   <- ローカル設定(locate-libraryで存在時のみ)
```

**elisp/ カスタムライブラリ** (8ファイル, 2,624行):
`smart-font-scaling.el`, `text-adjust.el`, `cygwin-mount.el`, `mell.el`,
`cp5022x.el`, `mozc-cursor-color.el`, `setup-cygwin.el`, `eww-hatebu.el`

## Conventions

**詳細は `.emacs.d/CLAUDE.md` (105行) を Read すること。**

- **use-package**: `:ensure t` + `:defer t`（または `:hook`/`:commands`/`:bind`）必須
- **キーバインドプレフィックス**: `C-c a`=AI, `C-c n`=org-roam, `C-c f`=フォント, `C-c e`=Ellama
- **モジュール終端**: 各 `init-*.el` は末尾に `(provide 'init-xxx)` を記載
- **OS分岐**: `init-platform.el` で `system-type` による条件分岐、WSL は `uname -r` で検出

<constraints>
- 変更後は必ず `validate.sh` を実行（括弧バランス・provide・use-package 規約を検証）
- パッケージ更新時は関連パッケージを全て揃えて更新（magit/magit-section 等）
- `custom-settings.el` と `straight/` は編集禁止
</constraints>

## References

詳細な知識は `references/` を参照:
- `references/templates.md` — use-package テンプレートと実例
- `references/face-customization.md` — フェイス優先順位、遅延ロード、ターミナル設定
- `references/debugging.md` — batch テスト、フェイスデバッグ手順

## Scripts

検証用スクリプト。Claude は**実行して結果を受け取る**（中身を読む必要なし）。

```bash
# 規約検証（provide, 括弧バランス, use-package パターン）
.claude/skills/emacs-config/scripts/validate.sh              # 全モジュール
.claude/skills/emacs-config/scripts/validate.sh init-ai      # 特定モジュール

# キーバインド重複検出
.claude/skills/emacs-config/scripts/check_keybindings.sh

# クロスプラットフォーム検証（system-type 分岐、OS固有パッケージ、フォント）
.claude/skills/emacs-config/scripts/cross_platform_check.sh
```

## Common Modifications Checklist

### パッケージ追加
- [ ] 機能カテゴリから配置先 `init-*.el` を選択
- [ ] `check_keybindings.sh` でキーバインド競合を事前確認
- [ ] `:defer t` / `:hook` / `:commands` で遅延読み込みを設定
- [ ] OS固有なら `when` ガードまたは `init-platform.el` に配置
- [ ] **`validate.sh` で規約検証**（括弧バランス、provide、use-package）

### キーバインド変更
- [ ] プレフィックス体系を確認（Conventions 参照）
- [ ] `check_keybindings.sh` で既存バインドとの重複検出
- [ ] **変更後に `validate.sh` で検証**

## Debugging

```bash
# 特定モジュールの構文チェック
emacs --batch -l ~/.emacs.d/inits/init-XXX.el 2>&1 | cat

# 全体の起動テスト
emacs --batch -l ~/.emacs.d/init.el 2>&1 | cat

# デバッグモードで起動
emacs --debug-init
```

フェイスのデバッグ手順は `references/debugging.md` を参照。

## Do Not Edit

- `custom-settings.el` — Emacs custom-set-variables が自動生成
- `straight/` — パッケージマネージャが管理
