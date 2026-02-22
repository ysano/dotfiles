---
name: tmux-config
description: >
  tmux設定の変更・デバッグガイド（~3,800行, モジュラー.conf, Claude Voice統合, TPM）。
  .tmux.conf編集、キーバインド変更、ステータスバー調整、OS固有設定時に使用。
allowed-tools: Read, Write, Edit, MultiEdit, Bash, Glob, Grep
---

# Tmux Config Skill

## Architecture & Loading Order

`.tmux.conf` (55行) がエントリーポイント。source-file の順序:

```
.tmux.conf
  ├── base.conf          (46行)  基本設定、Prefix=C-z
  ├── terminal.conf      (18行)  ターミナル設定
  ├── appearance.conf    (33行)  外観設定
  ├── status.conf        (11行)  ステータスバー基本
  ├── keybindings.conf   (15行)  キーバインド
  │
  ├── [OS固有: if-shell 分岐]
  │   ├── os/wsl.conf    (15行)  ← WSL (uname=Linux + WSL_DISTRO_NAME)
  │   ├── os/linux.conf   (9行)  ← Pure Linux
  │   ├── os/darwin.conf (31行)  ← macOS
  │   └── os/freebsd.conf (8行) ← FreeBSD
  │
  ├── [条件付き]
  │   └── claude.conf    (81行)  ← CLAUDE_VOICE_ENABLED=true 時のみ
  │
  └── plugin-config/resurrect.conf (54行) ← resurrect/continuum設定
      └── TPM run (最後)
```

## Conventions

- **Prefix キー**: `C-z`（Emacs の `suspend-frame` と意図的に競合）
- **キーバインド**: emacs モード (`mode-keys emacs`)、emacsCX テーブルで `Prefix x` サブコマンド
- **OS検出順序**: WSL → Pure Linux → macOS → FreeBSD（WSLは `uname=Linux` のため先行検出）
- **status-right**: `resurrect.conf` が最終的に有効（`status.conf` は上書きされる）
- **クリップボード**: OS別 conf で `copy-pipe`/`copy-command` 設定
- **Claude Voice**: `Prefix + v` でメニュー（`claude.conf` で定義、docs/ に詳細）

<constraints>
- status-right を変更する場合は `resurrect.conf` の設定を確認すること
- `plugins/` は TPM が管理 — 直接編集禁止
- `default-terminal` は `tmux-256color` を使用（`xterm-256color` は不正）
- `set-clipboard` は `external` を使用（`on` はセキュリティリスク）
- 変更後は `check_conflicts.sh` で競合検出を実行
</constraints>

## References

詳細な知識は `references/` を参照:
- `references/templates.md` — キーバインド・status-right・OS設定の実例、Plugin Architecture
- `references/wiki-distilled.md` — tmux wiki 蒸留（TERM設定、クリップボード、フォーマット、条件分岐、レシピ）
- `references/config-issues.md` — 現行設定の問題点と改善候補

Claude Voice 詳細: `.tmux/docs/` (7ファイル) を参照。

## Scripts

検証用スクリプト。Claude は**実行して結果を受け取る**（中身を読む必要なし）。

```bash
# 競合検出（status-right 複数定義、キーバインド重複）
.claude/skills/tmux-config/scripts/check_conflicts.sh

# クロスプラットフォーム検証（OS分岐漏れ、クリップボード、source-file パス）
.claude/skills/tmux-config/scripts/cross_platform_check.sh
```

## Common Modifications Checklist

### status-right 変更
- [ ] `resurrect.conf` の `status-right` を確認（最終的に有効な定義）
- [ ] 変更は `resurrect.conf` 側で行う（`status.conf` は上書きされる）
- [ ] `check_conflicts.sh` で status-right 複数定義を検証

### キーバインド追加
- [ ] プレフィックステーブル or root テーブルかを判断
- [ ] `keybindings.conf`（共通）or `os/*.conf`（OS固有）に配置
- [ ] `check_conflicts.sh` でキーバインド重複を検証

### OS固有設定追加
- [ ] `.tmux.conf` の `if-shell` 分岐パターン（WSL先行）に従う
- [ ] 対応する `os/*.conf` にのみ追記
- [ ] `cross_platform_check.sh` で OS分岐漏れを検証

### Claude Voice 変更
- [ ] `.tmux/docs/` の該当ドキュメントを Read
- [ ] `claude.conf` の条件付き読み込み (`CLAUDE_VOICE_ENABLED`) を維持
- [ ] `TMUX_CLAUDE_VOICE_DEBUG=true` でデバッグテスト

## Debugging

```bash
# 設定リロード
tmux source ~/.tmux.conf

# resurrect テスト
~/.tmux/test_resurrect.sh

# デバッグモード有効化
TMUX_DEBUG_MODE=true tmux

# Claude Voice デバッグ
TMUX_CLAUDE_VOICE_DEBUG=true tmux
```

## Do Not Edit

- `plugins/` — TPM が管理するプラグインディレクトリ
- TPM 自体のスクリプト (`plugins/tpm/scripts/`)
