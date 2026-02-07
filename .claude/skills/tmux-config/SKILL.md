---
name: Tmux Config
description: >
  Provides architecture knowledge and modification guidelines for the dotfiles tmux
  configuration (~3,800 lines, modular .conf files, Claude Voice integration, TPM plugins).
  Use when modifying tmux settings, editing .tmux.conf or .tmux/*.conf files, configuring
  Claude Voice audio notifications, adjusting status bar or status-right, working with
  tmux-resurrect, tmux-continuum, TPM plugins, if-shell OS detection, keybindings,
  or troubleshooting tmux session persistence and pane layouts.
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

## Critical Integration Points

### status-right 競合に注意
`status.conf` と `resurrect.conf` の両方が `status-right` を設定する。
**`resurrect.conf` が最後に読み込まれるため、その値が最終的に有効になる。**
status-right を変更する場合は `resurrect.conf` の設定も確認すること。

### Prefix キー
`C-z` を使用。Emacs の `suspend-frame` と意図的に競合（tmux内ではtmuxが優先）。

### OS検出順序
WSL は Linux と同じ `uname=Linux` を返すため、**WSLを先に検出**する設計:
1. WSL: `uname=Linux` + `$WSL_DISTRO_NAME` or `/proc/version` に "microsoft"
2. Pure Linux: `uname=Linux` + WSLではない
3. macOS: `uname=Darwin`
4. FreeBSD: `uname=FreeBSD`

## Claude Voice Integration

`claude/` ディレクトリ (11ファイル, ~3,448行):

| ファイル | 行数 | 責務 |
|---|---|---|
| `panning_engine.sh` | 791 | 音声パンニングエンジン |
| `sound_utils.sh` | 616 | サウンドユーティリティ |
| `ollama_utils.sh` | 521 | Ollama LLM連携 |
| `functions.sh` | 510 | 共通関数 |
| `integration_test.sh` | 387 | 統合テスト |
| `toggle_notify_mode.sh` | 179 | 通知モード切替 |
| `polling_monitor.sh` | 151 | ポーリング監視 |
| `pan_test.sh` | 130 | パンニングテスト |
| `core/logging_utils.sh` | 74 | ログユーティリティ |
| `quick_pan_test.sh` | 49 | クイックテスト |
| `unified_pan_test.sh` | 40 | 統合パンテスト |

**詳細ドキュメント**: `.tmux/docs/` (7ファイル) を参照:
- `01-system-overview.md` — システム全体概要
- `02-implementation-guide.md` — 実装ガイド
- `03-panning-engine.md` — パンニングエンジン詳細
- `04-ollama-integration.md` — Ollama連携
- `05-sound-engine.md` — サウンドエンジン
- `06-implementation-checklist.md` — 実装チェックリスト

**キーバインド**: `Prefix + v` で Claude Voice メニュー（`claude.conf` で定義）

## Plugin Architecture

### TPM (Tmux Plugin Manager)
- プラグインディレクトリ: `.tmux/plugins/`
- `tpm` は `.tmux.conf` 最終行の `run '~/.tmux/plugins/tpm/tpm'` で起動

### resurrect/continuum
- 設定ファイル: `.tmux/plugin-config/resurrect.conf` (54行)
- 詳細設定ドキュメント: `.tmux/plugin-config/README.md` (206行)
- セッション保存/復元とバックグラウンド自動保存を管理

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
