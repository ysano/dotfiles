---
name: configuring-tmux
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

## Templates & Examples

### status-right を変更したい場合

**問題**: `status.conf` を編集したが反映されない
**原因**: `resurrect.conf` が後から読み込まれ上書きしている
**解決**: `resurrect.conf` 側の `status-right` を編集する

```bash
# .tmux/plugin-config/resurrect.conf で設定（ここが最終的に有効）
set -g status-right '#{?client_prefix,#[reverse] C-z #[noreverse],} ...'
```

### 新しいキーバインドを追加したい場合

```bash
# .tmux/keybindings.conf に追加（prefix テーブル）
bind <key> <command>

# root テーブル（prefix 不要）の場合
bind -n <key> <command>

# OS固有の場合は対応する os/*.conf に追加
# 例: macOS のみ → .tmux/os/darwin.conf
```

### OS固有の設定を追加したい場合

```bash
# .tmux.conf 内の if-shell 分岐パターンに従う（WSL先行検出）
if-shell 'test -n "$WSL_DISTRO_NAME"' 'source-file ~/.tmux/os/wsl.conf'
if-shell 'uname | grep -q Darwin' 'source-file ~/.tmux/os/darwin.conf'
```

### 実例: Emacs風キーテーブルにコマンドを追加

**やりたいこと**: `Prefix x b` でバッファ一覧（choose-tree）を開きたい

**配置先**: `.tmux/keybindings.conf`（emacsCX テーブルに追加）

```bash
# 既存: bind x switch-client -T emacsCX  (emacsCX テーブルへの切替)
# 既存の emacsCX バインド (0=kill, 1=break, 2=split-v, 3=split-h, ...)
bind -T emacsCX b choose-tree -Zs
```

**ポイント**: `bind -T emacsCX` で Prefix x の後に続くキーを定義。既存の 0/1/2/3/o/k/i と重複しないキーを選ぶ。

### 実例: OS固有のクリップボード設定

**やりたいこと**: WSL でシステムクリップボードとの連携を追加

**配置先**: `.tmux/os/wsl.conf`

```bash
# コピーモードで選択したテキストを Windows クリップボードにコピー
bind -T copy-mode M-w send-keys -X copy-pipe-and-cancel "clip.exe"
```

**ポイント**: OS固有設定は対応する `os/*.conf` に配置。`if-shell` 分岐で排他的に読み込まれるため、他 OS への影響なし。

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

## Plugin Architecture

### TPM (Tmux Plugin Manager)
- プラグインディレクトリ: `.tmux/plugins/`
- `tpm` は `.tmux.conf` 最終行の `run '~/.tmux/plugins/tpm/tpm'` で起動

### resurrect/continuum
- 設定ファイル: `.tmux/plugin-config/resurrect.conf` (54行)
- 詳細設定ドキュメント: `.tmux/plugin-config/README.md` (206行)
- セッション保存/復元とバックグラウンド自動保存を管理

## Scripts

検証用スクリプトは `scripts/` に配置。Claude は**実行して結果を受け取る**（中身を読む必要なし）。

```bash
# 競合検出（status-right 複数定義、キーバインド重複）
.claude/skills/tmux-config/scripts/check_conflicts.sh

# クロスプラットフォーム検証（OS分岐漏れ、クリップボード、source-file パス）
.claude/skills/tmux-config/scripts/cross_platform_check.sh
```

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
