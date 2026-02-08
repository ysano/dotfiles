---
name: configuring-keyboard
description: >
  Provides architecture knowledge and modification guidelines for the dotfiles keyboard
  configuration (Karabiner-Elements 4,209 lines, skhd 230 lines, yabai 71 lines).
  Shared theme: "Emacs keybindings everywhere" on macOS.
  Use when modifying karabiner.json rules, skhd hotkeys, yabai window manager settings,
  or troubleshooting keybinding conflicts between Karabiner, skhd, and system shortcuts.
allowed-tools: Read, Write, Edit, MultiEdit, Bash, Glob, Grep
---

# Keyboard Config Skill

## Architecture & File Structure

```
karabiner/
  ├── karabiner.json                          (4,209行)  メイン設定
  └── assets/
      └── complex_modifications/
          └── emacs-like.json                  (4,132行)  読み取り専用バックアップ

.skhdrc                                        (230行)   skhd ホットキー定義
.yabairc                                       (71行)    yabai ウィンドウマネージャー設定

keyboard-maestro/
  └── emacs keybind for macos Macros.kmmacros  (1,925行)  読み取り専用

mayu/
  └── 104onFKB.mayu                            (58行)    Windows用（参照のみ）
```

### Deploy Method

| ファイル | デプロイ方法 |
|---|---|
| `.skhdrc` | `link.sh` → `$HOME/.skhdrc` |
| `.yabairc` | `link.sh` → `$HOME/.yabairc` |
| `karabiner/` | 手動: `~/.config/karabiner/` にシンボリックまたはコピー |
| `keyboard-maestro/` | 手動: Keyboard Maestro アプリにインポート |

## Emacs Mode State Machine

Karabiner は2つの仮想変数でステートマシンを実現:

```
emacs_mode_cx       = 0 | 1    C-x プレフィックスキーの有効/無効
emacs_mode_markset  = 0 | 1    C-SPC マークセットの有効/無効
```

**処理フロー**:
1. `C-x` 押下 → `emacs_mode_cx = 1` (1秒タイムアウトで自動リセット)
2. `emacs_mode_cx = 1` の状態で `C-s`/`C-f`/`u`/`k`/`C-c` → 対応操作を実行 → `emacs_mode_cx = 0`
3. `C-SPC` 押下 → `emacs_mode_markset` トグル
4. `emacs_mode_markset = 1` + カーソル移動 → Shift 付きで選択範囲を拡張

## Conventions

### Karabiner Rule Naming
- 全ルールは `[Emacs Mode]` プレフィックスで始める
- C-x サブルールは `[Emacs Mode|C-x]` プレフィックス
- 例: `[Emacs Mode] Control+PNBF to Up/Down/Left/Right`
- 例: `[Emacs Mode|C-x] C-s to Save (Command+S)`

### Bundle Identifiers (除外アプリ)
全ルールで同一の `bundle_identifiers` 除外リストを使用すること（36パターン）。
主なカテゴリ:
- **エディタ**: VSCode, GNU Emacs, Aquamacs
- **ターミナル**: Terminal.app, iTerm2, Hyper
- **リモートデスクトップ**: RDP, TeamViewer, VMware, Parallels, VirtualBox
- **Vim/X11**: MacVim, X11 環境

**一貫性ルール**: 新しいルール追加時は既存ルールの `bundle_identifiers` をコピーすること。
アプリ除外を追加/削除する場合は**全ルール**を一括更新すること。

### skhd Modifier Hierarchy
修飾キーの使い分け:
- `alt -` : ウィンドウフォーカス移動
- `shift + alt -` : ウィンドウスワップ/リサイズ
- `shift + cmd -` : ウィンドウ warp/移動
- `shift + ctrl -` : フローティングウィンドウ位置調整
- `ctrl + alt -` : モニター間フォーカス
- `ctrl + cmd -` : ウィンドウをモニター間移動

### skhd Conflict Markers
Karabiner と競合するバインドは無効化して `# conflict karabiner` コメントを付与:
```
# shift + cmd - right : yabai -m window --warp east # conflict karabiner
```

## Templates

### Karabiner: 新しい Emacs Mode ルールの追加

`rules` 配列に以下の構造でルールを追加する。`bundle_identifiers` は既存ルールからコピーすること。

```json
{
  "description": "[Emacs Mode] Control+<KEY> to <ACTION>",
  "manipulators": [{
    "conditions": [
      {
        "bundle_identifiers": [ "...既存ルールからコピー..." ],
        "type": "frontmost_application_unless"
      },
      { "name": "emacs_mode_cx", "type": "variable_if", "value": 0 }
    ],
    "from": {
      "key_code": "<KEY>",
      "modifiers": { "mandatory": ["control"], "optional": ["caps_lock"] }
    },
    "to": [{ "key_code": "<TARGET_KEY>" }],
    "to_after_key_up": [
      { "set_variable": { "name": "emacs_mode_markset", "value": 0 } }
    ],
    "type": "basic"
  }]
}
```

**注意**: `emacs_mode_markset` をリセットしないルール（選択系）は `to_after_key_up` を省略する。

### skhd: 新しいホットキーの追加

修飾キー階層（Conventions 参照）に従って `.skhdrc` に追加:

```bash
# フォーマット: modifier - key : command
alt - <key> : yabai -m window --focus <direction>

# Karabiner と競合する場合はコメントアウトして記録
# modifier - key : command # conflict karabiner
```

### 実例: C-x サブコマンドの追加

**やりたいこと**: `C-x u` で Undo (Command+Z) を実行するルールを追加

```json
{
  "description": "[Emacs Mode|C-x] u to Undo (Command+Z)",
  "manipulators": [{
    "conditions": [
      {
        "bundle_identifiers": [ "...既存ルールからコピー..." ],
        "type": "frontmost_application_unless"
      },
      { "name": "emacs_mode_cx", "type": "variable_if", "value": 1 }
    ],
    "from": {
      "key_code": "u",
      "modifiers": { "optional": ["caps_lock"] }
    },
    "to": [{ "key_code": "z", "modifiers": ["command"] }],
    "to_after_key_up": [
      { "set_variable": { "name": "emacs_mode_cx", "value": 0 } }
    ],
    "type": "basic"
  }]
}
```

**ポイント**: `emacs_mode_cx = 1` 条件 + `from` に `mandatory: ["control"]` 不要（C-x の後の `u` 単体）。実行後に `emacs_mode_cx = 0` でプレフィックスをリセット。

### 実例: yabai ウィンドウ操作の skhd ホットキー追加

**やりたいこと**: `Alt+d` で親ウィンドウのズームトグルを追加

**配置先**: `.skhdrc`（Window Zoom セクション）

```bash
alt - d : yabai -m window --toggle zoom-parent
```

**ポイント**: `alt -` 階層（ウィンドウフォーカス/操作系）に追加。Karabiner が `alt` 単体を変換しないため競合なし。

## Common Modifications Checklist

### Karabiner ルール追加
- [ ] `[Emacs Mode]` プレフィックスでルール命名
- [ ] 既存ルールから `bundle_identifiers` をコピー
- [ ] `emacs_mode_cx` 条件を適切に設定（C-x サブルールかどうか）
- [ ] `check_karabiner.sh` で JSON構文・除外アプリ一貫性・重複を検証

### 除外アプリの追加/削除
- [ ] **全ルール**の `bundle_identifiers` を一括更新（部分更新は禁止）
- [ ] `check_karabiner.sh` の Bundle Identifier Consistency チェックで PASS を確認

### skhd ホットキー追加
- [ ] 修飾キー階層（alt→shift+alt→shift+cmd→...）に従う
- [ ] Karabiner との競合を確認（`C-<key>` 系は Karabiner が先に処理）
- [ ] 競合する場合はコメントアウト + `# conflict karabiner` を付与
- [ ] `check_skhd_yabai.sh` でバインド重複を検証

### yabai 設定変更
- [ ] `.yabairc` を編集
- [ ] `check_skhd_yabai.sh` で bash 構文を検証
- [ ] `yabai --restart-service` でリロードテスト

## Critical Integration: Karabiner → skhd Processing Order

1. **Karabiner が先に処理**: 物理キー入力を変換
2. **skhd が変換後のキーを受信**: Karabiner の出力が skhd の入力になる
3. **競合の原因**: Karabiner が修飾キー+文字を別のキーに変換すると、skhd がそのバインドを受信できない

**既知の競合**:
- `shift + cmd - right` が Karabiner の Emacs Mode ルールと干渉 → skhd 側で無効化済み

## Scripts

検証用スクリプトは `scripts/` に配置。Claude は**実行して結果を受け取る**（中身を読む必要なし）。

```bash
# Karabiner 検証（JSON構文、除外アプリ一貫性、ルール重複）
.claude/skills/keyboard-config/scripts/check_karabiner.sh

# skhd/yabai 検証（バインド重複、yabairc構文、conflict marker確認）
.claude/skills/keyboard-config/scripts/check_skhd_yabai.sh
```

## Debugging

```bash
# Karabiner EventViewer でキー入力を確認
open /Applications/Karabiner-EventViewer.app

# skhd のリロード
skhd --reload

# yabai のリスタート
yabai --restart-service

# skhd デバッグ（キー入力のログ確認）
skhd -o
```

## Do Not Edit

- `karabiner/assets/` -- 読み取り専用のバックアップ/ソース定義
- `keyboard-maestro/*.kmmacros` -- Keyboard Maestro アプリからエクスポートされたファイル
- `mayu/` -- Windows 専用、macOS 環境から参照のみ
