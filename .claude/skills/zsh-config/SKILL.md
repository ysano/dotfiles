---
name: zsh-config
description: >
  Zsh設定の変更・デバッグガイド（~4,600行, dual-stage loading, Zinit）。
  エイリアス追加、PATH管理、プラグイン設定、起動時間最適化時に使用。
allowed-tools: Read, Write, Edit, MultiEdit, Bash, Glob, Grep
---

# Zsh Config Skill

## Architecture — Dual-Stage Loading

### Stage 1: Login Shell — `.zprofile` (290行)
```
.zprofile
  +-- utils.zsh (160行) を最初に source
      -> detect_os(), has_command(), safe_path_prepend/append(), setup_locale()
  -> XDG Base Directory 設定
  -> 環境変数 (EDITOR, PAGER, LESS, etc.)
  -> PATH 構築 (safe_path_prepend で重複排除)
  -> 開発ツール設定 (Go, Rust, Node, Python, Ruby, etc.)
```

### Stage 2: Interactive Shell — `.zshrc` (64行)
```
.zshrc
  1. P10k instant prompt (WSL: quiet モード)
  2. core_settings.zsh    (78行)   Zsh オプション、履歴設定
  3. zinit_setup.zsh      (338行)  プラグインマネージャ
  4. colors_and_prompt.zsh (52行)  テーマ、Powerlevel10k
  5. keybindings_custom.zsh (79行) カスタムキーバインド
  6. functions.zsh          (29行) シェル関数
     aliases.zsh           (334行) エイリアス定義
  7. git-worktree.zsh    (2,996行) Git Worktree コマンド群
     -> unalias gwt* (OMZ git plugin との競合回避)
  8. .zshrc.local                  ローカル設定(存在時)
  9. .p10k.zsh, .fzf.zsh          プロンプト/fzf
```

**OS固有エイリアス**: `aliases_darwin.zsh`, `aliases_linux.zsh`,
`aliases_freebsd.zsh`, `aliases_msys.zsh` — `aliases.zsh` から source

## Core Utilities (`utils.zsh`)

| 関数 | 用途 | 戻り値 |
|---|---|---|
| `detect_os()` | OS検出+キャッシュ | `$ZSH_OS_TYPE`, `$ZSH_IS_WSL` |
| `has_command CMD` | コマンド存在チェック+キャッシュ | exit code 0/1 |
| `safe_path_prepend DIR` | PATH先頭に追加（重複排除） | -- |
| `safe_path_append DIR` | PATH末尾に追加（重複排除） | -- |
| `cleanup_path` | `typeset -U path` で重複排除 | -- |

## Conventions

- **エイリアス**: `has_command` ガード付き、フォールバック（モダンツール→標準コマンド）
- **PATH**: `safe_path_prepend`/`safe_path_append` で管理（`.zprofile` に配置）
- **プラグイン**: `zinit_setup.zsh` の `setup_*` 関数内、`wait lucid` で遅延読み込み
- **キーバインド**: `keybindings_custom.zsh`、Emacs モード (`bindkey -e`)
- **OS固有**: `aliases_<os>.zsh` に配置（`aliases.zsh` から自動 source）

<constraints>
- 変更後は必ず `validate.sh` を実行（構文、has_command ガード、PATH 管理を検証）
- PATH は `safe_path_prepend`/`safe_path_append` を使用（`export PATH=` 禁止）
- `.p10k.zsh` は編集禁止（`p10k configure` が生成）
- `.zshrc` 内の `MANAGED BY RANCHER DESKTOP` ブロック (L62-64) は編集禁止
- `git-worktree.zsh` (2,996行) の変更は慎重に — 該当部分のみ Read すること
</constraints>

## References

詳細な知識は `references/` を参照:
- `references/templates.md` — エイリアス・PATH・プラグイン追加のテンプレートと実例
- `references/debugging.md` — デバッグコマンド、Terminal Color Support、Zinit アーキテクチャ

## Scripts

検証用スクリプト。Claude は**実行して結果を受け取る**（中身を読む必要なし）。

```bash
# 設定検証（source チェーン、構文、has_command ガード、PATH管理）
.claude/skills/zsh-config/scripts/validate.sh                # 全モジュール
.claude/skills/zsh-config/scripts/validate.sh aliases.zsh    # 特定モジュール

# クロスプラットフォーム検証（OS検出、エイリアス分岐、PATH、グレースフル劣化）
.claude/skills/zsh-config/scripts/cross_platform_check.sh

# 起動時間ベンチマーク（閾値判定付き）
.claude/skills/zsh-config/scripts/benchmark.sh               # デフォルト: 5回、500ms
.claude/skills/zsh-config/scripts/benchmark.sh --threshold 300 --runs 10
```

## Common Modifications Checklist

### エイリアス追加
- [ ] `aliases.zsh` に `has_command` ガード付きで追加
- [ ] フォールバック（モダンツール→標準コマンド）を実装
- [ ] OS固有なら `aliases_<os>.zsh` に配置
- [ ] **`validate.sh` で検証**

### PATH 追加
- [ ] `.zprofile` に `safe_path_prepend`/`safe_path_append` で追加
- [ ] `export PATH=` の直接設定を避ける
- [ ] **`validate.sh` で検証**

### プラグイン追加
- [ ] `zinit_setup.zsh` の適切な `setup_*` 関数内に配置
- [ ] `wait lucid` で遅延読み込みを設定
- [ ] **`benchmark.sh` で起動時間への影響を測定**
- [ ] OMZ プラグインとのエイリアス競合を確認

### キーバインド変更
- [ ] `keybindings_custom.zsh` に追加
- [ ] Emacs モード (`bindkey -e`) との整合性を確認
- [ ] **`validate.sh` で構文検証**

## Debugging

```bash
# デバッグモードで読み込みテスト
ZSH_DEBUG=1 zsh -c "source ~/.zprofile && echo OK"

# 起動時間測定
time zsh -i -c exit
```

詳細は `references/debugging.md` を参照。
