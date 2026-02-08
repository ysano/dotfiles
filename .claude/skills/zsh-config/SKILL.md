---
name: configuring-zsh
description: >
  Provides architecture knowledge and modification guidelines for the dotfiles Zsh
  configuration (~4,600 lines, .zprofile/.zshrc dual-stage loading, Zinit plugin manager).
  Use when modifying Zsh settings, adding aliases or shell functions, configuring Zinit
  plugins, managing PATH with safe_path_prepend, editing .zprofile or .zshrc, working
  with .zsh/*.zsh module files, optimizing shell startup time, modifying git-worktree.zsh
  (gwt commands), or troubleshooting Zsh completion, keybindings, or Powerlevel10k prompt.
allowed-tools: Read, Write, Edit, MultiEdit, Bash, Glob, Grep
---

# Zsh Config Skill

## Architecture — Dual-Stage Loading

### Stage 1: Login Shell — `.zprofile` (290行)
```
.zprofile
  └── utils.zsh (160行) を最初に source
      → detect_os(), has_command(), safe_path_prepend/append(), setup_locale()
  → XDG Base Directory 設定
  → 環境変数 (EDITOR, PAGER, LESS, etc.)
  → PATH 構築 (safe_path_prepend で重複排除)
  → 開発ツール設定 (Go, Rust, Node, Python, Ruby, etc.)
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
     → unalias gwt* (OMZ git plugin との競合回避)
  8. .zshrc.local                  ローカル設定(存在時)
  9. .p10k.zsh, .fzf.zsh          プロンプト/fzf
```

**OS固有エイリアス**: `aliases_darwin.zsh`(7行), `aliases_linux.zsh`(13行),
`aliases_freebsd.zsh`(4行), `aliases_msys.zsh`(3行) — `aliases.zsh` から source

## Core Utilities (`utils.zsh`)

| 関数 | 用途 | 戻り値 |
|---|---|---|
| `detect_os()` | OS検出+キャッシュ | `$ZSH_OS_TYPE`, `$ZSH_IS_WSL` |
| `has_command CMD` | コマンド存在チェック+キャッシュ | exit code 0/1 |
| `safe_path_prepend DIR` | PATH先頭に追加（重複排除） | — |
| `safe_path_append DIR` | PATH末尾に追加（重複排除） | — |
| `cleanup_path` | `typeset -U path` で重複排除 | — |

## Templates & Examples

### エイリアス追加（Graceful Degradation 付き）

```zsh
# aliases.zsh に追加
if has_command modern-tool; then
    alias cmd='modern-tool --options'
elif has_command fallback-tool; then
    alias cmd='fallback-tool --options'
fi
# (標準コマンドがフォールバックとして残る)
```

### PATH にツールを追加

```zsh
# .zprofile に追加（safe_path_prepend で重複排除）
safe_path_prepend "$HOME/.new-tool/bin"
```

### OS固有エイリアスの追加

```zsh
# aliases_darwin.zsh / aliases_linux.zsh 等に追加
# aliases.zsh から自動的に source される
alias osx-only='some-command'
```

### Zinit プラグインの追加

```zsh
# zinit_setup.zsh の該当 setup_* 関数内に追加
zinit ice wait lucid
zinit light author/plugin-name
```

### 実例: モダンツールのエイリアス追加

**やりたいこと**: `cat` を bat に置き換え、bat がなければ素の cat を使う

**配置先**: `.zsh/aliases.zsh`（Navigation セクション）

```zsh
if has_command bat; then
    alias cat='bat --style=auto'
fi
```

**ポイント**: `has_command` でガード、`elif`/`else` なしでフォールバックは標準コマンド。

### 実例: 開発ツールの PATH 追加

**やりたいこと**: Deno のパスを追加したい

**配置先**: `.zprofile`（Development Tools セクション）

```zsh
# Deno
safe_path_prepend "$HOME/.deno/bin"
```

**ポイント**: `export PATH=` は使わず `safe_path_prepend` で重複排除。`.zprofile` に配置（ログインシェルで1回だけ実行）。

### 実例: エディタエイリアスのフォールバックチェーン

**やりたいこと**: 環境に応じて最適なエディタを `e` に割り当てたい

**配置先**: `.zsh/aliases.zsh`（`setup_editor_aliases` 関数内）

```zsh
setup_editor_aliases() {
    if [[ "$TERM_PROGRAM" = "vscode" ]]; then
        alias e='code'
    elif has_command emacsclient; then
        alias e='emacsclient -n'
    elif has_command emacs; then
        alias e='emacs'
    elif has_command vim; then
        alias e='vim'
    fi
}
```

**ポイント**: 優先順位付きフォールバック。`$TERM_PROGRAM` で実行コンテキストも考慮。

## Common Modifications Checklist

### エイリアス追加
- [ ] `aliases.zsh` に `has_command` ガード付きで追加
- [ ] フォールバック（モダンツール→標準コマンド）を実装
- [ ] OS固有なら `aliases_<os>.zsh` に配置
- [ ] `validate.sh` で has_command ガードを検証

### PATH 追加
- [ ] `.zprofile` に `safe_path_prepend`/`safe_path_append` で追加
- [ ] `export PATH=` の直接設定を避ける
- [ ] `validate.sh` で PATH 管理を検証

### プラグイン追加
- [ ] `zinit_setup.zsh` の適切な `setup_*` 関数内に配置
- [ ] `wait lucid` で遅延読み込みを設定
- [ ] `benchmark.sh` で起動時間への影響を測定
- [ ] OMZ プラグインとのエイリアス競合を確認

### キーバインド変更
- [ ] `keybindings_custom.zsh` に追加
- [ ] Emacs モード (`bindkey -e`) との整合性を確認
- [ ] `validate.sh` で構文検証

## Zinit Plugin Architecture — `zinit_setup.zsh` (338行)

`main()` 関数から各 `setup_*` を順序呼び出し:
1. `setup_annexes()` — Zinit拡張
2. `setup_omz_libs()` — OMZ ライブラリ (key-bindings, history, etc.)
3. `setup_omz_plugins()` — OMZ プラグイン (git, docker, etc.)
4. `setup_external_plugins()` — 外部プラグイン (zsh-autosuggestions, fast-syntax-highlighting, etc.)
5. `setup_completions()` — 補完設定

**注意**: OMZ git プラグインの `gwt` エイリアスは `git-worktree.zsh` と競合するため、
`.zshrc` で `unalias gwt gwta gwtls gwtmv gwtrm 2>/dev/null || true` で無効化済み。

## Git Worktree — `git-worktree.zsh` (2,996行)

主要コマンド: `gwt`, `gwta`, `gwtls`, `gwtmv`, `gwtrm`
テスト: `./test_git_worktree.zsh`

**注意**: 大規模ファイルのため、変更時は該当部分のみ Read すること。

## Scripts

検証用スクリプトは `scripts/` に配置。Claude は**実行して結果を受け取る**（中身を読む必要なし）。

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

## Debugging

```bash
# デバッグモードで読み込みテスト
ZSH_DEBUG=1 zsh -c "source ~/.zprofile && echo OK"

# 起動時間測定
time zsh -i -c exit

# Zinit プラグイン読み込み時間
# Zsh内: zinit times

# 設定テストスクリプト
./test_zsh_config.zsh
```

## Do Not Edit

- `.zshrc` 内の `MANAGED BY RANCHER DESKTOP` ブロック (L62-64)
- `.p10k.zsh` — `p10k configure` が生成
- `git-worktree.zsh` の変更は慎重に（2,996行、広範囲に影響）
