---
name: Zsh Config
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

## Key Design Patterns

### OS検出キャッシュ (`utils.zsh`)
```zsh
detect_os()  # → $ZSH_OS_TYPE (darwin/linux/wsl/freebsd/msys)
             # → $ZSH_IS_WSL (true/false)
```

### コマンド存在キャッシュ (`utils.zsh`)
```zsh
has_command CMD  # → $ZSH_CMD_CACHE 連想配列でキャッシュ
                 # 存在しないコマンドの重複チェックを回避
```

### PATH管理 (`utils.zsh`)
```zsh
safe_path_prepend DIR  # 存在するディレクトリのみPATH先頭に追加
safe_path_append DIR   # 存在するディレクトリのみPATH末尾に追加
cleanup_path           # typeset -U path で重複排除
```

### Graceful Degradation Pattern
```zsh
# モダンツールがあれば使い、なければ標準コマンド
if has_command eza; then
    alias ls='eza --icons --git'
elif has_command exa; then
    alias ls='exa --icons --git'
fi
# (標準 ls はフォールバックとして残る)
```

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
