# Templates & Examples

## エイリアス追加（Graceful Degradation 付き）

```zsh
# aliases.zsh に追加
if has_command modern-tool; then
    alias cmd='modern-tool --options'
elif has_command fallback-tool; then
    alias cmd='fallback-tool --options'
fi
# (標準コマンドがフォールバックとして残る)
```

## 実例: モダンツールのエイリアス追加

**やりたいこと**: `cat` を bat に置き換え、bat がなければ素の cat を使う

**配置先**: `.zsh/aliases.zsh`（Navigation セクション）

```zsh
if has_command bat; then
    alias cat='bat --style=auto'
fi
```

**ポイント**: `has_command` でガード、`elif`/`else` なしでフォールバックは標準コマンド。

## PATH にツールを追加

```zsh
# .zprofile に追加（safe_path_prepend で重複排除）
safe_path_prepend "$HOME/.new-tool/bin"
```

## 実例: 開発ツールの PATH 追加

**やりたいこと**: Deno のパスを追加したい

**配置先**: `.zprofile`（Development Tools セクション）

```zsh
# Deno
safe_path_prepend "$HOME/.deno/bin"
```

**ポイント**: `export PATH=` は使わず `safe_path_prepend` で重複排除。`.zprofile` に配置（ログインシェルで1回だけ実行）。

## OS固有エイリアスの追加

```zsh
# aliases_darwin.zsh / aliases_linux.zsh 等に追加
# aliases.zsh から自動的に source される
alias osx-only='some-command'
```

## Zinit プラグインの追加

```zsh
# zinit_setup.zsh の該当 setup_* 関数内に追加
zinit ice wait lucid
zinit light author/plugin-name
```

## 実例: エディタエイリアスのフォールバックチェーン

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
