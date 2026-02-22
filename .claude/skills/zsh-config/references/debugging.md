# Debugging & Troubleshooting

## 基本コマンド

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

## Terminal Color Support

### COLORTERM 環境変数
Emacs や bat 等のツールが true color (24bit) を使うには `COLORTERM=truecolor` が必要。
`aliases.zsh` の `setup_truecolor()` で自動検出・設定される。

```zsh
# 検出ロジック:
# 1. COLORTERM が既に設定済み → 何もしない
# 2. xterm-direct terminfo が存在 → truecolor 設定
# 3. TERM が *-256color → truecolor にフォールバック（モダンターミナルはほぼ対応）
```

**注意**: `alias emacs='COLORTERM=truecolor emacs'` ではなく `export COLORTERM=truecolor` を使う。
環境変数にすれば Emacs 以外のツール（bat, delta 等）にも適用される。

### トラブルシューティング
```bash
# true color 対応確認
printf '\e[38;2;255;0;0mRED\e[0m \e[38;2;0;255;0mGREEN\e[0m\n'

# 現在の COLORTERM 確認
echo $COLORTERM

# Emacs 内で色数確認
# M-: (display-color-cells)
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
