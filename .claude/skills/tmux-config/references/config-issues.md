# 現行設定の改善候補（残存）

tmux wiki の推奨事項と現行 dotfiles 設定を照合した結果。
高・中重大度の問題は修正済み。以下は残存する低重大度の改善候補。

## 修正済み (2026-02)

- ~~`default-terminal "xterm-256color"` → `tmux-256color` に修正~~ (base.conf)
- ~~`set-clipboard on` → `external` に修正~~ (wsl.conf, linux.conf, darwin.conf)
- ~~darwin.conf の二重設定を解消~~
- ~~`xterm-keys on` → `extended-keys on` + フォールバック~~ (terminal.conf)
- ~~レガシーバージョン分岐（< 2.1, < 2.2）を削除~~ (base.conf, appearance.conf)
- ~~`bc` 依存のバージョン比較を削除~~
- ~~`status-interval 1` → `5` に変更~~ (base.conf)
- ~~`terminal-overrides` の分散を整理~~ (terminal.conf)
- ~~`focus-events on` を追加~~ (terminal.conf)

## 低重大度（将来の改善候補）

### 1. copy-command 未使用（tmux 3.2+）

OS別 conf で `copy-pipe-and-cancel` を個別にバインドしている。
tmux 3.2+ なら `copy-command` オプションで統一的に設定可能。

```bash
# os/wsl.conf
set -s copy-command 'clip.exe'

# os/linux.conf
set -s copy-command 'xclip -selection clipboard'

# os/darwin.conf
set -s copy-command 'pbcopy'
```

個別の `bind -T copy-mode` が不要になり、設定がシンプルに。
ただし、現在の copy-pipe 方式も正常に動作しており、急ぎではない。

### 2. darwin.conf: pbcopy バインドなし

OSC 52 (`set-clipboard external`) で動作するため実害なし。
ただし OSC 52 非対応ターミナル利用時のフォールバックとして
`copy-command 'pbcopy'` or `copy-pipe` を追加してもよい。
