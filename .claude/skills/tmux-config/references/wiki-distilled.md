# tmux Wiki 蒸留リファレンス

Source: https://github.com/tmux/tmux/wiki (2026-02 時点)

## 1. Terminal & TERM 設定

### default-terminal の正しい値

tmux 内部の `TERM` は `tmux-256color` または `screen-256color` を使う。
**`xterm-256color` は誤り** — FAQ で最も多い問題の原因。

```bash
# 正しい設定
set -g default-terminal "tmux-256color"

# tmux-256color の terminfo がない環境向けフォールバック
set -g default-terminal "screen-256color"
```

理由: tmux は xterm ではない。xterm の機能（一部エスケープシーケンス等）をサポートしないため、
アプリケーションが xterm 固有機能を使おうとして表示が壊れる。

### RGB / True Color

tmux 3.2+ は `terminal-features` を推奨:
```bash
set -as terminal-features ",xterm-256color:RGB"   # 外部ターミナルの TERM に合わせる
```

tmux 3.1 以前は `terminal-overrides`:
```bash
set -as terminal-overrides ",xterm*:Tc"
```

### terminal-overrides vs terminal-features

| オプション | tmux バージョン | 用途 |
|---|---|---|
| `terminal-overrides` | 全バージョン | terminfo capability の上書き（レガシー） |
| `terminal-features` | 3.2+ | 機能フラグベースの設定（推奨） |

主な terminal-features フラグ:
- `RGB` — True Color サポート
- `clipboard` — OSC 52 クリップボード
- `extkeys` — 拡張キーサポート

```bash
# 複数フラグを一度に設定
set -as terminal-features ",xterm-256color:RGB:clipboard:extkeys"
```

### escape-time

Escape キーと Alt(Meta) シーケンスを区別するための待機時間（ミリ秒）。

```bash
set -s escape-time 0    # ローカル接続向け（遅延なし）
set -s escape-time 50   # SSH 経由向け（安全な値）
```

デフォルト 500ms は Emacs/Vim ユーザーには遅すぎる。0 に設定すると Alt キーが高速に反応する。
ただし遅いネットワーク越しだとエスケープシーケンスが分断されて誤認識する可能性あり。

### 表示の問題

```bash
# 罫線が破線になる場合（UTF-8 line drawing 問題）
set -as terminal-overrides ",*:U8=0"

# イタリック体が効かない場合（screen は italics 非対応）
set -g default-terminal "tmux-256color"   # tmux 系にすれば解決

# ウィンドウタイトル設定でフリーズする場合
set -g set-titles off
```

## 2. クリップボード

### 2つのアプローチ

| 方式 | 仕組み | 利点 |
|---|---|---|
| **OSC 52** (`set-clipboard`) | ターミナルにエスケープシーケンスを送信 | SSH 越しでも動作、X11 不要 |
| **外部ツール** (`copy-command` / `copy-pipe`) | xclip/pbcopy/clip.exe を実行 | ターミナル非依存 |

### OSC 52 (set-clipboard)

```bash
set -s set-clipboard external   # 推奨（アプリからの直接設定を防ぐ）
set -s set-clipboard on         # アプリからも設定可能（セキュリティリスク）
set -s set-clipboard off        # 無効
```

**`on` vs `external`**: `on` だとペイン内の任意コマンド（sudo 含む）がシステムクリップボードを
書き換え可能。信頼できない環境では `external` を使う。

3つの前提条件:
1. `set-clipboard` が `on` or `external`
2. terminfo に `Ms` capability がある
3. ターミナル自体が OSC 52 をサポート

Ms capability の確認と設定:
```bash
# 確認
tmux info | grep Ms:

# 不足時（tmux 3.2+）
set -as terminal-features ',xterm-256color:clipboard'

# 不足時（旧バージョン）
set -as terminal-overrides ',xterm-256color:Ms=\E]52;%p1%s;%p2%s\007'
```

ターミナル別サポート状況:
| ターミナル | 対応 | 備考 |
|---|---|---|
| iTerm2 | Yes | Preferences で有効化が必要 |
| kitty | Yes | `clipboard_control` で append バグ回避 |
| xterm | Yes | デフォルト無効、`.Xresources` で有効化 |
| VTE (GNOME Terminal 等) | No | `set-clipboard off` にすべき |
| Windows Terminal | Yes | デフォルト有効 |

### 外部ツール方式

tmux 3.2+ は `copy-command` オプションで統一的に設定可能:
```bash
set -s copy-command 'xsel -i'        # Linux
set -s copy-command 'pbcopy'         # macOS
set -s copy-command 'clip.exe'       # WSL
```

tmux 3.1 以前は `copy-pipe` でキーごとに設定:
```bash
# emacs モード
bind -T copy-mode M-w send -X copy-pipe-and-cancel 'xsel -i'
bind -T copy-mode C-w send -X copy-pipe-and-cancel 'xsel -i'

# vi モード
bind -T copy-mode-vi y send -X copy-pipe-and-cancel 'xsel -i'
```

### ネストした tmux

内側の tmux のクリップボードを外のシステムに届けるには、
**外側** の tmux で `set-clipboard on`（`external` ではなく）が必要。

## 3. フォーマットシステム

### 基本構文

`#{}` でフォーマット変数を展開。`#()` でシェルコマンドを実行。

### 主要変数

| 変数 | 内容 |
|---|---|
| `#{session_name}` | セッション名 |
| `#{window_index}`, `#{window_name}` | ウィンドウ番号・名前 |
| `#{pane_current_path}` | ペインの現在ディレクトリ |
| `#{pane_current_command}` | ペインで実行中のコマンド |
| `#{client_prefix}` | プレフィックスキーが押されているか |
| `#{pane_in_mode}` | コピーモード中か |
| `#{alternate_on}` | 代替スクリーン表示中か |
| `#{pane_at_top}`, `#{pane_at_bottom}` | ペインが端にあるか |
| `#{@user_option}` | ユーザー定義オプション |

### 修飾子

```bash
#{b:path}           # basename（パスからファイル名）
#{d:path}           # dirname（パスからディレクトリ）
#{t:timestamp}      # タイムスタンプを読みやすく変換
#{=N:var}           # N文字に切り詰め
#{=|N|...:var}      # N文字に切り詰め、省略時に "..." を付加
#{q:var}            # 特殊文字をクォート
#{E:var}            # 二重展開
#{s/regex/repl/:var}  # 正規表現置換
#{m:pattern,var}    # パターンマッチ（r フラグで正規表現）
```

### 条件式

```bash
# 三項演算子
#{?condition,true_value,false_value}

# 論理演算
#{&&:cond1,cond2}   # AND
#{||:cond1,cond2}   # OR

# 比較
#{==:a,b}  #{!=:a,b}  #{<:a,b}  #{>:a,b}

# ネスト例: プレフィックス押下時に背景色変更
#{?client_prefix,#[bg=red],#[default]}
```

### ループ（tmux 3.2+）

```bash
#{S:format}   # 全セッションを反復
#{W:format}   # 全ウィンドウを反復
#{P:format}   # 全ペインを反復
```

### 数学演算（tmux 3.2+）

`e` 修飾子: `+`, `-`, `*`, `/`, `m`(modulus)。`f` フラグで浮動小数点。

## 4. 条件分岐パターン

### %if（パース時条件 — 推奨）

設定ファイル読み込み時に評価される。`if-shell` より高速。

```bash
%if #{==:#{host_short},myhost}
source-file ~/.tmux.conf.myhost
%elif #{==:#{host_short},otherhost}
source-file ~/.tmux.conf.other
%endif
```

制約: シェルコマンドの結果は使えない（フォーマット変数のみ）。

### if-shell（実行時条件）

シェルコマンドの成否で分岐。OS 検出等に使用。

```bash
if-shell 'command -v xclip' 'set -s copy-command "xclip -i"'

# フォーマット条件も可能（-F フラグ）
bind T if -F '#{==:#{pane_mode},copy-mode}' 'send -X history-top'
```

### バージョン互換性

```bash
# -q フラグで不明なオプションのエラーを抑制
set -gq some-new-option value
```

## 5. キーバインド上級

### カスタムキーテーブル

```bash
# テーブル定義と切替
bind x switch-client -T myTable
bind -T myTable a command-a
bind -T myTable b command-b
```

### extended-keys（tmux 3.2+）

`C-1`, `C-;` 等の従来認識できなかったキーを使用可能に:
```bash
set -s extended-keys on
set -as terminal-features 'xterm*:extkeys'
```

`xterm-keys on` の後継。対応ターミナル: xterm, mintty, iTerm2（CSI u 有効化が必要）。

### コピーモードのカスタマイズ

```bash
# マウスドラッグ終了時の動作変更
unbind -T copy-mode MouseDragEnd1Pane                      # 何もしない
bind -T copy-mode MouseDragEnd1Pane send -X copy-selection  # コピーだが選択維持
bind -T copy-mode MouseDragEnd1Pane send -X copy-selection-no-clear  # コピー＋選択維持
```

### ブロック構文（tmux 3.2+）

```bash
bind K {
    if -F '#{==:#{window_name},ksh}' {
        kill-window
    } {
        display 'not killing window'
    }
}
```

## 6. 実用レシピ

### 新しいペイン/ウィンドウで同じディレクトリを開く

```bash
bind '"' split-window -c "#{pane_current_path}"
bind % split-window -hc "#{pane_current_path}"
bind c new-window -c "#{pane_current_path}"
```

### ペイン移動でラップしない

```bash
bind -r Up if -F '#{pane_at_top}' '' 'selectp -U'
bind -r Down if -F '#{pane_at_bottom}' '' 'selectp -D'
bind -r Left if -F '#{pane_at_left}' '' 'selectp -L'
bind -r Right if -F '#{pane_at_right}' '' 'selectp -R'
```

### マウスホイールの高度な制御

```bash
bind -n WheelUpPane {
    if -F '#{||:#{pane_in_mode},#{mouse_any_flag}}' {
        send -M
    } {
        if -F '#{alternate_on}' { send-keys -N 3 Up } { copy-mode -e }
    }
}
```

### コピーモード専用ペイン（tmux 3.2+）

```bash
bind C {
    splitw -f -l30% ''
    set-hook -p pane-mode-changed 'if -F "#{!=:#{pane_mode},copy-mode}" "kill-pane"'
    copy-mode -s'{last}'
}
```

### CPU 使用率が高い場合

```bash
# automatic-rename が原因の可能性
setw -g automatic-rename off
```

## 7. 人気設定から蒸留したパターン

Source: gpakosz/.tmux, samoshkin/tmux-config, spicycode gist

### ネスト tmux の F12 トグル (samoshkin)

リモート SSH 先の tmux を操作する際、外側のキーバインドを F12 で一括無効化。
ステータスバーに `[OFF]` を表示して視覚的にフィードバック。

```bash
# F12 で外側 tmux のキーバインドを無効化/復元
bind -T root F12 \
  set prefix None \;\
  set key-table off \;\
  set status-style "fg=colour245,bg=colour238" \;\
  if -F '#{pane_in_mode}' 'send-keys -X cancel' \;\
  refresh-client -S

bind -T off F12 \
  set -u prefix \;\
  set -u key-table \;\
  set -u status-style \;\
  refresh-client -S
```

### 環境変数のリフレッシュ (samoshkin)

SSH 再接続時に `SSH_AUTH_SOCK`, `DISPLAY`, `SSH_TTY` が古くなる問題の対策。

```bash
# セッション再アタッチ時に環境変数を更新
set -g update-environment "SSH_AUTH_SOCK SSH_CONNECTION DISPLAY SSH_TTY"
```

### クリップボードのフォールバックチェーン (gpakosz)

プラットフォームごとに最適なクリップボードツールを自動検出:

```bash
# 検出優先順位: xsel → xclip → wl-copy → pbcopy → clip.exe → /dev/clipboard
if-shell 'command -v xsel' 'set -s copy-command "xsel -i --clipboard"' \
  'if-shell "command -v xclip" "set -s copy-command \"xclip -selection clipboard\"" \
    "if-shell \"command -v pbcopy\" \"set -s copy-command pbcopy\" \
      \"if-shell \\\"command -v clip.exe\\\" \\\"set -s copy-command clip.exe\\\"\""'
```

実際には OS 別 conf で分離する方がメンテしやすい（現在の dotfiles 方式）。

### ウィンドウ分割時にカレントディレクトリを継承 (共通)

ほぼ全ての人気設定で採用されている定番パターン:

```bash
bind '"' split-window -v -c "#{pane_current_path}"
bind % split-window -h -c "#{pane_current_path}"
bind c new-window -c "#{pane_current_path}"
```

### ペインリサイズの repeat キー (spicycode, gpakosz)

`-r` フラグで連続リサイズ可能に（prefix を毎回押し直す必要なし）:

```bash
bind -r H resize-pane -L 2
bind -r J resize-pane -D 2
bind -r K resize-pane -U 2
bind -r L resize-pane -R 2
```

### renumber-windows (gpakosz, samoshkin)

ウィンドウを閉じた時に番号の歯抜けを防ぐ:

```bash
set -g renumber-windows on
```

### マウストグル (gpakosz)

マウスサポートを一時的に無効化（テキスト選択でターミナルのネイティブ選択を使いたい時）:

```bash
bind m set -g mouse \; display "Mouse: #{?mouse,ON,OFF}"
```

### 設定リロード (共通)

ほぼ全ての人気設定で採用:

```bash
bind r source-file ~/.tmux.conf \; display "Config reloaded"
```

### SSH 検出によるステータスバー適応 (samoshkin)

リモートセッションではバッテリー・日時を非表示にしてスペース節約:

```bash
%if #{SSH_CLIENT}
  # リモート: 最小限の status-right
  set -g status-right '#{?client_prefix,#[reverse] ^A #[noreverse],} #H'
%else
  # ローカル: フル表示
  set -g status-right '#{?client_prefix,#[reverse] ^A #[noreverse],} %Y-%m-%d %H:%M | #H'
%endif
```

### テーマ変数パターン (samoshkin)

色を変数で管理して一貫性を保つ:

```bash
color_main="colour208"        # オレンジ
color_secondary="colour240"   # グレー
color_level_ok="colour076"    # 緑
color_level_warn="colour220"  # 黄
color_level_stress="colour160" # 赤

set -g status-style "fg=$color_secondary,bg=default"
set -g window-status-current-style "fg=$color_main,bold"
```

注意: tmux は `source-file` 跨ぎで変数を保持しないため、変数定義と使用は同一ファイル内に。

### monitor-activity / monitor-bell (共通)

ウィンドウのアクティビティ・ベル通知:

```bash
set -g monitor-activity on
set -g monitor-bell on
set -g visual-activity off    # メッセージではなくステータスバーのハイライトで通知
```
