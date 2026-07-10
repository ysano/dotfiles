#!/bin/bash

# Claude Code ステータスライン設定
# Powerlevel10k Rainbow テーマスタイル
# 参考: ~/.p10k.zsh (nerdfont-v3 + powerline, rainbow, 2 lines, round heads/tails)

# JSON入力を取得
input=$(cat)

# JSON から情報を抽出
model=$(echo "$input" | jq -r '.model.display_name // "Claude"')
current_dir=$(echo "$input" | jq -r '.workspace.current_dir // ""')
output_style=$(echo "$input" | jq -r '.output_style.name // ""')
vim_mode=$(echo "$input" | jq -r '.vim.mode // ""')
remaining_pct=$(echo "$input" | jq -r '.context_window.remaining_percentage // ""')
agent_name=$(echo "$input" | jq -r '.agent.name // ""')
worktree_name=$(echo "$input" | jq -r '.worktree.name // ""')
worktree_branch=$(echo "$input" | jq -r '.worktree.branch // ""')
git_worktree=$(echo "$input" | jq -r '.workspace.git_worktree // ""')
cc_version=$(echo "$input" | jq -r '.version // ""')

# Powerlevel10k Rainbow 色定義（ANSI 256色）
# 背景色
BG_OS="\033[48;5;7m"           # 白背景 (OS icon)
BG_DIR="\033[48;5;4m"          # 青背景 (directory)
BG_GIT_CLEAN="\033[48;5;2m"    # 緑背景 (git clean)
BG_GIT_DIRTY="\033[48;5;3m"    # 黄背景 (git modified)
BG_TIME="\033[48;5;7m"         # 白背景 (time)
BG_CONTEXT="\033[48;5;0m"      # 黒背景 (context)
BG_WARN="\033[48;5;1m"         # 赤背景 (warning)
BG_WORKTREE="\033[48;5;5m"     # マゼンタ背景 (worktree)

# 前景色
FG_BLACK="\033[38;5;0m"        # 黒
FG_WHITE="\033[38;5;254m"      # 白
FG_GREY="\033[38;5;244m"       # グレー
FG_GREEN="\033[38;5;76m"       # 緑 (prompt char OK)
FG_RED="\033[38;5;196m"        # 赤 (prompt char ERROR)
FG_CYAN="\033[38;5;51m"        # シアン
FG_BLUE="\033[38;5;39m"        # 青
FG_YELLOW="\033[38;5;220m"     # 黄
FG_MAGENTA="\033[38;5;205m"    # マゼンタ

C_RESET="\033[0m"

# Powerline セパレータ（Nerd Font）
SEP_RIGHT_HARD=""  # U+E0B4 (丸い終端)
SEP_LEFT_HARD=""   # U+E0B6 (丸い開始)
SEP_RIGHT_THIN=""  # U+E0B5
SEP_LEFT_THIN=""   # U+E0B7

# OS icon (macOS)
os_icon=""  # Nerd Font Apple icon

# ディレクトリ表示（P10k の truncate_to_unique スタイル）
if [ -n "$current_dir" ]; then
    # ホームディレクトリを ~ に置き換え
    display_dir="${current_dir/#$HOME/~}"

    # パスが長い場合は最後の2階層のみ表示
    dir_parts=$(echo "$display_dir" | awk -F'/' '{print NF-1}')
    if [ "$dir_parts" -gt 2 ]; then
        display_dir="…/$(echo "$display_dir" | awk -F'/' '{print $(NF-1)"/"$NF}')"
    fi
else
    display_dir="~"
fi

# Git情報を取得（オプショナルロックをスキップ）
git_branch=""
git_dirty=false
git_icon=""  # Nerd Font git branch icon

if [ -n "$current_dir" ] && [ -d "$current_dir" ]; then
    cd "$current_dir" 2>/dev/null
    if git rev-parse --git-dir > /dev/null 2>&1; then
        # ブランチ名を取得
        git_branch=$(git -c core.fileMode=false -c core.safecrlf=false symbolic-ref --short HEAD 2>/dev/null || git -c core.fileMode=false -c core.safecrlf=false rev-parse --short HEAD 2>/dev/null)

        # Git ステータスを確認（高速化のため簡易チェック）
        if [ -n "$(git -c core.fileMode=false -c core.safecrlf=false status --porcelain 2>/dev/null)" ]; then
            git_dirty=true
        fi
    fi
fi

# Claude Code バージョン差異（最新版は6hキャッシュ＋バックグラウンド更新）
# 描画のたびに実行されるため npm view は直接呼ばず、キャッシュを読むだけにする
version_display=""
if [ -n "$cc_version" ]; then
    _cc_cache="$HOME/.claude/.cc-latest-version"
    # 6時間より古い or 無ければバックグラウンドで更新（描画はブロックしない）
    if [ -z "$(find "$_cc_cache" -mmin -360 2>/dev/null)" ]; then
        ( npm view @anthropic-ai/claude-code version 2>/dev/null > "$_cc_cache.tmp" \
            && mv "$_cc_cache.tmp" "$_cc_cache" ) >/dev/null 2>&1 &
    fi
    _cc_latest=$(cat "$_cc_cache" 2>/dev/null)
    # 古い時だけ警告表示（最新なら何も出さない）
    if [ -n "$_cc_latest" ] && [ "$cc_version" != "$_cc_latest" ]; then
        version_display="⚠v${cc_version}->${_cc_latest}"
    fi
fi

# コンテキスト残量の表示
context_display=""
if [ -n "$remaining_pct" ]; then
    remaining_int=${remaining_pct%.*}
    if [ "$remaining_int" -lt 20 ]; then
        context_icon="⚠"
        context_color="${FG_RED}"
    elif [ "$remaining_int" -lt 50 ]; then
        context_icon="◐"
        context_color="${FG_YELLOW}"
    else
        context_icon="◉"
        context_color="${FG_GREY}"
    fi
    context_display="${context_icon}${remaining_int}%"
fi

# Vimモード表示（P10k の prompt_char スタイル）
vim_display=""
if [ "$vim_mode" = "NORMAL" ]; then
    vim_display="❮"  # VICMD
    vim_color="${FG_GREEN}"
elif [ "$vim_mode" = "INSERT" ]; then
    vim_display="❯"  # VIINS
    vim_color="${FG_CYAN}"
fi

# Agent 名表示
agent_display=""
if [ -n "$agent_name" ]; then
    agent_display="[${agent_name}]"
fi

# ステータスラインを組み立て（Powerline スタイル）
status_line=""

# セグメント1: OS Icon
status_line+=$(printf '%b' "${BG_OS}${FG_BLACK} ${os_icon} ${C_RESET}")
status_line+=$(printf '%b' "${FG_WHITE}${BG_DIR}${SEP_LEFT_HARD}${C_RESET}")

# セグメント2: Directory
status_line+=$(printf '%b' "${BG_DIR}${FG_WHITE} ${display_dir} ${C_RESET}")

# セグメント3: Git (条件付き)
if [ -n "$git_branch" ]; then
    if [ "$git_dirty" = true ]; then
        # Dirty (黄背景)
        status_line+=$(printf '%b' "${FG_YELLOW}${BG_GIT_DIRTY}${SEP_LEFT_HARD}${C_RESET}")
        status_line+=$(printf '%b' "${BG_GIT_DIRTY}${FG_BLACK} ${git_icon}${git_branch} ${C_RESET}")
        status_line+=$(printf '%b' "${FG_YELLOW}${SEP_RIGHT_HARD}${C_RESET}")
    else
        # Clean (緑背景)
        status_line+=$(printf '%b' "${FG_GREEN}${BG_GIT_CLEAN}${SEP_LEFT_HARD}${C_RESET}")
        status_line+=$(printf '%b' "${BG_GIT_CLEAN}${FG_BLACK} ${git_icon}${git_branch} ${C_RESET}")
        status_line+=$(printf '%b' "${FG_GREEN}${SEP_RIGHT_HARD}${C_RESET}")
    fi
else
    # Gitなし
    status_line+=$(printf '%b' "${FG_BLUE}${SEP_RIGHT_HARD}${C_RESET}")
fi

# Worktreeセグメントを追加（worktreeセッションの場合）
# worktree.name (--worktree セッション) または workspace.git_worktree (リンクされたworktree) を使用
worktree_display=""
if [ -n "$worktree_name" ]; then
    worktree_display="${worktree_name}"
    if [ -n "$worktree_branch" ]; then
        worktree_display="${worktree_name}@${worktree_branch}"
    fi
elif [ -n "$git_worktree" ]; then
    worktree_display="${git_worktree}"
fi

if [ -n "$worktree_display" ]; then
    status_line+=$(printf '%b' "${FG_MAGENTA}${BG_WORKTREE}${SEP_LEFT_HARD}${C_RESET}")
    status_line+=$(printf '%b' "${BG_WORKTREE}${FG_WHITE}  ${worktree_display} ${C_RESET}")
    status_line+=$(printf '%b' "${FG_MAGENTA}${SEP_RIGHT_HARD}${C_RESET}")
fi

# 1行目右側: Agent名
right_elements=""

if [ -n "$agent_display" ]; then
    right_elements+=$(printf '%b' " ${FG_MAGENTA}${agent_display}${C_RESET}")
fi

if [ -n "$right_elements" ]; then
    status_line+=$(printf '%b' " ${FG_GREY}│${C_RESET}${right_elements}")
fi

# 2行目: モデル / context残量 / output style / vim mode
line2=""

# Output style（デフォルト以外）
if [ -n "$output_style" ] && [ "$output_style" != "default" ]; then
    line2+=$(printf '%b' "${FG_MAGENTA}[${output_style}]${C_RESET} ")
fi

# Vim mode
if [ -n "$vim_display" ]; then
    line2+=$(printf '%b' "${vim_color}${vim_display}${C_RESET} ")
fi

# Model
if [ -n "$model" ]; then
    line2+=$(printf '%b' "${FG_BLUE}${model}${C_RESET}")
fi

# Context残量
if [ -n "$context_display" ]; then
    line2+=$(printf '%b' " ${FG_GREY}│${C_RESET} ${context_color}${context_display}${C_RESET}")
fi

# Claude Code バージョン差異（古い時だけ警告・赤）
if [ -n "$version_display" ]; then
    line2+=$(printf '%b' " ${FG_GREY}│${C_RESET} ${FG_RED}${version_display}${C_RESET}")
fi

# 2行目を改行で追加
if [ -n "$line2" ]; then
    status_line+="\n${line2}"
fi

# 出力
printf '%b' "$status_line"
