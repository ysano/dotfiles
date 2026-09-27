#!/bin/bash

# Claude Code ステータスライン設定
# Powerlevel10k Rainbow テーマスタイル
# 参考: ~/.p10k.zsh (nerdfont-v3 + powerline, rainbow, 2 lines, round heads/tails)

# JSON入力を取得
input=$(cat)

# jq の有無を確認（未導入時は新規追加分だけグレースフルに非表示にする。
# 既存の抽出は元々 jq 前提のため対象外）
have_jq=false
command -v jq >/dev/null 2>&1 && have_jq=true

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
effort_level=$(echo "$input" | jq -r '.effort.level // ""')

# セッション（5 時間枠）/ 週次の利用率とリセット時刻（Unix epoch 秒）。
# rate_limits.{five_hour,seven_day} はサブスクライバーで最初の API 応答後のみ
# 存在するオプショナルフィールド。無ければ非表示。
session_used_pct=""; session_resets_at=""
weekly_used_pct=""; weekly_resets_at=""
if $have_jq; then
    # 区切りはタブ等の空白だと空欄が詰められて列がずれるため、非空白の "|" を使う
    IFS='|' read -r session_used_pct session_resets_at weekly_used_pct weekly_resets_at < <(
        echo "$input" | jq -r '[.rate_limits.five_hour.used_percentage, .rate_limits.five_hour.resets_at,
            .rate_limits.seven_day.used_percentage, .rate_limits.seven_day.resets_at]
            | map(if . == null then "" else tostring end) | join("|")')
fi

# 使用中の Claude アカウント判別（メールアドレスのドメイン）。
# CLAUDE_CONFIG_DIR ごとにログインアカウントが異なる想定。
# .claude.json の oauthAccount.emailAddress を読む。.claude.json の場所は
# CLAUDE_CONFIG_DIR 指定時は "$CLAUDE_CONFIG_DIR/.claude.json"、未指定時は
# "$HOME/.claude.json"（~/.claude/ 配下ではない）。
# 数千行規模のため結果を 6 時間キャッシュし、期限切れはバックグラウンド更新
# （cc_version チェックと同じ方式）。キャッシュ未作成の初回のみ同期取得。
acct_config_dir="${CLAUDE_CONFIG_DIR:-$HOME/.claude}"
if [ -n "$CLAUDE_CONFIG_DIR" ]; then
    acct_json="$CLAUDE_CONFIG_DIR/.claude.json"
else
    acct_json="$HOME/.claude.json"
fi
account_email=""
if $have_jq && [ -f "$acct_json" ]; then
    _acct_cache="$acct_config_dir/.statusline-account-email-cache"
    _acct_jq='.oauthAccount.emailAddress // empty'
    if [ ! -f "$_acct_cache" ]; then
        ( jq -r "$_acct_jq" "$acct_json" > "$_acct_cache" ) 2>/dev/null
    elif [ -z "$(find "$_acct_cache" -mmin -360 2>/dev/null)" ]; then
        ( jq -r "$_acct_jq" "$acct_json" 2>/dev/null > "$_acct_cache.tmp" \
            && mv "$_acct_cache.tmp" "$_acct_cache" ) >/dev/null 2>&1 &
    fi
    account_email=$(cat "$_acct_cache" 2>/dev/null)
fi
account_domain=""
if [ -n "$account_email" ]; then
    account_domain="${account_email#*@}"
fi

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
    # ローカルが公開最新版より「古い時だけ」警告（新しい/同一なら何も出さない）
    # 文字列比較では新旧を区別できないため sort -V で古い方を判定する
    if [ -n "$_cc_latest" ] && [ "$cc_version" != "$_cc_latest" ]; then
        _cc_older=$(printf '%s\n%s\n' "$cc_version" "$_cc_latest" | sort -V | head -n1)
        if [ "$_cc_older" = "$cc_version" ]; then
            version_display="⚠v${cc_version}->${_cc_latest}"
        fi
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
        context_color="${FG_GREEN}"
    fi
    context_display="${context_icon}${remaining_int}%"
fi

# 利用枠の残量表示: rate_limit_display <ラベル> <used_percentage> <resets_at> <枠秒数> <予測開始の経過率>
# 例: "3d/7d:70% ⚠41h"（<リセットまでの残り時間>/<枠>:<残り%>）。リセット時刻が無ければ "7d:70%"。
# 残り時間の書式は format_duration を参照。used_percentage が空なら何も出さない。
# 色はペース（枠開始からの平均消費ペースが続いた場合の枯渇予測・暦時間）で決める:
#   緑 = リセットまで持つ / 黄 = 残り時間の後半で枯渇 / 赤 = 残り時間の前半で枯渇
#   枯渇する場合は "⚠<枯渇までの時間>" を付ける（数字と同色）。
# 例外: 残り 5% 未満は常に赤。枠序盤（経過率 < <予測開始の経過率>）やリセット時刻が
# 無くペースを判定できない間は、残り% の閾値（<20 赤 / <50 黄 / それ以上緑）で決める。
rate_limit_display() {
    local label=$1 used=$2 resets_at=$3 window=$4 min_elapsed=$5 remaining color text
    local left_s="" pace="" tte_s="" warn=""
    [ -z "$used" ] && return
    remaining=$(awk -v u="$used" 'BEGIN{v=100-u; if (v<0) v=0; printf "%d", v}')
    if [ -n "$resets_at" ]; then
        left_s=$(awk -v r="$resets_at" -v n="$(date +%s)" 'BEGIN{s=r-n; if (s<0) s=0; printf "%d", s}')
        # ペース判定: "ok"（持つ）/ "<枯渇までの秒数>" / 空（判定不能＝序盤）
        pace=$(awk -v u="$used" -v l="$left_s" -v w="$window" -v m="$min_elapsed" \
            'BEGIN{e=w-l; if (u>=100) { print 0; exit }
                   if (u<=0) { print "ok"; exit }
                   if (e<=0 || e/w<m) exit;
                   t=(100-u)/(u/e); if (t<l) printf "%d", t; else print "ok"}')
    fi
    if [ "$remaining" -lt 5 ]; then
        color="${FG_RED}"
    elif [ "$pace" = "ok" ]; then
        color="${FG_GREEN}"
    elif [ -n "$pace" ]; then
        tte_s=$pace
        if [ "$tte_s" -le $((left_s / 2)) ]; then
            color="${FG_RED}"
        else
            color="${FG_YELLOW}"
        fi
    elif [ "$remaining" -lt 20 ]; then
        color="${FG_RED}"
    elif [ "$remaining" -lt 50 ]; then
        color="${FG_YELLOW}"
    else
        color="${FG_GREEN}"
    fi
    if [ -n "$left_s" ]; then
        text="$(format_duration "$left_s")/${label}:${remaining}%"
    else
        text="${label}:${remaining}%"
    fi
    if [ -n "$tte_s" ] && [ "$remaining" -gt 0 ]; then
        text+=" ⚠$(format_duration "$tte_s" floor)"
    fi
    printf '%s' "${color}${text}${C_RESET}"
}
# 秒数を "Nd"（2 日超）/ "NNh"（2 時間超〜2 日）/ "NNm"（2 時間以下）に整形。
# 2 日を超えるのは実質 7d 枠のみ（5h 枠は常に h/m 表示）。
# 第 2 引数 floor で切り捨て（枯渇予測を早めに見せる安全側の丸め）、既定は切り上げ。
format_duration() {
    awk -v s="$1" -v fl="${2:-}" 'function r(u){ q=int(s/u); if (fl!="floor" && s%u>0) q++; return q }
        BEGIN{if (s<=7200) printf "%dm", r(60)
              else if (s<=172800) printf "%dh", r(3600)
              else printf "%dd", r(86400)}'
}
session_display=$(rate_limit_display "5h" "$session_used_pct" "$session_resets_at" 18000 0.2)
weekly_display=$(rate_limit_display "7d" "$weekly_used_pct" "$weekly_resets_at" 604800 0.05)

# 使用中アカウントの表示（メールドメインで判別）
account_display=""
if [ -n "$account_domain" ]; then
    account_display="@${account_domain}"
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

# Effort level（モデルの隣に表示）
if [ -n "$effort_level" ]; then
    line2+=$(printf '%b' " ${FG_GREY}(${effort_level})${C_RESET}")
fi

# 使用中アカウント（メールドメインで判別）
if [ -n "$account_display" ]; then
    line2+=$(printf '%b' " ${FG_GREY}│${C_RESET} ${FG_CYAN}${account_display}${C_RESET}")
fi

# Context残量
if [ -n "$context_display" ]; then
    line2+=$(printf '%b' " ${FG_GREY}│${C_RESET} ${context_color}${context_display}${C_RESET}")
fi

# セッション（5 時間枠）/ 週次の利用残量
for _rl in "$session_display" "$weekly_display"; do
    [ -n "$_rl" ] && line2+=$(printf '%b' " ${FG_GREY}│${C_RESET} ${_rl}")
done

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
