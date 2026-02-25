#!/bin/sh
# window_name.sh - tmux window-status-format用ウィンドウ名ヘルパー
# 優先順位: project:branch > folder > pane_current_command
# Usage: window_name.sh <pane_current_path> <pane_current_command>

pane_path="${1:-.}"
pane_cmd="${2:-}"

# ブランチ名短縮: 親階層を1文字に abbreviate する
# 例: feature/auth/login-page → f/a/login-page
#     fix/bug-123 → f/bug-123
#     master → master
shorten_branch() {
  _branch="$1"
  case "$_branch" in
    */*) ;;       # スラッシュを含む場合のみ短縮
    *) printf '%s' "$_branch"; return ;;
  esac

  _last="${_branch##*/}"       # 末尾セグメント (login-page)
  _parents="${_branch%/*}"     # 親階層 (feature/auth)

  _result=""
  _rest="$_parents"
  while [ -n "$_rest" ]; do
    case "$_rest" in
      */*)
        _seg="${_rest%%/*}"
        _rest="${_rest#*/}"
        ;;
      *)
        _seg="$_rest"
        _rest=""
        ;;
    esac
    _first=$(printf '%.1s' "$_seg")
    _result="${_result}${_first}/"
  done

  printf '%s%s' "$_result" "$_last"
}

# pane_current_path が存在しなければ pane_current_command にフォールバック
if [ ! -d "$pane_path" ]; then
  printf '%s' "${pane_cmd:-unknown}"
  exit 0
fi

# git リポジトリ内かチェック
if command -v git >/dev/null 2>&1; then
  toplevel=$(cd "$pane_path" 2>/dev/null && git rev-parse --show-toplevel 2>/dev/null)
  if [ -n "$toplevel" ]; then
    project=$(basename "$toplevel")
    branch=$(cd "$pane_path" 2>/dev/null && git branch --show-current 2>/dev/null)
    if [ -n "$branch" ]; then
      short=$(shorten_branch "$branch")
      printf '%s:%s' "$project" "$short"
      exit 0
    fi
  fi
fi

# git リポジトリ外: basename of pane_current_path
dirname=$(basename "$pane_path")
if [ -n "$dirname" ]; then
  printf '%s' "$dirname"
  exit 0
fi

# フォールバック: pane_current_command
printf '%s' "${pane_cmd:-unknown}"
