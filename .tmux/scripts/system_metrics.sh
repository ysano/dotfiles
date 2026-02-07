#!/bin/sh
# system_metrics.sh - tmux status-right用システムメトリクスヘルパー
# OS自動検出し、各環境に適したメトリクスを出力する
# tmuxの #() から呼び出すことで、format string内のクォーティング問題を回避

case "$(uname -s)" in
  Darwin)
    if command -v tmux-mem-cpu-load >/dev/null 2>&1; then
      tmux-mem-cpu-load --interval 2
    else
      sysctl -n vm.loadavg 2>/dev/null | tr -d '{}'
    fi
    ;;
  Linux)
    load=$(cut -d ' ' -f 1-3 /proc/loadavg 2>/dev/null)
    mem=$(free 2>/dev/null | awk 'NR==2{printf "%.0f%%", $3*100/$2}')
    if [ -n "$load" ] && [ -n "$mem" ]; then
      printf '%s Mem:%s' "$load" "$mem"
    elif [ -n "$load" ]; then
      printf '%s' "$load"
    fi
    ;;
  FreeBSD)
    sysctl -n vm.loadavg 2>/dev/null | tr -d '{}'
    ;;
  *)
    printf 'N/A'
    ;;
esac
