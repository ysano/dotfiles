#!/bin/bash
# ci.sh - tmux CI validation script
# Called from .github/workflows/ci.yml
#
# 使い方:
#   bash ci.sh                         全部のテストを同時に回す（CI と同じ）
#   bash ci.sh open-path balance-panes 選んだテストだけを回す（静的チェックは常に回す）
#   bash ci.sh --serial [名前...]      1 つずつ順に回す（原因を追うとき）
#   bash ci.sh --list                  選べる名前を並べる
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"

# 名前・説明・実行するコマンド（SCRIPT_DIR から見たパス）。出力はこの順に並べる
SUITES=(
  "worktree-launch|worktree launcher tests|bash claude/test_worktree_launch.sh"
  "logging-utils|logging utils tests (set -u/-e, ADR 0002)|bash claude/test_logging_utils.sh"
  "ollama-utils|ollama utils tests (sanitize_summary_for_speech)|bash claude/test_ollama_utils.sh"
  "functions|functions tests (format_pane_label 等)|bash claude/test_functions.sh"
  "open-path|open-path tests|bash test_open_path.sh"
  "balance-panes|balance-panes tests|bash test_balance_panes.sh"
  "agents|agent workspace tests (python unittest)|python3 -m unittest discover -s agents -p test_*.py"
  "ci|ci.sh tests|bash test_ci.sh"
  "conflicts|conflict check (check_conflicts.sh)|bash ../.claude/skills/tmux-config/scripts/check_conflicts.sh"
)

suite_field() { # suite_field <行> <1=名前|2=説明|3=コマンド>
  local name desc cmd
  IFS='|' read -r name desc cmd <<< "$1"
  case $2 in 1) echo "$name" ;; 2) echo "$desc" ;; 3) echo "$cmd" ;; esac
}

usage() {
  echo "usage: bash ci.sh [--serial] [--list] [name...]" >&2
  echo "names:" >&2
  for s in "${SUITES[@]}"; do echo "  $(suite_field "$s" 1)" >&2; done
}

serial=0
selected=()
while [ $# -gt 0 ]; do
  case $1 in
    --serial) serial=1 ;;
    --list)
      for s in "${SUITES[@]}"; do printf '%-16s %s\n' "$(suite_field "$s" 1)" "$(suite_field "$s" 2)"; done
      exit 0 ;;
    -h|--help) usage; exit 0 ;;
    -*) echo "ci.sh: unknown option: $1" >&2; usage; exit 2 ;;
    *) selected+=("$1") ;;
  esac
  shift
done

# 選んだ名前を確かめてから回す（知らない名前があれば何も回さない）
runs=()
if [ ${#selected[@]} -eq 0 ]; then
  runs=("${SUITES[@]}")
else
  for name in "${selected[@]}"; do
    found=""
    for s in "${SUITES[@]}"; do [ "$(suite_field "$s" 1)" = "$name" ] && found=$s; done
    if [ -z "$found" ]; then
      echo "ci.sh: unknown suite: $name" >&2; usage; exit 2
    fi
  done
  # 指定の順でなく、決まった順に並べる
  for s in "${SUITES[@]}"; do
    for name in "${selected[@]}"; do
      if [ "$(suite_field "$s" 1)" = "$name" ]; then runs+=("$s"); break; fi
    done
  done
fi

echo "=== tmux CI Check ==="
echo ""

# 1. Syntax check: all .conf files parseable (no unmatched quotes/braces)
echo "--- Syntax Check ---"
errors=0
for conf in "$SCRIPT_DIR"/*.conf "$SCRIPT_DIR"/**/*.conf; do
  [ -f "$conf" ] || continue
  # Check for unmatched quotes (simple heuristic)
  if grep -nP "^[^#]*(?<![\\\\])\"([^\"]*$)" "$conf" 2>/dev/null; then
    echo "WARN: Possible unmatched quote in $conf"
  fi
done
echo "[PASS] Syntax check completed"
echo ""

# 2. Script executability check
echo "--- Script Permissions ---"
for script in "$SCRIPT_DIR"/scripts/*.sh; do
  [ -f "$script" ] || continue
  if [ ! -x "$script" ]; then
    echo "[FAIL] Not executable: $script"
    errors=$((errors + 1))
  else
    echo "[PASS] Executable: $(basename "$script")"
  fi
done
echo ""

# 3. Script syntax check (sh -n)
echo "--- Script Syntax ---"
for script in "$SCRIPT_DIR"/scripts/*.sh; do
  [ -f "$script" ] || continue
  if sh -n "$script" 2>/dev/null; then
    echo "[PASS] Valid syntax: $(basename "$script")"
  else
    echo "[FAIL] Syntax error: $script"
    errors=$((errors + 1))
  fi
done
echo ""

# 4. Tests: 同時に回し、出力は suite ごとにまとめて決まった順に出す
#    （各テストは自分専用の一時ディレクトリと tmux のソケットを使うので、同時でも干渉しない）
out_dir=$(mktemp -d)
trap 'rm -rf "$out_dir"' EXIT

run_suite() { # run_suite <番号> <行>: 出力・終了コード・秒数を out_dir に残す
  local start=$SECONDS rc=0
  # shellcheck disable=SC2086 # コマンドは空白で区切る（引数に空白を含まない）
  (cd "$SCRIPT_DIR" && set -f && exec $(suite_field "$2" 3)) > "$out_dir/$1.log" 2>&1 < /dev/null || rc=$?
  echo "$rc $((SECONDS - start))" > "$out_dir/$1.rc"
}

report_suite() { # report_suite <番号> <行>
  local name rc secs
  name=$(suite_field "$2" 1)
  echo "--- $name ($(suite_field "$2" 2)) ---"
  cat "$out_dir/$1.log"
  read -r rc secs < "$out_dir/$1.rc"
  if [ "$rc" = 0 ]; then
    echo "[PASS] $name (${secs}s)"
  else
    echo "[FAIL] $name (exit $rc, ${secs}s)"
    errors=$((errors + 1))
  fi
  echo ""
}

pids=()
for i in "${!runs[@]}"; do
  if [ "$serial" = 1 ]; then
    run_suite "$i" "${runs[$i]}"
    report_suite "$i" "${runs[$i]}"
  else
    run_suite "$i" "${runs[$i]}" &
    pids[i]=$!
  fi
done
if [ "$serial" = 0 ]; then
  for i in "${!runs[@]}"; do
    wait "${pids[$i]}" || true
    report_suite "$i" "${runs[$i]}"
  done
fi

echo "=== CI Complete (errors: $errors) ==="
[ "$errors" -eq 0 ] && exit 0 || exit 1
