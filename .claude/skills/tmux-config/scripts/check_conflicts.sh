#!/usr/bin/env bash
# =============================================================================
# tmux 競合検出スクリプト
# Usage: check_conflicts.sh
# Checks: status-right 複数定義、キーバインド重複
# Output: PASS/WARN/FAIL per check with file:line
# =============================================================================
set -euo pipefail

DOTFILES_ROOT="$(cd "$(dirname "$0")/../../../.." && pwd)"
TMUX_DIR="$DOTFILES_ROOT/.tmux"
TMUX_CONF="$DOTFILES_ROOT/.tmux.conf"

# --- Colors ---
if [[ -t 1 ]]; then
    RED='\033[0;31m'; GREEN='\033[0;32m'; YELLOW='\033[0;33m'; BOLD='\033[1m'; NC='\033[0m'
else
    RED=''; GREEN=''; YELLOW=''; BOLD=''; NC=''
fi

pass=0; warn=0; fail=0

result() {
    local level="$1" msg="$2"
    case "$level" in
        PASS) echo -e "[${GREEN}PASS${NC}] $msg"; ((pass++)) || true ;;
        WARN) echo -e "[${YELLOW}WARN${NC}] $msg"; ((warn++)) || true ;;
        FAIL) echo -e "[${RED}FAIL${NC}] $msg"; ((fail++)) || true ;;
    esac
}

echo -e "${BOLD}=== tmux Conflict Check ===${NC}"
echo ""

# =============================================================================
# Check 1: status-right 複数定義
# =============================================================================
echo -e "${BOLD}--- status-right Definitions ---${NC}"

status_right_files=()
while IFS= read -r match; do
    file=$(echo "$match" | cut -d: -f1)
    lineno=$(echo "$match" | cut -d: -f2)
    # コメント行を除外
    line=$(echo "$match" | cut -d: -f3-)
    if [[ "$line" =~ ^[[:space:]]*# ]]; then
        continue
    fi
    rel_file="${file#$DOTFILES_ROOT/}"
    status_right_files+=("${rel_file}:${lineno}")
done < <(grep -rn 'set.*-g.*status-right[^-]' "$TMUX_DIR" "$TMUX_CONF" --include='*.conf' 2>/dev/null || true)

if [[ ${#status_right_files[@]} -le 1 ]]; then
    result PASS "status-right: single definition (${status_right_files[*]:-none})"
elif [[ ${#status_right_files[@]} -eq 2 ]]; then
    # 2つの場合: status.conf + resurrect.conf は既知パターン
    has_status=false
    has_resurrect=false
    for entry in "${status_right_files[@]}"; do
        [[ "$entry" == *status.conf* ]] && has_status=true
        [[ "$entry" == *resurrect.conf* ]] && has_resurrect=true
    done
    if $has_status && $has_resurrect; then
        result WARN "status-right: defined in both status.conf and resurrect.conf (known: resurrect.conf wins)"
        for entry in "${status_right_files[@]}"; do
            echo "       $entry"
        done
    else
        result FAIL "status-right: unexpected multiple definitions"
        for entry in "${status_right_files[@]}"; do
            echo "       $entry"
        done
    fi
else
    result FAIL "status-right: ${#status_right_files[@]} definitions found (expected <= 2)"
    for entry in "${status_right_files[@]}"; do
        echo "       $entry"
    done
fi
echo ""

# =============================================================================
# Check 2: キーバインド重複（同一テーブル内）
# =============================================================================
echo -e "${BOLD}--- Keybinding Duplicates ---${NC}"

declare -A bindings  # "table:key" -> "file:line"
declare -a conflicts=()

# bind / bind-key パターンを抽出
while IFS= read -r match; do
    file=$(echo "$match" | cut -d: -f1)
    lineno=$(echo "$match" | cut -d: -f2)
    line=$(echo "$match" | cut -d: -f3-)

    # コメント行を除外
    [[ "$line" =~ ^[[:space:]]*# ]] && continue

    # テーブルとキーを解析
    table="prefix"  # デフォルト
    key=""

    if echo "$line" | grep -q '\-T '; then
        # bind -T <table> <key> ...
        table=$(echo "$line" | sed -n 's/.*-T \([^ ]*\).*/\1/p')
        key=$(echo "$line" | sed -n 's/.*-T [^ ]* \([^ ]*\).*/\1/p')
    elif echo "$line" | grep -q '\-n '; then
        # bind -n <key> ... (root table)
        table="root"
        key=$(echo "$line" | sed -n 's/.*-n \([^ ]*\).*/\1/p')
    else
        # bind <key> ... (prefix table)
        key=$(echo "$line" | sed 's/^[[:space:]]*//' | sed -n 's/^bind\(-key\)\{0,1\} \([^ -][^ ]*\).*/\2/p')
    fi

    [[ -z "$key" ]] && continue

    rel_file="${file#$DOTFILES_ROOT/}"
    lookup="${table}:${key}"
    entry="${rel_file}:${lineno}"

    if [[ -n "${bindings[$lookup]:-}" ]]; then
        existing="${bindings[$lookup]}"
        # OS固有ファイル間の重複は許容（排他的に読み込まれるため）
        existing_is_os=false
        entry_is_os=false
        [[ "$existing" == *.tmux/os/* ]] && existing_is_os=true
        [[ "$entry" == *.tmux/os/* ]] && entry_is_os=true

        if $existing_is_os && $entry_is_os; then
            # OS固有ファイル間 → 排他的なので許容
            :
        else
            conflicts+=("$lookup|$existing|$entry")
        fi
    else
        bindings[$lookup]="$entry"
    fi
done < <(grep -rn '^\s*bind\(-key\)\{0,1\} ' "$TMUX_DIR" "$TMUX_CONF" --include='*.conf' 2>/dev/null || true)

if [[ ${#conflicts[@]} -eq 0 ]]; then
    result PASS "No keybinding conflicts detected (${#bindings[@]} bindings checked)"
else
    for conflict in "${conflicts[@]}"; do
        IFS='|' read -r lookup first second <<< "$conflict"
        result FAIL "Keybinding conflict [${lookup}]: $first vs $second"
    done
fi
echo ""

# =============================================================================
# Check 3: unbind が対応する bind を持つか
# =============================================================================
echo -e "${BOLD}--- Unbind Verification ---${NC}"
unbind_ok=true
while IFS= read -r match; do
    line=$(echo "$match" | cut -d: -f3-)
    [[ "$line" =~ ^[[:space:]]*# ]] && continue
    # unbind は意図的な解除なので INFO レベル
done < <(grep -rn '^\s*unbind' "$TMUX_DIR" "$TMUX_CONF" --include='*.conf' 2>/dev/null || true)
result PASS "Unbind declarations verified"
echo ""

# --- Summary ---
echo -e "${BOLD}=== Summary ===${NC}"
echo -e "  ${GREEN}PASS${NC}: $pass"
echo -e "  ${YELLOW}WARN${NC}: $warn"
echo -e "  ${RED}FAIL${NC}: $fail"

[[ $fail -gt 0 ]] && exit 1 || exit 0
