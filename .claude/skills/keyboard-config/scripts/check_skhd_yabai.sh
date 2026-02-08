#!/usr/bin/env bash
# =============================================================================
# skhd/yabai 設定検証スクリプト
# Usage: check_skhd_yabai.sh
# Checks: skhd バインド重複、yabairc 構文、conflict marker 確認
# Output: PASS/WARN/FAIL per check with details
# =============================================================================
set -euo pipefail

DOTFILES_ROOT="$(cd "$(dirname "$0")/../../../.." && pwd)"
SKHDRC="$DOTFILES_ROOT/.skhdrc"
YABAIRC="$DOTFILES_ROOT/.yabairc"

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

echo -e "${BOLD}=== skhd/yabai Config Check ===${NC}"
echo ""

# =============================================================================
# Check 1: skhd 有効バインドの重複検出
# =============================================================================
echo -e "${BOLD}--- skhd Binding Duplicates ---${NC}"

if [[ ! -f "$SKHDRC" ]]; then
    result WARN ".skhdrc not found — skipping"
else
    declare -A skhd_bindings
    declare -a skhd_conflicts=()
    lineno=0

    while IFS= read -r line; do
        ((lineno++)) || true

        # コメント行・空行・コメントアウトされたバインドをスキップ
        stripped=$(echo "$line" | sed 's/^[[:space:]]*//')
        [[ -z "$stripped" ]] && continue
        [[ "$stripped" == \#* ]] && continue

        # バインド行: "modifier - key : command" パターン
        if echo "$stripped" | grep -qE '^[a-z].*:.*yabai|^[a-z].*:.*skhd'; then
            # ":" の左側がキーバインド部分
            binding=$(echo "$stripped" | sed 's/[[:space:]]*:.*//' | sed 's/[[:space:]]*$//')
            # 正規化: スペースを統一
            normalized=$(echo "$binding" | tr -s ' ')

            if [[ -n "${skhd_bindings[$normalized]:-}" ]]; then
                existing_line="${skhd_bindings[$normalized]}"
                skhd_conflicts+=("$normalized (line $existing_line vs line $lineno)")
            else
                skhd_bindings[$normalized]="$lineno"
            fi
        fi
    done < "$SKHDRC"

    if [[ ${#skhd_conflicts[@]} -eq 0 ]]; then
        result PASS "No duplicate bindings (${#skhd_bindings[@]} active bindings checked)"
    else
        for conflict in "${skhd_conflicts[@]}"; do
            result FAIL "Duplicate binding: $conflict"
        done
    fi
fi
echo ""

# =============================================================================
# Check 2: .yabairc 構文チェック
# =============================================================================
echo -e "${BOLD}--- yabairc Syntax ---${NC}"

if [[ ! -f "$YABAIRC" ]]; then
    result WARN ".yabairc not found — skipping"
else
    if bash -n "$YABAIRC" 2>/dev/null; then
        result PASS ".yabairc: bash syntax OK"
    else
        err=$(bash -n "$YABAIRC" 2>&1 || true)
        result FAIL ".yabairc: syntax error — $err"
    fi
fi
echo ""

# =============================================================================
# Check 3: conflict karabiner コメントの存在確認
# =============================================================================
echo -e "${BOLD}--- Karabiner Conflict Markers ---${NC}"

if [[ ! -f "$SKHDRC" ]]; then
    result WARN ".skhdrc not found — skipping"
else
    conflict_count=0
    while IFS= read -r match; do
        ((conflict_count++)) || true
        lineno=$(echo "$match" | cut -d: -f1)
        line=$(echo "$match" | cut -d: -f2-)
        # コメントアウトされたバインド行から実際のバインドを抽出
        binding=$(echo "$line" | sed 's/^[[:space:]]*#[[:space:]]*//' | sed 's/[[:space:]]*#.*conflict.*//')
        echo "       line $lineno: $binding (disabled due to Karabiner conflict)"
    done < <(grep -n '# conflict karabiner' "$SKHDRC" 2>/dev/null || true)

    if [[ $conflict_count -gt 0 ]]; then
        result WARN "$conflict_count binding(s) disabled due to Karabiner conflicts (intentional)"
    else
        result PASS "No Karabiner conflict markers found"
    fi
fi
echo ""

# --- Summary ---
echo -e "${BOLD}=== Summary ===${NC}"
echo -e "  ${GREEN}PASS${NC}: $pass"
echo -e "  ${YELLOW}WARN${NC}: $warn"
echo -e "  ${RED}FAIL${NC}: $fail"

[[ $fail -gt 0 ]] && exit 1 || exit 0
