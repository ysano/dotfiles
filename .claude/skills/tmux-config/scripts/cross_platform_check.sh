#!/usr/bin/env bash
# =============================================================================
# tmux クロスプラットフォーム検証スクリプト
# Usage: cross_platform_check.sh
# Checks: OS固有ファイルの存在、if-shell 分岐の網羅性、クリップボード設定
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

echo -e "${BOLD}=== tmux Cross-Platform Check ===${NC}"
echo ""

# =============================================================================
# Check 1: OS固有設定ファイルの存在
# =============================================================================
echo -e "${BOLD}--- OS-Specific Config Files ---${NC}"

expected_os_files=(wsl.conf linux.conf darwin.conf freebsd.conf)
for conf in "${expected_os_files[@]}"; do
    if [[ -f "$TMUX_DIR/os/$conf" ]]; then
        lines=$(wc -l < "$TMUX_DIR/os/$conf")
        result PASS "os/$conf exists (${lines} lines)"
    else
        result FAIL "os/$conf missing"
    fi
done
echo ""

# =============================================================================
# Check 2: .tmux.conf の if-shell 分岐が全OSをカバー
# =============================================================================
echo -e "${BOLD}--- if-shell OS Branch Coverage ---${NC}"

has_wsl=false
has_linux=false
has_darwin=false
has_freebsd=false
wsl_line=0
linux_line=0

while IFS= read -r match; do
    lineno=$(echo "$match" | cut -d: -f1)
    line=$(echo "$match" | cut -d: -f2-)

    # WSL検出: uname=Linux + WSL_DISTRO_NAME or microsoft（正の条件で参照）
    if echo "$line" | grep -q 'WSL_DISTRO_NAME\|microsoft'; then
        # source wsl.conf を含む行の方を WSL と判定
        if echo "$line" | grep -qi 'wsl'; then
            has_wsl=true
            [[ $wsl_line -eq 0 ]] && wsl_line=$lineno
        fi
    fi
    # Pure Linux: os/linux.conf を source している行
    if echo "$line" | grep -q 'os/linux\.conf\|os/linux'; then
        has_linux=true
        [[ $linux_line -eq 0 ]] && linux_line=$lineno
    fi
    # macOS
    if echo "$line" | grep -q 'Darwin'; then
        has_darwin=true
    fi
    # FreeBSD
    if echo "$line" | grep -q 'FreeBSD'; then
        has_freebsd=true
    fi
done < <(grep -n 'if-shell\|source-file' "$TMUX_CONF" 2>/dev/null || true)

$has_wsl     && result PASS "WSL detection branch exists" \
             || result FAIL "WSL detection branch missing in .tmux.conf"
$has_linux   && result PASS "Linux detection branch exists" \
             || result FAIL "Linux detection branch missing in .tmux.conf"
$has_darwin  && result PASS "macOS detection branch exists" \
             || result FAIL "macOS detection branch missing in .tmux.conf"
$has_freebsd && result PASS "FreeBSD detection branch exists" \
             || result FAIL "FreeBSD detection branch missing in .tmux.conf"

# WSL が Linux より先に検出されているか確認
if [[ $wsl_line -gt 0 && $linux_line -gt 0 ]]; then
    if [[ $wsl_line -lt $linux_line ]]; then
        result PASS "WSL detected before Linux (line $wsl_line < $linux_line)"
    else
        result FAIL "WSL must be detected BEFORE Linux (WSL:$wsl_line, Linux:$linux_line)"
    fi
fi
echo ""

# =============================================================================
# Check 3: クリップボード設定の網羅性
# =============================================================================
echo -e "${BOLD}--- Clipboard Configuration ---${NC}"

declare -A clipboard_os
for conf in wsl.conf linux.conf darwin.conf freebsd.conf; do
    os_name="${conf%.conf}"
    conf_path="$TMUX_DIR/os/$conf"
    [[ -f "$conf_path" ]] || continue

    if grep -q 'copy-pipe\|pbcopy\|clip\.exe\|xclip\|xsel\|wl-copy' "$conf_path" 2>/dev/null; then
        clipboard_os[$os_name]=true
        result PASS "$os_name: clipboard integration configured"
    else
        clipboard_os[$os_name]=false
        result WARN "$os_name: no clipboard integration found in os/$conf"
    fi
done
echo ""

# =============================================================================
# Check 4: source-file パスの存在確認
# =============================================================================
echo -e "${BOLD}--- Source File Paths ---${NC}"

while IFS= read -r match; do
    lineno=$(echo "$match" | cut -d: -f1)
    line=$(echo "$match" | cut -d: -f2-)

    # コメント行を除外
    [[ "$line" =~ ^[[:space:]]*# ]] && continue

    # パスを抽出（~/.tmux/ or 相対パス）
    path=$(echo "$line" | sed -n "s/.*source-file ['\"]\\{0,1\\}\([^'\"]*\\)['\"]\\{0,1\\}.*/\\1/p")
    [[ -z "$path" ]] && continue

    # ~ を展開してチェック（dotfiles内を想定）
    check_path="${path/\~\/.tmux/$TMUX_DIR}"
    check_path="${check_path/\~\/.tmux.conf/$TMUX_CONF}"

    if [[ -f "$check_path" ]]; then
        result PASS "source-file: $path (line $lineno)"
    else
        # if-shell 内の source は条件付きなので WARN
        if grep -q "if-shell" <<< "$(sed -n "${lineno}p" "$TMUX_CONF" 2>/dev/null)"; then
            result PASS "source-file: $path (conditional, line $lineno)"
        else
            result WARN "source-file: $path not found (line $lineno)"
        fi
    fi
done < <(grep -n 'source-file' "$TMUX_CONF" 2>/dev/null || true)
echo ""

# --- Summary ---
echo -e "${BOLD}=== Summary ===${NC}"
echo -e "  ${GREEN}PASS${NC}: $pass"
echo -e "  ${YELLOW}WARN${NC}: $warn"
echo -e "  ${RED}FAIL${NC}: $fail"

[[ $fail -gt 0 ]] && exit 1 || exit 0
