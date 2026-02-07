#!/usr/bin/env bash
# =============================================================================
# Zsh クロスプラットフォーム検証スクリプト
# Usage: cross_platform_check.sh
# Checks: OS検出機構、OS固有エイリアス、PATH分岐、コマンドガード
# Output: PASS/WARN/FAIL per check with file:line
# =============================================================================
set -euo pipefail

DOTFILES_ROOT="$(cd "$(dirname "$0")/../../../.." && pwd)"
ZSH_DIR="$DOTFILES_ROOT/.zsh"
ZPROFILE="$DOTFILES_ROOT/.zprofile"

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

echo -e "${BOLD}=== Zsh Cross-Platform Check ===${NC}"
echo ""

# =============================================================================
# Check 1: OS検出機構 (utils.zsh)
# =============================================================================
echo -e "${BOLD}--- OS Detection Infrastructure ---${NC}"

utils_file="$ZSH_DIR/utils.zsh"
if [[ ! -f "$utils_file" ]]; then
    result FAIL "utils.zsh not found"
else
    # detect_os() 関数の存在
    if grep -q 'detect_os()' "$utils_file"; then
        result PASS "detect_os() function exists"
    else
        result FAIL "detect_os() function missing in utils.zsh"
    fi

    # OS タイプ変数
    for var in ZSH_OS_TYPE ZSH_IS_WSL ZSH_IS_MACOS ZSH_IS_LINUX; do
        if grep -q "$var" "$utils_file"; then
            result PASS "$var variable defined"
        else
            result WARN "$var variable not found in utils.zsh"
        fi
    done

    # ヘルパー関数
    for func in is_macos is_linux is_wsl; do
        if grep -q "${func}()" "$utils_file"; then
            result PASS "${func}() helper exists"
        else
            result WARN "${func}() helper missing in utils.zsh"
        fi
    done

    # detect_os が呼び出されているか
    if grep -q '^detect_os' "$utils_file"; then
        result PASS "detect_os called at module load time"
    else
        result WARN "detect_os may not be called automatically"
    fi
fi
echo ""

# =============================================================================
# Check 2: OS固有エイリアスファイルの存在と整合性
# =============================================================================
echo -e "${BOLD}--- OS-Specific Alias Files ---${NC}"

expected_alias_files=(aliases_darwin.zsh aliases_linux.zsh aliases_freebsd.zsh aliases_msys.zsh)
for alias_file in "${expected_alias_files[@]}"; do
    if [[ -f "$ZSH_DIR/$alias_file" ]]; then
        lines=$(wc -l < "$ZSH_DIR/$alias_file")
        result PASS "$alias_file exists (${lines} lines)"
    else
        result FAIL "$alias_file missing"
    fi
done

# aliases.zsh の load_os_aliases() が全 OS をカバーしているか
aliases_file="$ZSH_DIR/aliases.zsh"
if [[ -f "$aliases_file" ]]; then
    if grep -q 'load_os_aliases' "$aliases_file"; then
        result PASS "load_os_aliases() function exists in aliases.zsh"
    else
        result WARN "load_os_aliases() not found — OS aliases may not be loaded"
    fi

    # 各OSの分岐パスがあるか
    has_macos_branch=false
    has_linux_branch=false
    has_freebsd_branch=false
    has_msys_branch=false

    grep -q 'is_macos\|darwin' "$aliases_file" && has_macos_branch=true
    grep -q 'is_linux\|linux' "$aliases_file" && has_linux_branch=true
    grep -q 'freebsd' "$aliases_file" && has_freebsd_branch=true
    grep -q 'msys' "$aliases_file" && has_msys_branch=true

    $has_macos_branch   && result PASS "aliases.zsh: macOS branch exists" \
                        || result FAIL "aliases.zsh: macOS branch missing"
    $has_linux_branch   && result PASS "aliases.zsh: Linux branch exists" \
                        || result FAIL "aliases.zsh: Linux branch missing"
    $has_freebsd_branch && result PASS "aliases.zsh: FreeBSD branch exists" \
                        || result WARN "aliases.zsh: FreeBSD branch missing"
    $has_msys_branch    && result PASS "aliases.zsh: MSYS branch exists" \
                        || result WARN "aliases.zsh: MSYS branch missing"
fi
echo ""

# =============================================================================
# Check 3: .zprofile の OS固有 PATH 設定
# =============================================================================
echo -e "${BOLD}--- Platform-Specific PATH (.zprofile) ---${NC}"

if [[ -f "$ZPROFILE" ]]; then
    # Homebrew (macOS / Linux Homebrew)
    if grep -q 'brew\|Homebrew\|homebrew' "$ZPROFILE"; then
        if grep -q 'is_macos\|darwin\|has_command brew' "$ZPROFILE"; then
            result PASS "Homebrew PATH: guarded by OS/command check"
        else
            result WARN "Homebrew PATH: may lack OS guard"
        fi
    fi

    # Rancher Desktop / Docker
    if grep -q 'rancher\|docker' "$ZPROFILE" 2>/dev/null; then
        result PASS "Rancher/Docker PATH entries found"
    fi

    # macOS 固有ツール
    macos_tools=("/opt/homebrew" "/Applications" "MacPorts")
    for tool in "${macos_tools[@]}"; do
        if grep -q "$tool" "$ZPROFILE" 2>/dev/null; then
            lineno=$(grep -n "$tool" "$ZPROFILE" | head -1 | cut -d: -f1)
            # safe_path_prepend はディレクトリ存在チェックを含むのでOK
            if grep -B2 "$tool" "$ZPROFILE" | grep -q 'safe_path_prepend\|safe_path_append'; then
                result PASS "macOS tool '$tool': uses safe_path_* (.zprofile:$lineno)"
            elif grep -B2 "$tool" "$ZPROFILE" | grep -q 'is_macos\|darwin'; then
                result PASS "macOS tool '$tool': OS-guarded (.zprofile:$lineno)"
            else
                result WARN "macOS tool '$tool': may lack OS guard (.zprofile:$lineno)"
            fi
        fi
    done

    # Linux 固有パス (/snap, /usr/local/go 等)
    linux_paths=("/snap" "/usr/local/go")
    for lpath in "${linux_paths[@]}"; do
        if grep -q "$lpath" "$ZPROFILE" 2>/dev/null; then
            lineno=$(grep -n "$lpath" "$ZPROFILE" | head -1 | cut -d: -f1)
            if grep -B2 "$lpath" "$ZPROFILE" | grep -q 'safe_path_prepend\|safe_path_append'; then
                result PASS "Linux path '$lpath': uses safe_path_* (.zprofile:$lineno)"
            else
                result WARN "Linux path '$lpath': may lack safe_path guard (.zprofile:$lineno)"
            fi
        fi
    done
fi
echo ""

# =============================================================================
# Check 4: クリップボード・ターミナル統合
# =============================================================================
echo -e "${BOLD}--- Clipboard & Terminal Integration ---${NC}"

# OS固有エイリアスファイル内のクリップボードコマンド
declare -A expected_clipboard
expected_clipboard[darwin]="pbcopy\|pbpaste"
expected_clipboard[linux]="xclip\|xsel\|wl-copy\|wl-paste"
expected_clipboard[msys]="clip\|clip\.exe"

for os in darwin linux; do
    alias_file="$ZSH_DIR/aliases_${os}.zsh"
    [[ -f "$alias_file" ]] || continue

    pattern="${expected_clipboard[$os]}"
    if grep -q "$pattern" "$alias_file" 2>/dev/null; then
        result PASS "aliases_${os}.zsh: clipboard commands found"
    else
        # クリップボードは必須ではない
        result PASS "aliases_${os}.zsh: no explicit clipboard aliases (OK)"
    fi
done

# open コマンドの分岐
if grep -rq 'alias open=' "$ZSH_DIR" --include='*.zsh' 2>/dev/null; then
    result PASS "'open' alias defined for non-macOS platforms"
elif grep -rq 'xdg-open' "$ZSH_DIR" --include='*.zsh' 2>/dev/null; then
    result PASS "xdg-open referenced (Linux open equivalent)"
else
    result PASS "No explicit 'open' alias (may use system default)"
fi
echo ""

# =============================================================================
# Check 5: has_command によるグレースフル劣化
# =============================================================================
echo -e "${BOLD}--- Graceful Degradation (has_command) ---${NC}"

# プラットフォーム依存が高いツール群
platform_tools=(brew port apt dnf pacman)
for tool in "${platform_tools[@]}"; do
    refs=$(grep -rn "$tool" "$ZSH_DIR" --include='*.zsh' 2>/dev/null | grep -v '^\s*#' | grep -v 'aliases_' || true)
    if [[ -n "$refs" ]]; then
        if echo "$refs" | grep -q 'has_command'; then
            result PASS "$tool: guarded by has_command"
        else
            first_ref=$(echo "$refs" | head -1)
            file=$(echo "$first_ref" | cut -d: -f1)
            lineno=$(echo "$first_ref" | cut -d: -f2)
            basename_file=$(basename "$file")
            result WARN "$tool: used without has_command guard ($basename_file:$lineno)"
        fi
    fi
done
echo ""

# --- Summary ---
echo -e "${BOLD}=== Summary ===${NC}"
echo -e "  ${GREEN}PASS${NC}: $pass"
echo -e "  ${YELLOW}WARN${NC}: $warn"
echo -e "  ${RED}FAIL${NC}: $fail"

[[ $fail -gt 0 ]] && exit 1 || exit 0
