#!/usr/bin/env bash
# =============================================================================
# deploy-config 検証スクリプト
# Usage: validate.sh
# Checks: link.sh 構文、ソースファイル存在、デプロイ済みリンク整合性、MSYS2 環境
# Output: PASS/WARN/FAIL per check
# =============================================================================
set -euo pipefail

DOTFILES_ROOT="$(cd "$(dirname "$0")/../../../.." && pwd)"
LINK_SH="$DOTFILES_ROOT/link.sh"

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

echo -e "${BOLD}=== Deploy Config Validation ===${NC}"
echo ""

# =============================================================================
# Check 1: link.sh 構文チェック
# =============================================================================
echo -e "${BOLD}--- Syntax Check ---${NC}"

if [[ -f "$LINK_SH" ]]; then
    if zsh -n "$LINK_SH" 2>/dev/null; then
        result PASS "link.sh syntax OK"
    else
        result FAIL "link.sh syntax error"
    fi
else
    result FAIL "link.sh not found at $LINK_SH"
fi
echo ""

# =============================================================================
# Check 2: ソースファイル/ディレクトリ存在確認
# =============================================================================
echo -e "${BOLD}--- Source Files & Directories ---${NC}"

# files 配列（link.sh と同期）
files=(.zshrc .zprofile .tmux.conf .aspell.conf .xinitrc .Xresources .yabairc .skhdrc .Brewfile)
for f in "${files[@]}"; do
    if [[ -e "$DOTFILES_ROOT/$f" ]]; then
        result PASS "file: $f"
    else
        result WARN "file: $f not found (platform-specific?)"
    fi
done

# dirs 配列
dirs=(.zsh .emacs.d .tmux)
for d in "${dirs[@]}"; do
    if [[ -d "$DOTFILES_ROOT/$d" ]]; then
        result PASS "dir: $d/"
    else
        result FAIL "dir: $d/ missing"
    fi
done

# config_dirs 配列
config_dirs=(gwt bat ripgrep git)
for d in "${config_dirs[@]}"; do
    if [[ -d "$DOTFILES_ROOT/.config/$d" ]]; then
        result PASS "config: .config/$d/"
    else
        result WARN "config: .config/$d/ not found"
    fi
done
echo ""

# =============================================================================
# Check 3: デプロイ済みリンクの整合性
# =============================================================================
echo -e "${BOLD}--- Deployed Symlink Integrity ---${NC}"

link_checked=0
link_ok=0
link_broken=0

# ホーム直下ファイル
for f in "${files[@]}"; do
    target="$HOME/$f"
    if [[ -L "$target" ]]; then
        ((link_checked++)) || true
        link_dest="$(readlink "$target" 2>/dev/null || true)"
        if [[ -e "$target" ]]; then
            result PASS "link: ~/$f -> $link_dest"
            ((link_ok++)) || true
        else
            result FAIL "link: ~/$f -> $link_dest (broken)"
            ((link_broken++)) || true
        fi
    fi
done

# ホーム直下ディレクトリ
for d in "${dirs[@]}"; do
    target="$HOME/$d"
    if [[ -L "$target" ]]; then
        ((link_checked++)) || true
        link_dest="$(readlink "$target" 2>/dev/null || true)"
        if [[ -e "$target" ]]; then
            result PASS "link: ~/$d -> $link_dest"
            ((link_ok++)) || true
        else
            result FAIL "link: ~/$d -> $link_dest (broken)"
            ((link_broken++)) || true
        fi
    fi
done

# XDG_CONFIG_HOME配下
config_home="${XDG_CONFIG_HOME:-$HOME/.config}"
for d in "${config_dirs[@]}"; do
    target="$config_home/$d"
    if [[ -L "$target" ]]; then
        ((link_checked++)) || true
        link_dest="$(readlink "$target" 2>/dev/null || true)"
        if [[ -e "$target" ]]; then
            result PASS "link: $target -> $link_dest"
            ((link_ok++)) || true
        else
            result FAIL "link: $target -> $link_dest (broken)"
            ((link_broken++)) || true
        fi
    fi
done

if [[ $link_checked -eq 0 ]]; then
    result WARN "No deployed symlinks found (link.sh not yet run?)"
fi
echo ""

# =============================================================================
# Check 4: MSYS2/MINGW64 環境チェック（該当環境のみ）
# =============================================================================
if [[ "${OSTYPE:-}" == msys* || "${OSTYPE:-}" == cygwin* ]]; then
    echo -e "${BOLD}--- MSYS2/MINGW64 Environment ---${NC}"

    if command -v cygpath &>/dev/null; then
        result PASS "cygpath available"
    else
        result FAIL "cygpath not found (required for XDG_CONFIG_HOME symlinks)"
    fi

    if command -v cmd &>/dev/null; then
        result PASS "cmd.exe accessible"
    else
        result FAIL "cmd.exe not found"
    fi

    # Developer Mode チェック（Windows 10+）
    if command -v reg &>/dev/null; then
        dev_mode=$(reg query "HKLM\SOFTWARE\Microsoft\Windows\CurrentVersion\AppModelUnlock" /v AllowDevelopmentWithoutDevLicense 2>/dev/null | grep -o '0x1' || true)
        if [[ -n "$dev_mode" ]]; then
            result PASS "Developer Mode enabled"
        else
            result WARN "Developer Mode not detected (mklink may require elevated privileges)"
        fi
    fi
    echo ""
fi

# --- Summary ---
echo -e "${BOLD}=== Summary ===${NC}"
echo -e "  ${GREEN}PASS${NC}: $pass"
echo -e "  ${YELLOW}WARN${NC}: $warn"
echo -e "  ${RED}FAIL${NC}: $fail"
if [[ $link_checked -gt 0 ]]; then
    echo -e "  Symlinks: $link_ok/$link_checked OK, $link_broken broken"
fi

[[ $fail -gt 0 ]] && exit 1 || exit 0
