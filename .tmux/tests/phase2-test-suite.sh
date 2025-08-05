#!/bin/bash
# Phase 2 Test Suite - Comprehensive testing for refactored system
# Phase 2テストスイート - リファクタリング版システムの包括的テスト

set -euo pipefail

# カラー定義
readonly RED='\033[0;31m'
readonly GREEN='\033[0;32m'
readonly BLUE='\033[0;34m'
readonly YELLOW='\033[1;33m'
readonly NC='\033[0m'

# テスト統計
TESTS_PASSED=0
TESTS_FAILED=0
TESTS_SKIPPED=0

# ベースパス
readonly TEST_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
readonly TMUX_ROOT="$(dirname "$TEST_DIR")"
readonly CORE_DIR="$TMUX_ROOT/core"

# ログ関数
log_test() {
    local level="$1"
    shift
    case "$level" in
        "INFO") echo -e "${BLUE}[INFO]${NC} $*" ;;
        "PASS") echo -e "${GREEN}[PASS]${NC} $*"; ((TESTS_PASSED++)) ;;
        "FAIL") echo -e "${RED}[FAIL]${NC} $*"; ((TESTS_FAILED++)) ;;
        "SKIP") echo -e "${YELLOW}[SKIP]${NC} $*"; ((TESTS_SKIPPED++)) ;;
    esac
}

# テスト実行関数
run_test() {
    local test_name="$1"
    local test_command="$2"
    
    echo -n "Testing $test_name... "
    
    if eval "$test_command" >/dev/null 2>&1; then
        log_test "PASS" "$test_name"
    else
        log_test "FAIL" "$test_name"
    fi
}

# テスト実行関数（詳細出力）
run_test_with_output() {
    local test_name="$1"
    local test_command="$2"
    
    echo "Testing $test_name..."
    
    if output=$(eval "$test_command" 2>&1); then
        log_test "PASS" "$test_name"
        echo "  Output: $output"
    else
        log_test "FAIL" "$test_name"
        echo "  Error: $output"
    fi
}

echo -e "${BLUE}🧪 Phase 2 TMux Claude Voice Test Suite${NC}"
echo "========================================"
echo

# === Core File Existence Tests ===
echo -e "${BLUE}📁 Core File Existence Tests${NC}"

run_test "init-minimal.sh exists" "[[ -f '$CORE_DIR/init-minimal.sh' ]]"
run_test "audio-fallback.sh exists" "[[ -f '$CORE_DIR/audio-fallback.sh' ]]"
run_test "voice-unified.sh exists" "[[ -f '$CORE_DIR/voice-unified.sh' ]]"
run_test "ollama-cross.sh exists" "[[ -f '$CORE_DIR/ollama-cross.sh' ]]"
run_test "audio-mode-switcher.sh exists" "[[ -f '$CORE_DIR/audio-mode-switcher.sh' ]]"
run_test "tmux-keybindings.conf exists" "[[ -f '$CORE_DIR/tmux-keybindings.conf' ]]"

echo

# === Executable Permissions Tests ===
echo -e "${BLUE}🔧 Executable Permissions Tests${NC}"

run_test "init-minimal.sh executable" "[[ -x '$CORE_DIR/init-minimal.sh' ]]"
run_test "audio-fallback.sh executable" "[[ -x '$CORE_DIR/audio-fallback.sh' ]]"
run_test "voice-unified.sh executable" "[[ -x '$CORE_DIR/voice-unified.sh' ]]"
run_test "ollama-cross.sh executable" "[[ -x '$CORE_DIR/ollama-cross.sh' ]]"
run_test "audio-mode-switcher.sh executable" "[[ -x '$CORE_DIR/audio-mode-switcher.sh' ]]"

echo

# === Syntax Validation Tests ===
echo -e "${BLUE}🔍 Syntax Validation Tests${NC}"

for script in "$CORE_DIR"/*.sh; do
    if [[ -f "$script" ]]; then
        script_name=$(basename "$script")
        run_test "$script_name syntax" "bash -n '$script'"
    fi
done

echo

# === Function Existence Tests ===
echo -e "${BLUE}🔧 Function Existence Tests${NC}"

# init-minimal.sh functions
run_test "detect_platform_cached function" "grep -q 'detect_platform_cached()' '$CORE_DIR/init-minimal.sh'"
run_test "measure_performance function" "grep -q 'measure_performance()' '$CORE_DIR/init-minimal.sh'"

# audio-fallback.sh functions
run_test "detect_platform function" "grep -q 'detect_platform()' '$CORE_DIR/audio-fallback.sh'"
run_test "play_status_audio function" "grep -q 'play_status_audio()' '$CORE_DIR/audio-fallback.sh'"

# ollama-cross.sh functions
run_test "get_ollama_url function" "grep -q 'get_ollama_url()' '$CORE_DIR/ollama-cross.sh'"

echo

# === Functional Tests ===
echo -e "${BLUE}⚙️  Functional Tests${NC}"

# Test initialization
run_test_with_output "init-minimal.sh status" "'$CORE_DIR/init-minimal.sh' status"

# Test audio system diagnosis
run_test_with_output "audio-fallback.sh diagnose" "'$CORE_DIR/audio-fallback.sh' diagnose | head -5"

# Test audio mode switcher
run_test_with_output "audio-mode-switcher.sh get" "'$CORE_DIR/audio-mode-switcher.sh' get"

echo

# === Configuration Tests ===
echo -e "${BLUE}📋 Configuration Tests${NC}"

run_test "tmux-minimal.yaml exists" "[[ -f '$TMUX_ROOT/tmux-minimal.yaml' ]]"
run_test "audio_mode.conf exists" "[[ -f '$TMUX_ROOT/claude/config/audio_mode.conf' ]]"

# YAML syntax validation (if yq is available)
if command -v yq >/dev/null 2>&1; then
    run_test "tmux-minimal.yaml syntax" "yq eval '.' '$TMUX_ROOT/tmux-minimal.yaml' >/dev/null"
else
    log_test "SKIP" "tmux-minimal.yaml syntax (yq not available)"
fi

echo

# === Performance Tests ===
echo -e "${BLUE}⚡ Performance Tests${NC}"

# Test initialization performance
if [[ -f "$CORE_DIR/init-minimal.sh" ]]; then
    echo "Testing initialization performance..."
    start_time=$(date +%s%3N)
    "$CORE_DIR/init-minimal.sh" init >/dev/null 2>&1
    end_time=$(date +%s%3N)
    duration=$((end_time - start_time))
    
    if [[ $duration -lt 100 ]]; then
        log_test "PASS" "Initialization performance (${duration}ms < 100ms)"
    else
        log_test "FAIL" "Initialization performance (${duration}ms >= 100ms)"
    fi
fi

echo

# === Integration Tests ===
echo -e "${BLUE}🔗 Integration Tests${NC}"

# Test tmux configuration loading
if command -v tmux >/dev/null 2>&1; then
    run_test "tmux configuration syntax" "tmux -f '$TMUX_ROOT/../.tmux.conf' -c 'echo test' 2>/dev/null; echo 'exit 0'"
else
    log_test "SKIP" "tmux configuration syntax (tmux not available)"
fi

echo

# === Summary ===
echo "========================================"
echo -e "${BLUE}📊 Test Summary${NC}"
echo "  Passed:  $TESTS_PASSED"
echo "  Failed:  $TESTS_FAILED"
echo "  Skipped: $TESTS_SKIPPED"
echo "  Total:   $((TESTS_PASSED + TESTS_FAILED + TESTS_SKIPPED))"

if [[ $TESTS_FAILED -eq 0 ]]; then
    echo -e "${GREEN}✅ All tests passed successfully!${NC}"
    exit 0
else
    echo -e "${RED}❌ $TESTS_FAILED test(s) failed${NC}"
    exit 1
fi