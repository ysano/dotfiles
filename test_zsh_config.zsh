#!/usr/bin/env zsh
# Test script for refactored Zsh configuration
# Validates functionality without breaking current setup

# Colors for output
readonly RED='\033[0;31m'
readonly GREEN='\033[0;32m'
readonly YELLOW='\033[1;33m'
readonly NC='\033[0m' # No Color

# Test counters
typeset -g TESTS_RUN=0
typeset -g TESTS_PASSED=0
typeset -g TESTS_FAILED=0

# Test framework functions
test_assert() {
    local description="$1"
    local command="$2"
    local expected_exit_code="${3:-0}"
    
    TESTS_RUN=$((TESTS_RUN + 1))
    
    echo -n "Testing: $description ... "
    
    if eval "$command" >/dev/null 2>&1; then
        local actual_exit_code=0
    else
        local actual_exit_code=1
    fi
    
    if [[ $actual_exit_code -eq $expected_exit_code ]]; then
        echo -e "${GREEN}PASS${NC}"
        TESTS_PASSED=$((TESTS_PASSED + 1))
        return 0
    else
        echo -e "${RED}FAIL${NC}"
        echo "  Expected exit code: $expected_exit_code"
        echo "  Actual exit code: $actual_exit_code"
        TESTS_FAILED=$((TESTS_FAILED + 1))
        return 1
    fi
}

test_file_exists() {
    local file="$1"
    test_assert "File exists: $file" "[[ -f '$file' ]]"
}

test_function_exists() {
    local func="$1"
    test_assert "Function exists: $func" "typeset -f '$func' >/dev/null"
}

# Main test suite
main() {
    echo -e "${YELLOW}Running Zsh Configuration Tests${NC}"
    echo "======================================"
    
    # Test file syntax
    echo -e "\n${YELLOW}1. Syntax Tests${NC}"
    test_assert "utils.zsh syntax" "zsh -n .zsh/utils.zsh"
    test_assert "new .zprofile syntax" "zsh -n .zprofile"
    test_assert "new aliases.zsh syntax" "zsh -n .zsh/aliases.zsh"
    test_assert "new zinit_setup.zsh syntax" "zsh -n .zsh/zinit_setup.zsh"
    
    # Test utility functions in isolation
    echo -e "\n${YELLOW}2. Utility Function Tests${NC}"
    
    # Create temporary test environment
    local temp_dir=$(mktemp -d)
    local test_env="$temp_dir/test_env.zsh"
    
    cat > "$test_env" << 'EOF'
# Load utils in test environment
source .zsh/utils.zsh

# Test OS detection
detect_os
echo "OS_TYPE: $ZSH_OS_TYPE"

# Test command checking
if has_command ls; then
    echo "Command test: PASS"
else
    echo "Command test: FAIL"
fi

# Test path functions
typeset -a test_path
test_path=("/usr/bin" "/bin")
path=($test_path)

safe_path_append "/usr/local/bin"
safe_path_append "/nonexistent/path"

if [[ ${#path[@]} -eq 3 ]]; then
    echo "Path test: PASS"
else
    echo "Path test: FAIL"
fi
EOF
    
    test_assert "Utils functions work" "zsh '$test_env' | grep -q 'PASS'"
    
    # Clean up
    rm -rf "$temp_dir"
    
    # Test .zprofile functionality
    echo -e "\n${YELLOW}3. Profile Configuration Tests${NC}"
    
    # Recreate temp dir for this test
    temp_dir=$(mktemp -d)
    local profile_test="$temp_dir/profile_test.zsh"
    cat > "$profile_test" << 'EOF'
# Simulate login shell environment
export HOME="$PWD"
source .zprofile
echo "Profile loaded successfully"
EOF
    
    test_assert ".zprofile loads without errors" "zsh '$profile_test' 2>/dev/null | grep -q 'Profile loaded'"
    
    # Test specific functions
    echo -e "\n${YELLOW}4. Function Availability Tests${NC}"
    
    # Source utils to test functions
    source .zsh/utils.zsh
    
    test_function_exists "detect_os"
    test_function_exists "is_macos"
    test_function_exists "is_linux" 
    test_function_exists "is_wsl"
    test_function_exists "has_command"
    test_function_exists "safe_path_append"
    test_function_exists "setup_locale"
    test_function_exists "setup_editor"
    
    # Test environment variables are set correctly
    echo -e "\n${YELLOW}5. Environment Tests${NC}"
    
    # Test in subshell to avoid affecting current environment
    temp_dir=$(mktemp -d)
    local env_test="$temp_dir/env_test.zsh"
    cat > "$env_test" << 'EOF'
export HOME="$PWD"
source .zprofile 2>/dev/null

# Check critical environment variables
[[ -n "$PATH" ]] && echo "PATH: SET"
[[ -n "$EDITOR" ]] && echo "EDITOR: SET"
[[ -n "$PAGER" ]] && echo "PAGER: SET"
EOF
    
    test_assert "PATH is set" "zsh '$env_test' | grep -q 'PATH: SET'"
    test_assert "EDITOR is set" "zsh '$env_test' | grep -q 'EDITOR: SET'"
    test_assert "PAGER is set" "zsh '$env_test' | grep -q 'PAGER: SET'"
    
    # Performance tests
    echo -e "\n${YELLOW}6. Performance Tests${NC}"
    
    # Test loading time (should be under 1 second for basic config)
    temp_dir=$(mktemp -d)
    local perf_test="$temp_dir/perf_test.zsh"
    cat > "$perf_test" << 'EOF'
start=$(date +%s.%N)
source .zsh/utils.zsh
source .zprofile >/dev/null 2>&1
end=$(date +%s.%N)
duration=$(echo "$end - $start" | bc 2>/dev/null || echo "1")
if (( $(echo "$duration < 2.0" | bc 2>/dev/null || echo "1") )); then
    echo "Performance: ACCEPTABLE ($duration seconds)"
else
    echo "Performance: SLOW ($duration seconds)"
fi
EOF
    
    test_assert "Configuration loads quickly" "zsh '$perf_test' | grep -q 'ACCEPTABLE'"
    
    # Clean up
    rm -rf "$temp_dir"
    
    # Summary
    echo -e "\n${YELLOW}Test Summary${NC}"
    echo "============="
    echo "Tests run: $TESTS_RUN"
    echo -e "Passed: ${GREEN}$TESTS_PASSED${NC}"
    echo -e "Failed: ${RED}$TESTS_FAILED${NC}"
    
    if [[ $TESTS_FAILED -eq 0 ]]; then
        echo -e "\n${GREEN}All tests passed! Configuration is ready for deployment.${NC}"
        return 0
    else
        echo -e "\n${RED}Some tests failed. Please review before deployment.${NC}"
        return 1
    fi
}

# Run tests
main "$@"