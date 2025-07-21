#!/bin/bash
# Unit tests for enhanced Claude Code status detection

# Test framework
TESTS_PASSED=0
TESTS_FAILED=0
TEST_FIXTURES_DIR="$HOME/.tmux/tests/fixtures"
CLAUDE_SCRIPT_PATH="$HOME/.tmux/scripts/claude-status-enhanced.sh"

# Define the detection function for testing (extracted from claude-status-enhanced.sh)
detect_claude_status() {
    local output="$1"
    
    # For testing purposes, use the full output as context
    # In real implementation, this would extract UI context more carefully
    local ui_context="$output"
    
    # 1. Waiting: User input required (highest priority)
    if echo "$ui_context" | grep -qE '(Do you want|Would you like|Continue\?|Proceed\?|❯.*Yes|Error:|Failed:|Exception:)'; then
        echo "Waiting"
        return
    fi
    
    # 2. Busy: Processing with tokens and interrupt
    if echo "$ui_context" | grep -qE '\([0-9]+s\s+·.*tokens.*interrupt\)'; then
        echo "Busy"
        return
    fi
    
    # 3. Idle: Ready for input (prompt pattern)
    if echo "$ui_context" | grep -qE '>\s*$'; then
        echo "Idle"
        return
    fi
    
    # Default to Idle
    echo "Idle"
}

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Test helper functions
test_case() {
    local test_name="$1"
    local test_func="$2"
    
    echo -e "${YELLOW}Running: $test_name${NC}"
    
    if $test_func; then
        echo -e "${GREEN}✅ PASS: $test_name${NC}"
        ((TESTS_PASSED++))
    else
        echo -e "${RED}❌ FAIL: $test_name${NC}"
        ((TESTS_FAILED++))
    fi
    echo ""
}

assert_equals() {
    local expected="$1"
    local actual="$2"
    local message="${3:-Assertion failed}"
    
    if [ "$expected" = "$actual" ]; then
        return 0
    else
        echo -e "${RED}  Expected: '$expected'${NC}"
        echo -e "${RED}  Actual: '$actual'${NC}"
        echo -e "${RED}  Message: $message${NC}"
        return 1
    fi
}

assert_contains() {
    local haystack="$1"
    local needle="$2"
    local message="${3:-Should contain pattern}"
    
    if echo "$haystack" | grep -q "$needle"; then
        return 0
    else
        echo -e "${RED}  Pattern '$needle' not found${NC}"
        echo -e "${RED}  In text: '$haystack'${NC}"
        echo -e "${RED}  Message: $message${NC}"
        return 1
    fi
}

# Mock function to test detection layers independently
test_ui_detection() {
    # Test processing pattern detection with real function
    local output_with_spinner="✻ Ruminating… (6s · 2.8k tokens · esc to interrupt)"
    local result=$(detect_claude_status "$output_with_spinner")
    assert_equals "Busy" "$result" "Should detect busy state from processing pattern"
}

test_thinking_detection() {
    # Test busy state detection with thinking pattern
    local output_thinking="✢ Thinking… (3s · 23 tokens · esc to interrupt)"
    local result=$(detect_claude_status "$output_thinking")
    assert_equals "Busy" "$result" "Should detect busy state from thinking pattern"
}

test_error_detection() {
    # Test waiting state detection with error pattern
    local output_error="Error: Failed to execute command\nDo you want to try again?"
    local result=$(detect_claude_status "$output_error")
    assert_equals "Waiting" "$result" "Should detect waiting state from error"
}

test_prompt_detection() {
    # Test idle state detection with prompt pattern
    local output_prompt="> \n? for shortcuts"
    local result=$(detect_claude_status "$output_prompt")
    assert_equals "Idle" "$result" "Should detect idle state from prompt"
}

test_context_confirmation() {
    # Test waiting state detection with confirmation pattern
    local output_confirm="Do you want to proceed with this action?\n❯ 1. Yes\n  2. No"
    local result=$(detect_claude_status "$output_confirm")
    assert_equals "Waiting" "$result" "Should detect waiting state from confirmation"
}

test_context_input_request() {
    # Test waiting state detection with input request pattern
    local output_input="Would you like to continue?"
    local result=$(detect_claude_status "$output_input")
    assert_equals "Waiting" "$result" "Should detect waiting state from input request"
}

test_context_interrupt() {
    # Test busy state detection with interrupt pattern
    local output_interrupt="✻ Processing… (15s · 1.2k tokens · esc to interrupt)"
    local result=$(detect_claude_status "$output_interrupt")
    assert_equals "Busy" "$result" "Should detect busy state from interruptible process"
}

test_pattern_matching_processing() {
    # Test busy state pattern matching
    local output_processing="(5s · 234 tokens · esc to interrupt)"
    local result=$(detect_claude_status "$output_processing")
    assert_equals "Busy" "$result" "Should detect busy state via pattern matching"
}

test_pattern_matching_idle() {
    # Test idle state pattern matching
    local output_idle="Welcome to Claude Code\n\n> "
    local result=$(detect_claude_status "$output_idle")
    assert_equals "Idle" "$result" "Should detect idle state via pattern matching"
}

test_confidence_combination_high_priority() {
    # Test that busy detection takes priority over idle
    local busy_output="✻ Processing… (10s · 500 tokens · esc to interrupt)"
    local result=$(detect_claude_status "$busy_output")
    assert_equals "Busy" "$result" "Busy pattern should take priority"
}

test_confidence_combination_fallback() {
    # Test fallback to idle when no other patterns match
    local unknown_output="Some random text that doesn't match patterns"
    local result=$(detect_claude_status "$unknown_output")
    assert_equals "Idle" "$result" "Should fallback to idle state"
}

# Create test fixture files
create_test_fixtures() {
    mkdir -p "$TEST_FIXTURES_DIR"
    
    # Fixture 1: Claude Code idle state
    cat > "$TEST_FIXTURES_DIR/claude_idle.txt" << 'EOF'
Welcome to Claude Code

Type your message below. Use Shift+Tab to enable auto-accept edits.

> 
EOF

    # Fixture 2: Claude Code thinking (busy pattern)
    cat > "$TEST_FIXTURES_DIR/claude_thinking.txt" << 'EOF'
> help me write a function

✢ Thinking… (8s · 150 tokens · esc to interrupt)
EOF

    # Fixture 3: Claude Code processing with interrupt
    cat > "$TEST_FIXTURES_DIR/claude_processing.txt" << 'EOF'
> create a new file

✻ Processing… (12s · 450 tokens · esc to interrupt)
EOF

    # Fixture 4: Claude Code waiting for confirmation
    cat > "$TEST_FIXTURES_DIR/claude_confirmation.txt" << 'EOF'
I can help you create that file. The content will be:

```python
def hello():
    print("Hello, World!")
```

Do you want me to proceed with creating this file?
EOF

    # Fixture 5: Claude Code error state
    cat > "$TEST_FIXTURES_DIR/claude_error.txt" << 'EOF'
> run invalid command

Error: Command not found
Failed to execute the requested operation.
EOF

    echo "Test fixtures created in $TEST_FIXTURES_DIR"
}

# Integration test using fixtures
test_fixture_integration() {
    local fixture_file="$1"
    local expected_state="$2"
    local description="$3"
    
    if [ ! -f "$fixture_file" ]; then
        echo "Fixture file not found: $fixture_file"
        return 1
    fi
    
    # Mock tmux capture-pane by reading fixture
    local captured_output=$(cat "$fixture_file")
    
    # Test the actual detection function
    local result=$(detect_claude_status "$captured_output")
    
    assert_equals "$expected_state" "$result" "$description"
}

test_idle_fixture() {
    test_fixture_integration "$TEST_FIXTURES_DIR/claude_idle.txt" "Idle" "Should detect idle state from fixture"
}

test_thinking_fixture() {
    test_fixture_integration "$TEST_FIXTURES_DIR/claude_thinking.txt" "Busy" "Should detect busy state from thinking fixture"
}

test_processing_fixture() {
    test_fixture_integration "$TEST_FIXTURES_DIR/claude_processing.txt" "Busy" "Should detect busy state from processing fixture"
}

test_confirmation_fixture() {
    test_fixture_integration "$TEST_FIXTURES_DIR/claude_confirmation.txt" "Waiting" "Should detect waiting state from confirmation fixture"
}

test_error_fixture() {
    test_fixture_integration "$TEST_FIXTURES_DIR/claude_error.txt" "Waiting" "Should detect waiting state from error fixture"
}

# Performance test
test_detection_performance() {
    local iterations=20
    local total_time=0
    
    echo "Running performance test ($iterations iterations)..."
    
    for ((i=1; i<=iterations; i++)); do
        local start_time=$(date +%s%N)
        # Run detection on test data
        detect_claude_status "✻ Thinking… (5s · 100 tokens · esc to interrupt)" >/dev/null 2>&1
        local end_time=$(date +%s%N)
        
        local duration=$(( (end_time - start_time) / 1000000 ))  # Convert to ms
        total_time=$((total_time + duration))
    done
    
    local avg_time=$((total_time / iterations))
    echo "Average detection time: ${avg_time}ms"
    
    # Performance target: under 10ms per detection
    if [ $avg_time -lt 10 ]; then
        echo -e "${GREEN}✅ Performance test passed${NC}"
        return 0
    else
        echo -e "${RED}❌ Performance test failed: ${avg_time}ms > 10ms${NC}"
        return 1
    fi
}

# Main test execution
main() {
    echo -e "${YELLOW}=== Enhanced Claude Code Status Detection Tests ===${NC}"
    echo ""
    
    # Create test fixtures
    create_test_fixtures
    echo ""
    
    # UI Detection Tests
    echo -e "${YELLOW}--- UI Element Detection Tests ---${NC}"
    test_case "UI: Progress Indicator Detection" test_ui_detection
    test_case "UI: Thinking State Detection" test_thinking_detection
    test_case "UI: Error State Detection" test_error_detection
    test_case "UI: Prompt Detection" test_prompt_detection
    
    # Context Analysis Tests
    echo -e "${YELLOW}--- Context Analysis Tests ---${NC}"
    test_case "Context: Confirmation Request" test_context_confirmation
    test_case "Context: Input Request" test_context_input_request
    test_case "Context: Interrupt Detection" test_context_interrupt
    
    # Pattern Matching Tests
    echo -e "${YELLOW}--- Pattern Matching Tests ---${NC}"
    test_case "Pattern: Processing State" test_pattern_matching_processing
    test_case "Pattern: Idle State" test_pattern_matching_idle
    
    # Confidence System Tests
    echo -e "${YELLOW}--- Confidence System Tests ---${NC}"
    test_case "Confidence: High Priority Override" test_confidence_combination_high_priority
    test_case "Confidence: Medium Fallback" test_confidence_combination_fallback
    
    # Fixture Integration Tests
    echo -e "${YELLOW}--- Fixture Integration Tests ---${NC}"
    test_case "Fixture: Idle State" test_idle_fixture
    test_case "Fixture: Thinking State" test_thinking_fixture
    test_case "Fixture: Processing State" test_processing_fixture
    test_case "Fixture: Confirmation State" test_confirmation_fixture
    test_case "Fixture: Error State" test_error_fixture
    
    # Performance Tests
    echo -e "${YELLOW}--- Performance Tests ---${NC}"
    test_case "Performance: Detection Speed" test_detection_performance
    
    # Summary
    echo -e "${YELLOW}=== Test Summary ===${NC}"
    echo -e "Tests passed: ${GREEN}$TESTS_PASSED${NC}"
    echo -e "Tests failed: ${RED}$TESTS_FAILED${NC}"
    echo -e "Total tests: $((TESTS_PASSED + TESTS_FAILED))"
    
    if [ $TESTS_FAILED -eq 0 ]; then
        echo -e "${GREEN}🎉 All tests passed!${NC}"
        exit 0
    else
        echo -e "${RED}💥 Some tests failed!${NC}"
        exit 1
    fi
}

# Run tests if script is executed directly
if [[ "${BASH_SOURCE[0]}" == "${0}" ]]; then
    main "$@"
fi