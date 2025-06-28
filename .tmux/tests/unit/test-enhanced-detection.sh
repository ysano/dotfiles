#!/bin/bash
# Unit tests for enhanced Claude Code status detection

# Test framework
TESTS_PASSED=0
TESTS_FAILED=0
TEST_FIXTURES_DIR="$HOME/.tmux/tests/fixtures"

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
    # Source the detection functions (extract them to a separate file for testing)
    source ~/.tmux/scripts/claude-status-enhanced.sh
    
    # Test progress indicator detection
    local output_with_spinner="Processing request... |"
    local result=$(detect_ui_elements "$output_with_spinner")
    assert_equals "processing:high" "$result" "Should detect progress indicator"
}

test_thinking_detection() {
    source ~/.tmux/scripts/claude-status-enhanced.sh
    
    local output_thinking="Thinking about your request..."
    local result=$(detect_ui_elements "$output_thinking")
    assert_equals "thinking:high" "$result" "Should detect thinking state"
}

test_error_detection() {
    source ~/.tmux/scripts/claude-status-enhanced.sh
    
    local output_error="Error: Failed to execute command"
    local result=$(detect_ui_elements "$output_error")
    assert_equals "error_state:high" "$result" "Should detect error state"
}

test_prompt_detection() {
    source ~/.tmux/scripts/claude-status-enhanced.sh
    
    local output_prompt="claude > "
    local result=$(detect_ui_elements "$output_prompt")
    assert_equals "idle:medium" "$result" "Should detect prompt state"
}

test_context_confirmation() {
    source ~/.tmux/scripts/claude-status-enhanced.sh
    
    local output_confirm="Do you want to proceed with this action?"
    local result=$(analyze_context "$output_confirm")
    assert_equals "waiting_confirmation:high" "$result" "Should detect confirmation request"
}

test_context_input_request() {
    source ~/.tmux/scripts/claude-status-enhanced.sh
    
    local output_input="Please provide your API key:"
    local result=$(analyze_context "$output_input")
    assert_equals "waiting_input:high" "$result" "Should detect input request"
}

test_context_interrupt() {
    source ~/.tmux/scripts/claude-status-enhanced.sh
    
    local output_interrupt="Processing... (Press Esc to interrupt)"
    local result=$(analyze_context "$output_interrupt")
    assert_equals "processing:high" "$result" "Should detect interruptible process"
}

test_pattern_matching_processing() {
    source ~/.tmux/scripts/claude-status-enhanced.sh
    
    local output_processing="esc to interrupt current operation"
    local result=$(enhanced_pattern_matching "$output_processing")
    assert_equals "processing:high" "$result" "Should detect processing via pattern"
}

test_pattern_matching_idle() {
    source ~/.tmux/scripts/claude-status-enhanced.sh
    
    local output_idle="Welcome to Claude Code
    
    > "
    local result=$(enhanced_pattern_matching "$output_idle")
    assert_equals "idle:medium" "$result" "Should detect idle state"
}

test_confidence_combination_high_priority() {
    source ~/.tmux/scripts/claude-status-enhanced.sh
    
    # High confidence UI result should override medium confidence context
    local ui_result="processing:high"
    local context_result="idle:medium"
    local pattern_result="unknown:uncertain"
    
    local result=$(combine_detection_results "$ui_result" "$context_result" "$pattern_result")
    assert_equals "processing:high" "$result" "High confidence should take priority"
}

test_confidence_combination_fallback() {
    source ~/.tmux/scripts/claude-status-enhanced.sh
    
    # Should fallback to medium confidence when no high confidence available
    local ui_result="unknown:uncertain"
    local context_result="idle:medium" 
    local pattern_result="unknown:low"
    
    local result=$(combine_detection_results "$ui_result" "$context_result" "$pattern_result")
    assert_equals "idle:medium" "$result" "Should use medium confidence result"
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

    # Fixture 2: Claude Code thinking
    cat > "$TEST_FIXTURES_DIR/claude_thinking.txt" << 'EOF'
> help me write a function

Thinking about your request...
EOF

    # Fixture 3: Claude Code processing with interrupt
    cat > "$TEST_FIXTURES_DIR/claude_processing.txt" << 'EOF'
> create a new file

Creating file... (Press Esc to interrupt)
Processing request...
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
    
    # Test the enhanced detection (this would need the script to accept input as parameter)
    # For now, we'll test the individual components
    source ~/.tmux/scripts/claude-status-enhanced.sh
    
    local ui_result=$(detect_ui_elements "$captured_output")
    local context_result=$(analyze_context "$captured_output")
    local pattern_result=$(enhanced_pattern_matching "$captured_output")
    local final_result=$(combine_detection_results "$ui_result" "$context_result" "$pattern_result")
    
    local final_state=$(echo "$final_result" | cut -d: -f1)
    
    assert_equals "$expected_state" "$final_state" "$description"
}

test_idle_fixture() {
    test_fixture_integration "$TEST_FIXTURES_DIR/claude_idle.txt" "idle" "Should detect idle state from fixture"
}

test_thinking_fixture() {
    test_fixture_integration "$TEST_FIXTURES_DIR/claude_thinking.txt" "thinking" "Should detect thinking state from fixture"
}

test_processing_fixture() {
    test_fixture_integration "$TEST_FIXTURES_DIR/claude_processing.txt" "processing" "Should detect processing state from fixture"
}

test_confirmation_fixture() {
    test_fixture_integration "$TEST_FIXTURES_DIR/claude_confirmation.txt" "waiting_confirmation" "Should detect confirmation state from fixture"
}

test_error_fixture() {
    test_fixture_integration "$TEST_FIXTURES_DIR/claude_error.txt" "error_state" "Should detect error state from fixture"
}

# Performance test
test_detection_performance() {
    local iterations=50
    local total_time=0
    
    echo "Running performance test ($iterations iterations)..."
    
    for ((i=1; i<=iterations; i++)); do
        local start_time=$(date +%s%N)
        # Run detection on test fixture
        source ~/.tmux/scripts/claude-status-enhanced.sh >/dev/null 2>&1
        local end_time=$(date +%s%N)
        
        local duration=$(( (end_time - start_time) / 1000000 ))  # Convert to ms
        total_time=$((total_time + duration))
    done
    
    local avg_time=$((total_time / iterations))
    echo "Average detection time: ${avg_time}ms"
    
    # Performance target: under 50ms per detection
    if [ $avg_time -lt 50 ]; then
        echo -e "${GREEN}✅ Performance test passed${NC}"
        return 0
    else
        echo -e "${RED}❌ Performance test failed: ${avg_time}ms > 50ms${NC}"
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