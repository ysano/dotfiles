#!/bin/bash
# Test suite for 3-state Claude Code detection based on 14 real captures

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m'

# Test counters
TESTS_PASSED=0
TESTS_FAILED=0

# Test framework
test_detection() {
    local test_name="$1"
    local input_text="$2"
    local expected_status="$3"
    local description="$4"
    
    echo -e "${BLUE}Testing: $test_name${NC}"
    echo -e "Description: $description"
    
    # Use inline detection logic (simplified) - matches enhanced script priority
    local actual_status=""
    
    # 1. Waiting: User input required (highest priority)
    if echo "$input_text" | grep -qE '(Do you want|Would you like|Continue\?|Proceed\?|❯.*Yes|Error:|Failed:|Exception:)'; then
        actual_status="Waiting"
    # 2. Busy: Processing with tokens and interrupt
    elif echo "$input_text" | grep -qE '\([0-9]+s\s+·.*tokens.*interrupt\)'; then
        actual_status="Busy"
    # 3. Idle: Ready for input
    elif echo "$input_text" | grep -qE '>\s*$'; then
        actual_status="Idle"
    else
        actual_status="Idle"
    fi
    
    if [ "$actual_status" = "$expected_status" ]; then
        echo -e "${GREEN}✅ PASS${NC}: Expected '$expected_status', got '$actual_status'"
        ((TESTS_PASSED++))
    else
        echo -e "${RED}❌ FAIL${NC}: Expected '$expected_status', got '$actual_status'"
        echo -e "Input preview: ${input_text:0:80}..."
        ((TESTS_FAILED++))
    fi
    echo ""
}

echo -e "${YELLOW}=== 3-State Claude Code Detection Test Suite ===${NC}"
echo -e "Based on 14 real capture scenarios\n"

# Test 1: Basic Thinking (Busy)
test_detection \
    "Basic Thinking" \
    "✢ Thinking… (3s · 23 tokens · esc to interrupt)" \
    "Busy" \
    "Basic processing with small token count"

# Test 2: Deep Ruminating (Busy)
test_detection \
    "Deep Ruminating" \
    "✻ Ruminating… (6s · 2.8k tokens · esc to interrupt)" \
    "Busy" \
    "Heavy processing with large token count"

# Test 3: Output Generation (Busy)
test_detection \
    "Output Generation" \
    "* Sparkling… (21s · ↓ 474 tokens · esc to interrupt)" \
    "Busy" \
    "Output generation with direction indicator"

# Test 4: Tool Processing (Busy)
test_detection \
    "Tool Processing" \
    "✢ Designing… (9s · ⚒ 3.3k tokens · esc to interrupt)" \
    "Busy" \
    "Tool-enhanced processing with hammer icon"

# Test 5: Plan Confirmation (Waiting)
test_detection \
    "Plan Confirmation" \
    "Would you like to proceed?
❯ 1. Yes
  2. No, keep planning" \
    "Waiting" \
    "Plan execution confirmation dialog"

# Test 6: Tool Confirmation (Waiting)
test_detection \
    "Tool Confirmation" \
    "Bash command
find src/services -name \"*.js\" | xargs wc -l | tail -1
Count total lines in all services

Do you want to proceed?
❯ 1. Yes
  2. No, and tell Claude what to do differently (esc)" \
    "Waiting" \
    "Tool execution confirmation dialog"

# Test 7: API Error (Waiting)
test_detection \
    "API Error" \
    "API Error: Request timed out.
Try \"edit patients-page.js to...\"" \
    "Waiting" \
    "API timeout error requiring user attention"

# Test 8: Normal Idle
test_detection \
    "Normal Idle" \
    "> 
? for shortcuts" \
    "Idle" \
    "Basic idle state with help prompt"

# Test 9: Context Idle
test_detection \
    "Context Idle" \
    "> 
? for shortcuts
Context left until auto-compact: 38%" \
    "Idle" \
    "Idle state with context information"

# Test 10: Auto-accept Idle
test_detection \
    "Auto-accept Idle" \
    "> 
⏵⏵ auto-accept edits on (shift+tab to cycle)
Context left until auto-compact: 37%" \
    "Idle" \
    "Idle state in auto-accept mode"

# Test 11: Permission Context Idle
test_detection \
    "Permission Context Idle" \
    "> 
? for shortcuts
Bypassing Permissions" \
    "Idle" \
    "Idle state with permission bypass"

# Test 12: Plan Mode (Idle)
test_detection \
    "Plan Mode" \
    "> 
⏸ plan mode on (shift+tab to cycle)" \
    "Idle" \
    "Idle state in planning mode"

# Test 13: Error with Suggestion (Waiting)
test_detection \
    "Error with Suggestion" \
    "Failed to execute command
Error: Command not found" \
    "Waiting" \
    "Execution error requiring user intervention"

# Test 14: Complex Busy Pattern
test_detection \
    "Complex Busy Pattern" \
    "╭─────────────────────────────────────╮
│ ✻ Ruminating… (15s · 🔧 1.2k tokens · esc to interrupt) │
╰─────────────────────────────────────╯" \
    "Busy" \
    "Complex processing pattern with UI elements"

# Test edge cases
echo -e "${YELLOW}=== Edge Case Tests ===${NC}"

# Edge case 1: Empty input
test_detection \
    "Empty Input" \
    "" \
    "Idle" \
    "Empty terminal output"

# Edge case 2: No Claude Code UI
test_detection \
    "Non-Claude Terminal" \
    "$ ls -la
total 64
drwxr-xr-x  8 user user 4096 Jan 1 12:00 .
drwxr-xr-x 20 user user 4096 Jan 1 12:00 .." \
    "Idle" \
    "Regular shell terminal (should default to Idle)"

# Edge case 3: Mixed patterns
test_detection \
    "Mixed Patterns" \
    "> 
Do you want to continue with (5s · 123 tokens · esc to interrupt)?" \
    "Waiting" \
    "Waiting pattern should take precedence over busy pattern in question"

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