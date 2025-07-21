#\!/bin/bash

detect_claude_status() {
    local output="$1"
    echo "Input: $output" >&2
    
    # Get only the 3 lines above the input UI box for precise state detection
    local ui_context=$(echo "$output" | grep -B3 "╭─" | tail -4 || echo "$output" | tail -3)
    echo "UI Context: $ui_context" >&2
    
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

# Test
result=$(detect_claude_status "✻ Ruminating… (6s · 2.8k tokens · esc to interrupt)")
echo "Result: $result"
