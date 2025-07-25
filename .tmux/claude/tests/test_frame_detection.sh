#!/bin/bash
# 枠検出システムの単体テスト

# 設定
CLAUDE_VOICE_HOME="${CLAUDE_VOICE_HOME:-$HOME/.tmux/claude}"

# 基本モジュールの読み込み
source "$CLAUDE_VOICE_HOME/core/base.sh"
source "$CLAUDE_VOICE_HOME/core/screen_capture.sh"

# デバッグモードを有効化
claude_voice_init true

echo "=== Frame Detection System Test ==="
echo ""

# 枠検出のテスト関数を直接実行
test_frame_detection

echo ""
echo "=== Current Pane Smart Capture Test ==="
echo ""

# 現在のペインでの智的拡大テスト
echo "Testing smart expansion on current pane..."

# 複数の初期行数でテスト
test_lines=(10 20 30)

for lines in "${test_lines[@]}"; do
    echo "Testing with initial $lines lines:"
    
    # 智的拡大有効でキャプチャ
    echo "  Running smart capture..."
    smart_result=$(capture_screen_text "." "$lines" "true" "true")
    smart_length=${#smart_result}
    
    # 通常キャプチャ
    echo "  Running normal capture..."
    normal_result=$(capture_screen_text "." "$lines" "true" "false")
    normal_length=${#normal_result}
    
    echo "  Smart expansion: $smart_length characters"
    echo "  Normal capture:  $normal_length characters"
    echo "  Difference:      $((smart_length - normal_length)) characters"
    
    # 枠検出結果
    frame_info=$(detect_character_frames "$smart_result")
    frame_status=$(echo "$frame_info" | grep -o '"status": "[^"]*"' | cut -d'"' -f4)
    echo "  Frame status:    $frame_status"
    echo ""
done

echo "✅ Frame detection system test completed"