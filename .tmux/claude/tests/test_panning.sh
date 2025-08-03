#!/bin/bash
# 動的パンニング音声テスト

# 設定
CLAUDE_VOICE_HOME="${CLAUDE_VOICE_HOME:-$HOME/.tmux/claude}"

# 基本モジュールの読み込み
source "$CLAUDE_VOICE_HOME/core/foundation.sh"
source "$CLAUDE_VOICE_HOME/os/darwin.sh"

# テスト用設定の一時的有効化
export TEST_ENABLE_PANNING_AUDIO=true

echo "=== 音声パンニングテスト開始 ==="
echo ""

# 現在のアクティブウィンドウを取得
echo "現在のアクティブウィンドウを検出中..."
active_windows=()
while IFS= read -r window_id; do
    [[ -n "$window_id" ]] && active_windows+=("$window_id")
done < <(get_active_claude_windows)

window_count=${#active_windows[@]}
echo "検出されたアクティブウィンドウ: $window_count個 (${active_windows[*]})"

if [[ $window_count -eq 0 ]]; then
    echo "アクティブなClaude Codeウィンドウがありません。"
    echo "テスト用のダミーデータで動作確認を行います。"
    
    # テスト用のダミーウィンドウIDを設定
    active_windows=(1 2 3)
    window_count=3
    echo "テスト用ウィンドウ: ${active_windows[*]}"
fi

echo ""
echo "各ウィンドウの音声パンニングをテスト中..."

for i in "${!active_windows[@]}"; do
    window_id="${active_windows[$i]}"
    
    # パンニング値を計算
    panning_values=$(calculate_dynamic_panning "$window_id")
    left_gain=$(echo "$panning_values" | cut -d' ' -f1)
    right_gain=$(echo "$panning_values" | cut -d' ' -f2)
    
    echo "ウィンドウ $window_id: L=$left_gain, R=$right_gain"
    
    # 実際の音声再生テスト（短時間）
    test_message="ウィンドウ ${window_id} からのテスト音声です"
    echo "  再生中: $test_message"
    
    # 音声再生（バックグラウンドで実行）
    speak_text "$test_message" "Kyoko" "system_default" "200" "$window_id" &
    
    # 次のテストまで少し待機
    sleep 3
done

# 全ての音声再生が完了するまで待機
echo ""
echo "全ての音声再生が完了するまで待機中..."
wait

echo ""
echo "✅ 音声パンニングテスト完了"
echo ""
echo "パンニング分布:"
for i in "${!active_windows[@]}"; do
    window_id="${active_windows[$i]}"
    case $i in
        0) position="完全左" ;;
        $((window_count-1))) position="完全右" ;;
        *) position="中央寄り" ;;
    esac
    echo "  ウィンドウ $window_id: $position"
done