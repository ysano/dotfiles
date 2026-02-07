#!/bin/bash
# ファイル名: pan_test.sh
# 説明: Claude Voice パンニング統合テスト
# 使い方: ./pan_test.sh [--quick|--full]
#   --quick (デフォルト): 基本3点パンニング + 連続移動テスト
#   --full: 包括的テスト（詳細9点、複数音源、長文、エラーハンドリング含む）

set -euo pipefail
cd "$(dirname "${BASH_SOURCE[0]}")"

export TMUX_CLAUDE_VOICE_DEBUG=1

MODE="${1:---quick}"

# プラットフォーム別テスト音声ファイルの設定
if [[ "$(uname)" == "Darwin" ]]; then
    TEST_SOUND="/System/Library/Sounds/Glass.aiff"
    TEST_SOUNDS=("Glass.aiff" "Ping.aiff" "Funk.aiff" "Submarine.aiff")
    SOUND_DIR="/System/Library/Sounds"
else
    TEST_SOUND="/mnt/c/Windows/Media/Windows Notify Messaging.wav"
    TEST_SOUNDS=("Alarm01.wav" "Alarm02.wav" "Alarm03.wav" "Alarm04.wav")
    SOUND_DIR="/mnt/c/Windows/Media"
fi

# テスト音声ファイルの存在確認
if [[ ! -f "$TEST_SOUND" ]]; then
    echo "テスト音声ファイルが見つかりません: $TEST_SOUND"
    echo "利用可能な音声ファイルを検索中..."
    first_sound=$(find "$SOUND_DIR" -maxdepth 1 -type f \( -name '*.wav' -o -name '*.aiff' \) 2>/dev/null | head -1)
    if [[ -n "$first_sound" ]]; then
        TEST_SOUND="$first_sound"
        echo "代替ファイルを使用: $TEST_SOUND"
    else
        echo "音声ファイルが見つかりません。テストを中止します。"
        exit 1
    fi
fi

# === Quick モード（デフォルト）===
run_quick_test() {
    echo "=== Claude Voice パンニング基本テスト ==="
    echo "    通知音 + 読み上げ音声の左右パンニング"
    echo "    プラットフォーム: $(uname)"
    echo ""

    # パンニング機能が有効か確認
    panning_enabled=$(tmux show-option -gqv @claude_voice_panning_enabled 2>/dev/null || echo "true")
    echo "パンニング機能: $panning_enabled"

    if [[ "$panning_enabled" != "true" ]]; then
        echo "パンニング機能を有効化中..."
        tmux set-option -g @claude_voice_panning_enabled true
    fi

    echo ""
    echo "1. 通知音パンニングテスト"
    positions=(-0.8 0.0 0.8)
    labels=("左端" "中央" "右端")
    for i in "${!positions[@]}"; do
        echo -n "   ${labels[$i]}(${positions[$i]}): "
        ./panning_engine.sh pan "$TEST_SOUND" "${positions[$i]}"
        echo "再生中..."
        sleep 2
    done

    echo ""
    echo "2. パンゲイン計算テスト（音声再生なし）"
    ./panning_engine.sh test-gains 2>&1 | grep "位置"

    echo ""
    echo "3. 連続パンニング移動テスト"
    move_positions=(-0.8 -0.4 0.0 0.4 0.8)
    for pos in "${move_positions[@]}"; do
        echo -n "   位置 $pos: "
        ./panning_engine.sh pan "$TEST_SOUND" "$pos"
        echo "再生中..."
        sleep 1
    done

    echo ""
    echo "=== 基本テスト完了 ==="
}

# === Full モード ===
run_full_test() {
    # まず基本テストを実行
    run_quick_test

    echo ""
    echo "=== 包括的テスト（追加項目）==="

    echo ""
    echo "4. 詳細9点パンニングテスト (-1.0 ~ 1.0)"
    detailed_positions=(-1.0 -0.75 -0.5 -0.25 0.0 0.25 0.5 0.75 1.0)
    for pos in "${detailed_positions[@]}"; do
        echo -n "   位置 $pos: "
        ./panning_engine.sh pan "$TEST_SOUND" "$pos"
        echo "再生中..."
        sleep 1
    done

    echo ""
    echo "5. 複数音源パンニングテスト"
    test_positions=(-0.6 -0.2 0.2 0.6)
    for i in "${!TEST_SOUNDS[@]}"; do
        local_sound="${SOUND_DIR}/${TEST_SOUNDS[$i]}"
        if [[ -f "$local_sound" && $i -lt ${#test_positions[@]} ]]; then
            echo -n "   ${TEST_SOUNDS[$i]} (位置 ${test_positions[$i]}): "
            ./panning_engine.sh pan "$local_sound" "${test_positions[$i]}"
            echo "再生中..."
            sleep 2
        fi
    done

    echo ""
    echo "6. エラーハンドリングテスト"
    invalid_positions=("abc" "")
    for pos in "${invalid_positions[@]}"; do
        echo -n "   無効値 '$pos': "
        ./panning_engine.sh pan "$TEST_SOUND" "$pos" 2>/dev/null && echo "通過" || echo "エラー検出"
    done

    echo ""
    echo "=== 包括的テスト完了 ==="
}

# メイン実行
case "$MODE" in
    --quick|-q)
        run_quick_test
        ;;
    --full|-f)
        run_full_test
        ;;
    *)
        echo "使用方法: $0 [--quick|--full]"
        echo "  --quick, -q  基本テスト（デフォルト）"
        echo "  --full,  -f  包括的テスト"
        exit 1
        ;;
esac
