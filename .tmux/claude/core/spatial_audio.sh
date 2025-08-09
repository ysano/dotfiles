#!/bin/bash
# Claude Voice - Spatial Audio Manager
# デシベルベースの空間音響配置システム

# Claude Codeを実行しているウィンドウを検出
detect_claude_windows() {
    local windows=()
    
    # すべてのtmuxウィンドウをチェック
    while IFS=$'\t' read -r window_id window_name; do
        # ウィンドウ内のペインでClaude Codeプロセスを検索
        local pane_pids=$(tmux list-panes -t "$window_id" -F "#{pane_pid}" 2>/dev/null)
        
        for pane_pid in $pane_pids; do
            # プロセスツリーでclaude/nodeを検索
            if pstree -p "$pane_pid" 2>/dev/null | grep -q "node.*claude"; then
                windows+=("$window_id")
                break
            fi
        done
    done <<< "$(tmux list-windows -F '#{window_index}	#{window_name}')"
    
    # 重複を削除してソート
    printf '%s\n' "${windows[@]}" | sort -u
}

# デシベルベースの均等配置計算
# Equal Power Pan Law (3dB center)を使用
calculate_db_panning() {
    local window_count="$1"
    local window_index="$2"  # 0-based
    
    if [[ $window_count -le 1 ]]; then
        # モノラル（センター）
        echo "0.707|0.707"  # -3dB on both channels
        return
    fi
    
    # 位置を-1.0（左）から+1.0（右）に正規化
    local position=$(echo "scale=4; ($window_index * 2.0 / ($window_count - 1)) - 1.0" | bc -l)
    
    # Equal Power Pan Law: 
    # Left = cos((position + 1) * π/4)
    # Right = sin((position + 1) * π/4)
    
    # 角度計算（ラジアン）
    local angle=$(echo "scale=4; ($position + 1.0) * 0.7854" | bc -l)  # π/4 = 0.7854
    
    # 簡略化されたパンニング計算
    # 位置が-1（左）のとき: L=1.0, R=0.0
    # 位置が0（中央）のとき: L=0.707, R=0.707
    # 位置が+1（右）のとき: L=0.0, R=1.0
    
    local left_gain=$(echo "scale=4; sqrt((1.0 - $position) / 2.0)" | bc -l)
    local right_gain=$(echo "scale=4; sqrt((1.0 + $position) / 2.0)" | bc -l)
    
    # 負の値を0に補正
    if (( $(echo "$left_gain < 0" | bc -l) )); then
        left_gain="0"
    fi
    if (( $(echo "$right_gain < 0" | bc -l) )); then
        right_gain="0"
    fi
    
    echo "${left_gain}|${right_gain}"
}

# デシベルからリニアゲインへの変換
db_to_linear() {
    local db="$1"
    echo "scale=4; e($db * 0.1151)" | bc -l  # 10^(db/20) = e^(db*ln(10)/20)
}

# リニアゲインからデシベルへの変換
linear_to_db() {
    local linear="$1"
    if (( $(echo "$linear <= 0" | bc -l) )); then
        echo "-60"  # 実質的な無音
    else
        echo "scale=2; l($linear) * 8.6859" | bc -l  # 20*log10(linear) = 20*ln(linear)/ln(10)
    fi
}

# 空間配置されたClaude音声を再生
play_spatial_claude_voices() {
    local text="${1:-Claude Voice spatial audio test}"
    local voice="${2:-Kyoko}"
    local rate="${3:-200}"
    
    echo "🎧 Detecting Claude Code windows..."
    
    # Claude Codeウィンドウを検出
    local claude_windows=()
    while IFS= read -r window_id; do
        [[ -n "$window_id" ]] && claude_windows+=("$window_id")
    done <<< "$(detect_claude_windows)"
    
    local window_count=${#claude_windows[@]}
    
    if [[ $window_count -eq 0 ]]; then
        echo "❌ No Claude Code windows detected"
        return 1
    fi
    
    echo "📍 Found $window_count Claude Code window(s): ${claude_windows[*]}"
    echo "🔊 Playing spatial audio..."
    
    # 各ウィンドウに対して空間配置された音声を生成
    local index=0
    for window_id in "${claude_windows[@]}"; do
        # パンニング係数を計算
        local panning=$(calculate_db_panning "$window_count" "$index")
        IFS='|' read -r left_gain right_gain <<< "$panning"
        
        # デシベル値をログ
        local left_db=$(linear_to_db "$left_gain")
        local right_db=$(linear_to_db "$right_gain")
        
        echo "  Window $window_id: L=${left_db}dB, R=${right_db}dB"
        
        # ウィンドウ固有のテキスト
        local window_text="ウィンドウ ${window_id} からの音声です。${text}"
        
        # 空間音声を再生（バックグラウンド）
        play_panned_speech "$window_text" "$voice" "$rate" "$left_gain" "$right_gain" &
        
        # 少し遅延を入れて識別しやすくする
        sleep 0.5
        
        ((index++))
    done
    
    # すべての音声再生を待つ
    wait
    
    echo "✅ Spatial audio playback completed"
}

# パンニングされた音声を再生（macOS用）
play_panned_speech() {
    local text="$1"
    local voice="$2"
    local rate="$3"
    local left_gain="$4"
    local right_gain="$5"
    
    if [[ "$OSTYPE" != "darwin"* ]]; then
        log "WARN" "Spatial audio is currently only supported on macOS"
        return 1
    fi
    
    # 一時音声ファイルを生成
    local temp_audio="/tmp/claude_voice_${RANDOM}.aiff"
    
    # sayコマンドで音声ファイルを生成
    if say -v "$voice" -r "$rate" -o "$temp_audio" "$text" 2>/dev/null; then
        # ffplayでパンニング再生（必要に応じてffmpegをインストール）
        if command -v ffplay >/dev/null 2>&1; then
            # パンニングフィルタを適用
            ffplay -nodisp -autoexit -loglevel quiet \
                -af "pan=stereo|c0=${left_gain}*c0|c1=${right_gain}*c0" \
                "$temp_audio" 2>/dev/null
        else
            # フォールバック: 通常再生
            afplay "$temp_audio" 2>/dev/null
        fi
        
        # 一時ファイルを削除
        rm -f "$temp_audio"
    else
        log "ERROR" "Failed to generate speech audio"
        return 1
    fi
}

# パンニングテスト
test_spatial_audio() {
    echo "=== Spatial Audio Test ==="
    echo ""
    
    # 手動でウィンドウ数を指定してテスト
    echo "1. Testing panning calculation..."
    
    for window_count in 2 3 4 5; do
        echo "  With $window_count windows:"
        for ((i=0; i<window_count; i++)); do
            local panning=$(calculate_db_panning "$window_count" "$i")
            IFS='|' read -r left right <<< "$panning"
            local left_db=$(linear_to_db "$left")
            local right_db=$(linear_to_db "$right")
            printf "    Window %d: L=%.2fdB, R=%.2fdB\n" "$i" "$left_db" "$right_db"
        done
        echo ""
    done
    
    echo "2. Testing actual Claude window detection..."
    detect_claude_windows
    
    echo ""
    echo "3. Playing spatial audio test..."
    play_spatial_claude_voices "テスト音声"
}

# bc用の数学関数定義
export BC_ENV_ARGS="-l"
export BC_LINE_LENGTH=0

# bcの数学ライブラリを有効化
if ! echo "s(0)" | bc -l >/dev/null 2>&1; then
    log "WARN" "bc math library not available, using approximations"
    
    # 簡易的な近似関数
    calculate_db_panning() {
        local window_count="$1"
        local window_index="$2"
        
        if [[ $window_count -le 1 ]]; then
            echo "0.707|0.707"
            return
        fi
        
        # 線形パンニングの近似
        local position=$(echo "scale=4; $window_index / ($window_count - 1)" | bc)
        local left_gain=$(echo "scale=4; 1.0 - $position" | bc)
        local right_gain="$position"
        
        # パワー正規化
        local sum=$(echo "scale=4; $left_gain * $left_gain + $right_gain * $right_gain" | bc)
        local norm=$(echo "scale=4; sqrt($sum)" | bc)
        
        if [[ "$norm" != "0" ]]; then
            left_gain=$(echo "scale=4; $left_gain / $norm" | bc)
            right_gain=$(echo "scale=4; $right_gain / $norm" | bc)
        fi
        
        echo "${left_gain}|${right_gain}"
    }
fi

# エクスポート
export -f detect_claude_windows
export -f calculate_db_panning
export -f db_to_linear
export -f linear_to_db
export -f play_spatial_claude_voices
export -f play_panned_speech
export -f test_spatial_audio