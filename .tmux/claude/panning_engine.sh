#!/bin/bash
# ファイル名: panning_engine.sh
# 説明: tmux-claude-voice デシベルパンニングエンジン

# 依存ファイルの存在確認
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
readonly SCRIPT_DIR

# 依存ファイルの読み込み
if [[ -f "${SCRIPT_DIR}/functions.sh" ]]; then
    source "${SCRIPT_DIR}/functions.sh"
fi

if [[ -f "${SCRIPT_DIR}/sound_utils.sh" ]]; then
    source "${SCRIPT_DIR}/sound_utils.sh"
fi

# フォールバックログ機能
if ! command -v log_info >/dev/null 2>&1; then
    log_info() { echo "[INFO] $(date '+%Y-%m-%d %H:%M:%S') - $1" >&2; }
    log_error() { echo "[ERROR] $(date '+%Y-%m-%d %H:%M:%S') - $1" >&2; }
    log_debug() { [[ "$TMUX_CLAUDE_VOICE_DEBUG" == "1" ]] && echo "[DEBUG] $(date '+%Y-%m-%d %H:%M:%S') - $1" >&2 || true; }
fi

# フォールバックOS種別取得
if ! command -v get_os_type >/dev/null 2>&1; then
    get_os_type() { uname; }
fi

# 設定値取得（デフォルト値付き）
get_tmux_panning_option() {
    local option="$1"
    local default_value="$2"
    
    local value
    if value=$(tmux show-option -gqv "@$option" 2>/dev/null); then
        echo "$value"
    else
        echo "$default_value"
    fi
}

# Claude Codeウィンドウを検出する関数（既存のdetect_claude_windowsと重複回避）
detect_claude_windows_for_panning() {
    local pattern=$(get_tmux_panning_option "claude_voice_window_pattern" "Claude|claude|CLAUDE")
    
    log_debug "Claudeウィンドウを検索中（パンニング用）: パターン='$pattern'"
    
    # tmuxウィンドウリストを取得し、パターンにマッチするウィンドウを検索
    local windows
    if windows=$(tmux list-windows -F "#{session_name}:#{window_index}:#{window_name}" 2>/dev/null); then
        local claude_windows
        claude_windows=$(echo "$windows" | grep -E ":($pattern)(\s|$)" | cut -d':' -f1,2,3)
        
        if [[ -n "$claude_windows" ]]; then
            log_debug "検出されたClaudeウィンドウ（パンニング用）: $claude_windows"
            echo "$claude_windows"
        else
            log_debug "Claudeウィンドウが見つかりません（パンニング用）"
            echo ""
        fi
    else
        log_error "tmuxウィンドウリストの取得に失敗しました（パンニング用）"
        return 1
    fi
}

# Claude Codeウィンドウ数をカウント
count_claude_windows() {
    detect_claude_windows_for_panning | wc -l
}

# ウィンドウの均等配置位置を計算する関数
calculate_equal_spacing() {
    local target_window="$1"  # session:window形式
    
    if [[ -z "$target_window" ]]; then
        log_error "ターゲットウィンドウが指定されていません"
        echo "0.0"
        return 1
    fi
    
    local total_claude_windows
    total_claude_windows=$(count_claude_windows)
    
    log_debug "均等配置計算: target=$target_window, total=$total_claude_windows"

    if [[ $total_claude_windows -eq 0 ]]; then
        echo "0.0"  # デフォルト位置
        return
    fi

    if [[ $total_claude_windows -eq 1 ]]; then
        echo "0.0"  # 中央配置
        return
    fi

    # Claude Codeウィンドウのリストを取得し、対象ウィンドウのインデックスを特定
    local claude_windows_list
    claude_windows_list=$(detect_claude_windows_for_panning | cut -d':' -f1,2 | sort)
    
    local target_index=-1
    local index=0
    
    while IFS= read -r window_entry; do
        if [[ "$window_entry" == "$target_window" ]]; then
            target_index=$index
            break
        fi
        ((index++))
    done <<< "$claude_windows_list"

    if [[ $target_index -eq -1 ]]; then
        log_error "ターゲットウィンドウがリストに見つかりません: $target_window"
        echo "0.0"
        return 1
    fi

    # 均等配置の計算
    local margin=$(get_tmux_panning_option "claude_voice_pan_margin" "0.1")
    local usable_range
    usable_range=$(echo "scale=3; 2 * (1 - $margin)" | bc 2>/dev/null || echo "1.8")
    
    local spacing
    if [[ $total_claude_windows -gt 1 ]]; then
        spacing=$(echo "scale=3; $usable_range / ($total_claude_windows - 1)" | bc 2>/dev/null || echo "0.9")
    else
        spacing=0
    fi
    
    local start_position
    start_position=$(echo "scale=3; -1 + $margin" | bc 2>/dev/null || echo "-0.9")
    
    local position
    position=$(echo "scale=3; $start_position + $target_index * $spacing" | bc 2>/dev/null || echo "0.0")
    
    log_debug "均等配置計算結果: margin=$margin, usable_range=$usable_range, spacing=$spacing, start=$start_position, index=$target_index, position=$position"
    
    echo "$position"
}

# ウィンドウ位置に基づく音像位置を計算する関数
calculate_pan_position() {
    local target_window="$1"
    local dynamic_enabled=$(get_tmux_panning_option "claude_voice_pan_dynamic" "true")

    log_debug "音像位置計算: window=$target_window, dynamic=$dynamic_enabled"

    if [[ "$dynamic_enabled" == "true" ]]; then
        # 動的配置を使用
        local position
        position=$(calculate_equal_spacing "$target_window")
        
        local min_distance=$(get_tmux_panning_option "claude_voice_pan_min_distance" "0.15")

        # 最小距離を確保するための調整
        local adjusted_position
        adjusted_position=$(echo "scale=3; $position * (1 - $min_distance/2)" | bc 2>/dev/null || echo "$position")
        
        log_debug "動的音像位置: 基本位置=$position, 調整位置=$adjusted_position"
        echo "$adjusted_position"
    else
        # 固定配置を使用（簡易版）
        echo "0.0"  # 中央固定
    fi
}

# Equal Power Pan Law対応のゲイン計算
calculate_pan_gains() {
    local pan_position="$1"  # -1.0 ～ +1.0
    local pan_law=$(get_tmux_panning_option "claude_voice_pan_law" "equal_power")
    
    log_debug "パンゲイン計算: position=$pan_position, law=$pan_law"

    if [[ "$pan_law" == "equal_power" ]]; then
        # Equal Power Pan Law: cos/sin関数を使用
        local angle
        angle=$(echo "scale=6; ($pan_position + 1) * 3.14159 / 4" | bc -l 2>/dev/null)
        
        if [[ $? -eq 0 && -n "$angle" ]]; then
            local left_gain
            left_gain=$(echo "scale=6; c($angle)" | bc -l 2>/dev/null || echo "0.707")
            local right_gain
            right_gain=$(echo "scale=6; s($angle)" | bc -l 2>/dev/null || echo "0.707")
            
            log_debug "Equal Power計算: angle=$angle, left=$left_gain, right=$right_gain"
            echo "$left_gain $right_gain"
        else
            # bc -l が利用できない場合のフォールバック
            log_debug "bc -lが利用できません。線形補間を使用します"
            local left_gain
            left_gain=$(echo "scale=6; (1 - $pan_position) / 2" | bc 2>/dev/null || echo "0.5")
            local right_gain
            right_gain=$(echo "scale=6; (1 + $pan_position) / 2" | bc 2>/dev/null || echo "0.5")
            echo "$left_gain $right_gain"
        fi
    else
        # Linear Pan Law: 線形補間
        local left_gain
        left_gain=$(echo "scale=6; (1 - $pan_position) / 2" | bc 2>/dev/null || echo "0.5")
        local right_gain
        right_gain=$(echo "scale=6; (1 + $pan_position) / 2" | bc 2>/dev/null || echo "0.5")
        
        log_debug "線形パンニング: left=$left_gain, right=$right_gain"
        echo "$left_gain $right_gain"
    fi
}

# デシベルパンニングを適用してファイルを再生
apply_panning() {
    local input_file="$1"
    local pan_position="$2"
    local os_type=$(get_os_type)
    
    if [[ -z "$input_file" || -z "$pan_position" ]]; then
        log_error "パンニング適用: パラメータが不足しています (file=$input_file, position=$pan_position)"
        return 1
    fi
    
    if [[ ! -f "$input_file" ]]; then
        log_error "入力ファイルが見つかりません: $input_file"
        return 1
    fi

    log_debug "パンニング適用: file=$input_file, position=$pan_position, OS=$os_type"

    # パンゲインを計算
    local gains
    gains=$(calculate_pan_gains "$pan_position")
    local left_gain=$(echo "$gains" | cut -d' ' -f1)
    local right_gain=$(echo "$gains" | cut -d' ' -f2)
    
    log_debug "適用ゲイン: left=$left_gain, right=$right_gain"

    # ffplayの存在確認
    if ! command -v ffplay >/dev/null 2>&1; then
        log_error "ffplayコマンドが見つかりません。apt install ffmpeg または brew install ffmpeg を実行してください"
        return 1
    fi

    if [[ "$os_type" == "Darwin" ]]; then
        # macOS: ffplay使用（リアルタイム再生）
        local volume=$(get_tmux_panning_option "claude_voice_volume_macos" "0.8")
        local volume_percent
        volume_percent=$(echo "$volume * 100" | bc 2>/dev/null || echo "80")
        
        log_debug "macOS再生: volume=$volume_percent%, left=$left_gain, right=$right_gain"
        
        ffplay -af "pan=stereo|c0=${left_gain}*c0|c1=${right_gain}*c0" \
               -volume "$volume_percent" \
               "$input_file" \
               -nodisp -autoexit 2>/dev/null &
        local ffplay_pid=$!
        log_debug "ffplay開始 (PID: $ffplay_pid)"
    else
        # WSL/Linux: PowerShell経由でWindowsネイティブ機能を使用（優先）またはffplay
        local volume=$(get_tmux_panning_option "claude_voice_volume_wsl" "80")
        
        # PowerShellが利用可能な場合はWindowsネイティブ機能を使用
        if command -v get_powershell_path >/dev/null 2>&1; then
            local powershell_path
            if powershell_path=$(get_powershell_path); then
                # Windowsネイティブの音声再生（パンニングなし）
                local windows_path=$(echo "$input_file" | sed 's|/mnt/c/|C:/|g' | sed 's|/|\\|g')
                log_debug "WSL再生 (PowerShell): volume=$volume%, file=$windows_path"
                
                "$powershell_path" -Command "
                    try {
                        Add-Type -AssemblyName System.Windows.Forms
                        \$player = New-Object System.Media.SoundPlayer
                        \$player.SoundLocation = '$windows_path'
                        \$player.Play()
                    } catch {
                        Write-Error 'Failed to play sound: \$_'
                    }
                " 2>/dev/null &
                local ps_pid=$!
                log_debug "PowerShell再生開始 (PID: $ps_pid)"
            else
                log_debug "PowerShellが見つからないためffplayを使用"
                goto_ffplay
            fi
        else
            log_debug "get_powershell_path関数が見つからないためffplayを使用"
            goto_ffplay
        fi
    fi
}

# ffplayを使用した再生（フォールバック）
goto_ffplay() {
    local volume=$(get_tmux_panning_option "claude_voice_volume_wsl" "80")
    
    log_debug "Linux再生 (ffplay): volume=$volume%, left=$left_gain, right=$right_gain"
    
    ffplay -af "pan=stereo|c0=${left_gain}*c0|c1=${right_gain}*c0" \
           -volume "$volume" \
           "$input_file" \
           -nodisp -autoexit 2>/dev/null &
    local ffplay_pid=$!
    log_debug "ffplay開始 (PID: $ffplay_pid)"
}

# ウィンドウ識別用の音声通知を生成する関数
create_window_identified_sound() {
    local target_window="$1"    # session:window形式
    local sound_type="$2"       # start, complete, waiting, error
    
    if [[ -z "$target_window" || -z "$sound_type" ]]; then
        log_error "ウィンドウ識別音声: パラメータが不足しています (window=$target_window, type=$sound_type)"
        return 1
    fi

    log_debug "ウィンドウ識別音声作成: window=$target_window, type=$sound_type"

    # パンニングが有効かチェック
    local panning_enabled=$(get_tmux_panning_option "claude_voice_panning_enabled" "true")
    
    if [[ "$panning_enabled" != "true" ]]; then
        log_debug "パンニング無効のため通常再生を実行"
        # パンニングが無効の場合は通常の通知音再生
        if command -v play_notification_sound >/dev/null 2>&1; then
            play_notification_sound "$sound_type"
        else
            log_error "play_notification_sound関数が見つかりません"
        fi
        return $?
    fi

    # 音像位置を計算
    local pan_position
    pan_position=$(calculate_pan_position "$target_window")
    
    if [[ $? -ne 0 ]]; then
        log_error "音像位置の計算に失敗しました"
        return 1
    fi

    # 通知音ファイルを取得
    local sound_file
    if command -v get_system_sound_path >/dev/null 2>&1; then
        # デフォルトの通知音名を決定
        local sound_name
        case "$sound_type" in
            "start") sound_name="Submarine" ;;
            "complete") sound_name="Funk" ;;
            "waiting") sound_name="Basso" ;;
            "error") sound_name="Basso" ;;
            *) sound_name="Submarine" ;;
        esac
        
        sound_file=$(get_system_sound_path "$sound_name")
    else
        log_error "get_system_sound_path関数が見つかりません"
        return 1
    fi

    if [[ $? -ne 0 || -z "$sound_file" ]]; then
        log_error "通知音ファイルの取得に失敗しました"
        return 1
    fi

    log_debug "ウィンドウ識別音声: file=$sound_file, pan=$pan_position"

    # デシベルパンニングを適用してリアルタイム再生
    apply_panning "$sound_file" "$pan_position"
}

# パンニングエンジンのテスト
test_panning_engine() {
    local test_type="${1:-all}"
    
    echo "=== デシベルパンニングエンジンテスト開始 ($test_type) ==="
    
    # ウィンドウ検出テスト
    if [[ "$test_type" == "all" || "$test_type" == "windows" ]]; then
        echo "ウィンドウ検出テスト..."
        local claude_windows
        claude_windows=$(detect_claude_windows_for_panning)
        local count
        count=$(count_claude_windows)
        
        echo "検出されたClaudeウィンドウ数: $count"
        if [[ -n "$claude_windows" ]]; then
            echo "$claude_windows"
            echo "✓ ウィンドウ検出: 成功"
        else
            echo "✓ ウィンドウ検出: Claudeウィンドウなし"
        fi
    fi
    
    # 配置計算テスト
    if [[ "$test_type" == "all" || "$test_type" == "positioning" ]]; then
        echo "配置計算テスト..."
        
        # テストウィンドウのリストを作成
        local test_windows=("test:0" "test:1" "test:2")
        
        for window in "${test_windows[@]}"; do
            local position
            position=$(calculate_equal_spacing "$window")
            local pan_position
            pan_position=$(calculate_pan_position "$window")
            
            echo "ウィンドウ $window: 均等配置=$position, パン位置=$pan_position"
        done
        
        echo "✓ 配置計算: 成功"
    fi
    
    # パンゲイン計算テスト
    if [[ "$test_type" == "all" || "$test_type" == "gains" ]]; then
        echo "パンゲイン計算テスト..."
        
        local test_positions=("-1.0" "-0.5" "0.0" "0.5" "1.0")
        
        for position in "${test_positions[@]}"; do
            local gains
            gains=$(calculate_pan_gains "$position")
            echo "位置 $position: ゲイン=$gains"
        done
        
        echo "✓ パンゲイン計算: 成功"
    fi
    
    # 依存関係テスト
    if [[ "$test_type" == "all" || "$test_type" == "deps" ]]; then
        echo "依存関係テスト..."
        
        local missing_deps=()
        
        if ! command -v ffplay >/dev/null 2>&1; then
            missing_deps+=("ffplay")
        fi
        
        if ! command -v bc >/dev/null 2>&1; then
            missing_deps+=("bc")
        fi
        
        if [[ ${#missing_deps[@]} -eq 0 ]]; then
            echo "✓ 依存関係: すべて満たされています"
        else
            echo "✗ 依存関係: 不足しているコマンド: ${missing_deps[*]}"
        fi
    fi
    
    echo "=== デシベルパンニングエンジンテスト完了 ==="
    return 0
}

# パンニングエンジンの依存関係チェック
check_panning_dependencies() {
    local missing_deps=()
    
    echo "パンニングエンジン依存関係チェック..."
    
    # 必須コマンドのチェック
    if ! command -v ffplay >/dev/null 2>&1; then
        missing_deps+=("ffplay (ffmpeg package)")
    fi
    
    if ! command -v bc >/dev/null 2>&1; then
        missing_deps+=("bc (math calculator)")
    fi
    
    # tmuxコマンドのチェック
    if ! command -v tmux >/dev/null 2>&1; then
        missing_deps+=("tmux")
    fi
    
    if [[ ${#missing_deps[@]} -eq 0 ]]; then
        echo "✓ すべての依存関係が満たされています"
        return 0
    else
        echo "✗ 不足している依存関係:"
        for dep in "${missing_deps[@]}"; do
            echo "  - $dep"
        done
        
        echo ""
        echo "インストール方法:"
        echo "  Ubuntu/Debian: sudo apt-get install ffmpeg bc tmux"
        echo "  macOS: brew install ffmpeg bc tmux"
        
        return 1
    fi
}

# スクリプト実行時の処理
if [[ "${BASH_SOURCE[0]}" == "${0}" ]]; then
    case "${1:-test}" in
        "test")
            TMUX_CLAUDE_VOICE_DEBUG=1 test_panning_engine "all"
            ;;
        "test-windows")
            test_panning_engine "windows"
            ;;
        "test-positioning")
            test_panning_engine "positioning"
            ;;
        "test-gains")
            test_panning_engine "gains"
            ;;
        "test-deps")
            test_panning_engine "deps"
            ;;
        "deps")
            check_panning_dependencies
            ;;
        "pan")
            # 使用例: ./panning_engine.sh pan /path/to/sound.wav -0.5
            apply_panning "${2:-}" "${3:-0.0}"
            ;;
        "identify")
            # 使用例: ./panning_engine.sh identify session:window complete
            create_window_identified_sound "${2:-test:0}" "${3:-complete}"
            ;;
        *)
            echo "使用方法: $0 [test|test-windows|test-positioning|test-gains|test-deps|deps|pan|identify]"
            echo "  test             - 全テスト実行"
            echo "  test-windows     - ウィンドウ検出テスト"
            echo "  test-positioning - 配置計算テスト"
            echo "  test-gains       - パンゲイン計算テスト"
            echo "  test-deps        - 依存関係テスト"
            echo "  deps             - 依存関係チェック"
            echo "  pan <file> <pos> - パンニング適用テスト"
            echo "  identify <win> <type> - ウィンドウ識別音声テスト"
            exit 1
            ;;
    esac
fi