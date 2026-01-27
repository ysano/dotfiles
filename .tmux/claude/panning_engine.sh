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

# 設定値取得は sound_utils.sh の get_tmux_sound_option() を使用

# Claude Codeウィンドウを検出する関数（既存のdetect_claude_windowsと重複回避）
detect_claude_windows_for_panning() {
    local pattern=$(get_tmux_sound_option "claude_voice_window_pattern" "Claude|claude|CLAUDE")
    
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
    local margin=$(get_tmux_sound_option "claude_voice_pan_margin" "0.1")
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
    local dynamic_enabled=$(get_tmux_sound_option "claude_voice_pan_dynamic" "true")

    log_debug "音像位置計算: window=$target_window, dynamic=$dynamic_enabled"

    if [[ "$dynamic_enabled" == "true" ]]; then
        # 動的配置を使用
        local position
        position=$(calculate_equal_spacing "$target_window")
        
        local min_distance=$(get_tmux_sound_option "claude_voice_pan_min_distance" "0.15")

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
    local pan_law=$(get_tmux_sound_option "claude_voice_pan_law" "equal_power")
    
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

# 読み上げ音声用のパンニング適用（音声タイプ補正付き）
apply_speech_panning() {
    local input_file="$1"
    local pan_position="$2"
    local os_type=$(get_os_type)
    
    if [[ -z "$input_file" || -z "$pan_position" ]]; then
        log_error "読み上げパンニング適用: パラメータが不足しています (file=$input_file, position=$pan_position)"
        return 1
    fi
    
    if [[ ! -f "$input_file" ]]; then
        log_error "読み上げ音声ファイルが見つかりません: $input_file"
        return 1
    fi

    log_debug "読み上げパンニング適用: file=$input_file, position=$pan_position, OS=$os_type"

    # パンゲインを計算
    local gains
    gains=$(calculate_pan_gains "$pan_position")
    local left_gain=$(echo "$gains" | cut -d' ' -f1)
    local right_gain=$(echo "$gains" | cut -d' ' -f2)
    
    log_debug "読み上げパンゲイン: left=$left_gain, right=$right_gain"

    # ffplayの存在確認
    if ! command -v ffplay >/dev/null 2>&1; then
        log_error "ffplayコマンドが見つかりません。apt install ffmpeg または brew install ffmpeg を実行してください"
        return 1
    fi

    if [[ "$os_type" == "Darwin" ]]; then
        # macOS: 読み上げ音声用ボリューム補正適用
        local base_volume
        if command -v get_normalized_volume >/dev/null 2>&1; then
            base_volume=$(get_normalized_volume)
        else
            base_volume="0.8"  # フォールバック値
        fi
        
        local corrected_volume
        if command -v apply_volume_correction >/dev/null 2>&1; then
            corrected_volume=$(apply_volume_correction "speech" "$base_volume")
        else
            corrected_volume="$base_volume"
        fi
        
        local volume_percent
        volume_percent=$(echo "scale=0; $corrected_volume * 100" | bc 2>/dev/null || echo "80")
        
        log_debug "macOS読み上げ再生: speech_volume=$corrected_volume, ffplay_volume=$volume_percent%, left=$left_gain, right=$right_gain"
        
        ffplay -af "pan=stereo|c0=${left_gain}*c0|c1=${right_gain}*c0" \
               -volume "$volume_percent" \
               "$input_file" \
               -nodisp -autoexit 2>/dev/null &
        local ffplay_pid=$!
        log_debug "読み上げffplay開始 (PID: $ffplay_pid)"
    else
        # WSL/Linux: フォールバック
        local volume=$(get_tmux_sound_option "claude_voice_volume_wsl" "80")
        
        log_debug "Linux読み上げ再生: volume=$volume%, left=$left_gain, right=$right_gain"
        
        ffplay -af "pan=stereo|c0=${left_gain}*c0|c1=${right_gain}*c0" \
               -volume "$volume" \
               "$input_file" \
               -nodisp -autoexit 2>/dev/null &
        local ffplay_pid=$!
        log_debug "読み上げffplay開始 (PID: $ffplay_pid)"
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
        # macOS: ffplay使用（リアルタイム再生・ボリューム正規化適用）
        local base_volume
        if command -v get_normalized_volume >/dev/null 2>&1; then
            base_volume=$(get_normalized_volume)
        else
            base_volume="0.8"  # フォールバック値
        fi
        
        local corrected_volume
        if command -v apply_volume_correction >/dev/null 2>&1; then
            corrected_volume=$(apply_volume_correction "notification" "$base_volume")
        else
            corrected_volume="$base_volume"
        fi
        
        local volume_percent
        volume_percent=$(echo "scale=0; $corrected_volume * 100" | bc 2>/dev/null || echo "80")
        
        log_debug "macOS再生: normalized_volume=$corrected_volume, ffplay_volume=$volume_percent%, left=$left_gain, right=$right_gain"
        
        ffplay -af "pan=stereo|c0=${left_gain}*c0|c1=${right_gain}*c0" \
               -volume "$volume_percent" \
               "$input_file" \
               -nodisp -autoexit 2>/dev/null &
        local ffplay_pid=$!
        log_debug "ffplay開始 (PID: $ffplay_pid)"
    else
        # WSL/Linux: PowerShell経由でWindowsネイティブ機能を使用（優先）またはffplay
        local volume=$(get_tmux_sound_option "claude_voice_volume_wsl" "80")
        
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
    local volume=$(get_tmux_sound_option "claude_voice_volume_wsl" "80")
    
    log_debug "Linux再生 (ffplay): volume=$volume%, left=$left_gain, right=$right_gain"
    
    ffplay -af "pan=stereo|c0=${left_gain}*c0|c1=${right_gain}*c0" \
           -volume "$volume" \
           "$input_file" \
           -nodisp -autoexit 2>/dev/null &
    local ffplay_pid=$!
    log_debug "ffplay開始 (PID: $ffplay_pid)"
}

# ウィンドウ識別用の音声通知を生成する関数
# パンニング音声合成の作成と再生
create_panned_speech() {
    local text="$1"             # 読み上げテキスト
    local target_window="$2"    # session:window形式
    local os_type=$(get_os_type)
    
    if [[ -z "$text" || -z "$target_window" ]]; then
        log_error "パンニング音声合成: パラメータが不足しています (text=$text, window=$target_window)"
        return 1
    fi

    log_debug "パンニング音声合成作成: text=$text, window=$target_window, OS=$os_type"

    # パンニングが有効かチェック
    local panning_enabled=$(get_tmux_sound_option "claude_voice_panning_enabled" "true")
    
    if [[ "$panning_enabled" != "true" ]]; then
        log_debug "パンニング無効のため通常の音声合成を実行"
        # パンニングが無効の場合は通常の音声合成
        if command -v speak_text >/dev/null 2>&1; then
            speak_text "$text"
        else
            log_error "speak_text関数が見つかりません"
        fi
        return $?
    fi

    # macOSの場合のみ対応（WSLは将来対応予定）
    if [[ "$os_type" != "Darwin" ]]; then
        log_debug "WSL環境のため通常の音声合成を実行"
        if command -v speak_text >/dev/null 2>&1; then
            speak_text "$text"
        else
            log_error "speak_text関数が見つかりません"
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

    # 一時音声ファイルを生成
    local temp_file="/tmp/claude_panning_speech_${target_window//[:\/]/_}_$$.aiff"
    
    # 音声設定の取得
    local speech_rate=$(get_tmux_sound_option "claude_voice_speech_rate" "200")
    local voice_name=$(get_tmux_sound_option "claude_voice_macos_voice" "Kyoko")
    local voice_quality=$(get_tmux_sound_option "claude_voice_voice_quality" "enhanced")

    # 音声品質に応じた音声名の選択
    if [[ "$voice_quality" == "enhanced" ]]; then
        case "$voice_name" in
            "Kyoko") voice_name="Kyoko Enhanced" ;;
            "Otoya") voice_name="Otoya Enhanced" ;;
        esac
    fi

    log_debug "音声合成設定: voice=$voice_name, rate=$speech_rate, temp_file=$temp_file"
    
    # sayコマンドで一時ファイルに音声を出力
    if ! command -v say >/dev/null 2>&1; then
        log_error "sayコマンドが見つかりません"
        return 1
    fi
    
    say -v "$voice_name" -r "$speech_rate" "$text" -o "$temp_file" 2>/dev/null
    
    if [[ $? -ne 0 || ! -f "$temp_file" ]]; then
        log_error "音声ファイルの生成に失敗しました: $temp_file"
        return 1
    fi

    log_debug "一時音声ファイル生成成功: $temp_file ($(ls -lh "$temp_file" 2>/dev/null | awk '{print $5}' || echo 'size unknown'))"

    # パンニング適用して再生
    apply_speech_panning "$temp_file" "$pan_position" &
    local panning_pid=$!
    log_debug "パンニング音声再生開始 (PID: $panning_pid)"

    # バックグラウンドで一時ファイルを削除（10秒後）
    (sleep 10 && rm -f "$temp_file" 2>/dev/null && log_debug "一時音声ファイル削除: $temp_file") &
}

# パンニング音声合成（直接位置指定版）
create_direct_panned_speech() {
    local text="$1"           # 読み上げテキスト
    local pan_position="$2"   # -1.0 to 1.0 のパンニング位置
    local os_type=$(get_os_type)
    
    if [[ -z "$text" || -z "$pan_position" ]]; then
        log_error "直接パンニング音声合成: パラメータが不足しています (text=$text, position=$pan_position)"
        return 1
    fi

    # パンニング位置の範囲チェック
    if ! echo "$pan_position" | grep -qE '^-?[0-9]*\.?[0-9]+$'; then
        log_error "パンニング位置が無効です: $pan_position (-1.0 ～ 1.0の範囲で指定してください)"
        return 1
    fi

    log_debug "直接パンニング音声合成: text=$text, position=$pan_position, OS=$os_type"

    # パンニングが有効かチェック
    local panning_enabled=$(get_tmux_sound_option "claude_voice_panning_enabled" "true")
    
    if [[ "$panning_enabled" != "true" ]]; then
        log_debug "パンニング無効のため通常の音声合成を実行"
        # パンニングが無効の場合は通常の音声合成
        if command -v speak_text >/dev/null 2>&1; then
            speak_text "$text"
        else
            log_error "speak_text関数が見つかりません"
        fi
        return $?
    fi

    # macOSの場合のみ対応（WSLは将来対応予定）
    if [[ "$os_type" != "Darwin" ]]; then
        log_debug "WSL環境のため通常の音声合成を実行"
        if command -v speak_text >/dev/null 2>&1; then
            speak_text "$text"
        else
            log_error "speak_text関数が見つかりません"
        fi
        return $?
    fi

    # 一時音声ファイルを生成
    local temp_file="/tmp/claude_direct_speech_${pan_position//[.-]/_}_$$.aiff"
    
    # 音声設定の取得
    local speech_rate=$(get_tmux_sound_option "claude_voice_speech_rate" "200")
    local voice_name=$(get_tmux_sound_option "claude_voice_macos_voice" "Kyoko")
    local voice_quality=$(get_tmux_sound_option "claude_voice_voice_quality" "enhanced")

    # 音声品質に応じた音声名の選択
    if [[ "$voice_quality" == "enhanced" ]]; then
        case "$voice_name" in
            "Kyoko") voice_name="Kyoko Enhanced" ;;
            "Otoya") voice_name="Otoya Enhanced" ;;
        esac
    fi

    log_debug "直接パンニング音声設定: voice=$voice_name, rate=$speech_rate, position=$pan_position"
    
    # sayコマンドで一時ファイルに音声を出力
    if ! command -v say >/dev/null 2>&1; then
        log_error "sayコマンドが見つかりません"
        return 1
    fi
    
    say -v "$voice_name" -r "$speech_rate" "$text" -o "$temp_file" 2>/dev/null
    
    if [[ $? -ne 0 || ! -f "$temp_file" ]]; then
        log_error "音声ファイルの生成に失敗しました: $temp_file"
        return 1
    fi

    log_debug "一時音声ファイル生成成功: $temp_file ($(ls -lh "$temp_file" 2>/dev/null | awk '{print $5}' || echo 'size unknown'))"

    # パンニング適用して再生
    apply_speech_panning "$temp_file" "$pan_position" &
    local panning_pid=$!
    log_debug "直接パンニング音声再生開始 (PID: $panning_pid)"

    # バックグラウンドで一時ファイルを削除（10秒後）
    (sleep 10 && rm -f "$temp_file" 2>/dev/null && log_debug "直接パンニング一時ファイル削除: $temp_file") &
}

create_window_identified_sound() {
    local target_window="$1"    # session:window形式
    local sound_type="$2"       # start, complete, waiting, error
    
    if [[ -z "$target_window" || -z "$sound_type" ]]; then
        log_error "ウィンドウ識別音声: パラメータが不足しています (window=$target_window, type=$sound_type)"
        return 1
    fi

    log_debug "ウィンドウ識別音声作成: window=$target_window, type=$sound_type"

    # パンニングが有効かチェック
    local panning_enabled=$(get_tmux_sound_option "claude_voice_panning_enabled" "true")
    
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
        # デフォルトの通知音名を決定（sound_utils.shと統一）
        local sound_name
        case "$sound_type" in
            "start") sound_name="Ping" ;;
            "complete") sound_name="Glass" ;;
            "waiting") sound_name="Funk" ;;
            "error") sound_name="Sosumi" ;;
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
        "speak")
            # 新しい使用例: ./panning_engine.sh speak "テキスト" [pan_position]
            # pan_position: -1.0 to 1.0 の小数点 (省略時は0.0=中央)
            if [[ "$3" =~ ^-?[0-9]*\.?[0-9]+$ ]]; then
                # 第3引数が小数点の場合は直接パンニング位置として使用
                create_direct_panned_speech "${2:-テストメッセージです}" "${3:-0.0}"
            else
                # 従来の互換性のため、session:window形式もサポート
                create_panned_speech "${2:-テストメッセージです}" "${3:-test:0}"
            fi
            ;;
        *)
            echo "使用方法: $0 [test|test-windows|test-positioning|test-gains|test-deps|deps|pan|identify|speak]"
            echo "  test             - 全テスト実行"
            echo "  test-windows     - ウィンドウ検出テスト"
            echo "  test-positioning - 配置計算テスト"
            echo "  test-gains       - パンゲイン計算テスト"
            echo "  test-deps        - 依存関係テスト"
            echo "  deps             - 依存関係チェック"
            echo "  pan <file> <pos> - パンニング適用テスト"
            echo "  identify <win> <type> - ウィンドウ識別音声テスト"
            echo "  speak <text> [pan_pos] - パンニング読み上げ (-1.0~1.0 or session:window)"
            exit 1
            ;;
    esac
fi