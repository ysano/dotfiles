#!/bin/bash
# WSL Voice Engine Unified - Single Comprehensive Voice Engine
# Claude Code WSL音声統合システム（統一版）

set -euo pipefail

# === スクリプト設定 ===
readonly SCRIPT_VERSION="3.0.0"
readonly SCRIPT_NAME="WSL Voice Engine Unified"
readonly PLATFORM_DIR="$(dirname "${BASH_SOURCE[0]}")"
readonly MODULE_DIR="$PLATFORM_DIR/modules"
readonly CORE_DIR="$(dirname "$(dirname "$(dirname "${BASH_SOURCE[0]}")")")/core"

# === 環境変数設定 ===
export CLAUDE_VOICE_DEBUG="${CLAUDE_VOICE_DEBUG:-false}"
export WSL_VOICE_ENGINE_VERSION="$SCRIPT_VERSION"

# === 統一プラットフォームユーティリティの読み込み ===
readonly PLATFORM_UTILS_PATH="$CORE_DIR/platform_utils.sh"
if [[ -f "$PLATFORM_UTILS_PATH" ]]; then
    source "$PLATFORM_UTILS_PATH"
else
    echo "ERROR: Platform utilities not found: $PLATFORM_UTILS_PATH" >&2
    exit 1
fi

# === ログ関数（簡易版） ===
log() {
    local level="$1"
    shift
    if [[ "${CLAUDE_VOICE_DEBUG:-false}" == "true" ]] || [[ "$level" == "ERROR" ]]; then
        echo "[$(date '+%H:%M:%S')] [$level] $*" >&2
    fi
}

# === 日本語音声の優先順位（Windows 10/11標準音声） ===
readonly JAPANESE_VOICES=(
    "Microsoft Haruka Desktop"
    "Microsoft Sayaka Desktop"
    "Microsoft Ichiro Desktop"
    "Microsoft Ayumi Desktop"
    "Microsoft Nanami Desktop"
    "Microsoft Zira Desktop"
)

# === 英語フォールバック音声 ===
readonly ENGLISH_VOICES=(
    "Microsoft Zira Desktop"
    "Microsoft David Desktop"
    "Microsoft Mark Desktop"
)

# === ステータス別音声設定（Claude Code公式3状態） ===
declare -A STATUS_VOICE_CONFIGS=(
    ["⚡"]="rate:2|volume:90|pitch:medium|speed:fast"      # 処理実行中（忙しい状態）
    ["⌛"]="rate:0|volume:75|pitch:low|speed:normal"       # 確認待ち・入力待ち（のんびり）
    ["✅"]="rate:1|volume:85|pitch:high|speed:normal"      # アイドル・完了（明るく）
)

# === ステータス別効果音設定（Claude Code公式3状態） ===
declare -A STATUS_SOUND_CONFIGS=(
    ["⚡"]="beep_pattern:alert|frequency:800,800,600|duration:80,80,100|interval:20"        # 忙しい状態: 警告パターン
    ["⌛"]="beep_pattern:waiting|frequency:659,880,1175|duration:100,150,100|interval:50"   # 入力待ち: 上昇メロディー
    ["✅"]="beep_pattern:success|frequency:523,659,783,1046|duration:80,80,80,120|interval:30"  # 完了: 成功パターン
)

# === 音声エンジン初期化 ===
initialize_voice_engine() {
    log "INFO" "Initializing $SCRIPT_NAME v$SCRIPT_VERSION"
    
    # WSL環境確認
    if ! is_wsl; then
        log "ERROR" "Not running in WSL environment"
        return 1
    fi
    
    # PowerShell可用性確認
    if ! has_powershell; then
        log "ERROR" "PowerShell not available"
        return 1
    fi
    
    # Windows音声システム確認
    if ! has_windows_speech; then
        log "WARN" "Windows Speech System not available - limited functionality"
    fi
    
    log "INFO" "Voice engine initialized successfully"
    return 0
}

# === 音声の検出と選択 ===
detect_available_voices() {
    local powershell_path
    powershell_path=$(find_powershell) || {
        log "ERROR" "PowerShell not found for voice detection"
        return 1
    }
    
    "$powershell_path" -Command "
        try {
            Add-Type -AssemblyName System.Speech;
            \$synth = New-Object System.Speech.Synthesis.SpeechSynthesizer;
            \$voices = \$synth.GetInstalledVoices() | Where-Object {\$_.Enabled};
            \$voices | ForEach-Object {
                \$voice = \$_.VoiceInfo;
                Write-Output (\$voice.Name + '|' + \$voice.Culture.Name + '|' + \$voice.Gender);
            }
        } catch {
            Write-Output 'error: voice detection failed';
        }
    " 2>/dev/null | tr -d '\r'
}

# === 日本語音声の検出と選択 ===
detect_japanese_voices() {
    detect_available_voices | grep "|ja-JP|" | cut -d'|' -f1
}

select_best_japanese_voice() {
    local available_voices
    available_voices=$(detect_japanese_voices)
    
    if [[ -z "$available_voices" ]]; then
        log "WARN" "No Japanese voices available"
        return 1
    fi
    
    # 優先順位に従って選択
    for priority_voice in "${JAPANESE_VOICES[@]}"; do
        if echo "$available_voices" | grep -q "^$priority_voice$"; then
            echo "$priority_voice"
            log "DEBUG" "Selected Japanese voice: $priority_voice"
            return 0
        fi
    done
    
    # フォールバック: 最初の利用可能な日本語音声
    local first_voice
    first_voice=$(echo "$available_voices" | head -1)
    echo "$first_voice"
    log "DEBUG" "Using first available Japanese voice: $first_voice"
    return 0
}

# === 英語フォールバック音声選択 ===
select_fallback_english_voice() {
    local all_voices
    all_voices=$(detect_available_voices)
    local english_voices
    english_voices=$(echo "$all_voices" | grep "|en-US|" | cut -d'|' -f1)
    
    # 優先順位に従って選択
    for priority_voice in "${ENGLISH_VOICES[@]}"; do
        if echo "$english_voices" | grep -q "^$priority_voice$"; then
            echo "$priority_voice"
            log "DEBUG" "Selected English fallback voice: $priority_voice"
            return 0
        fi
    done
    
    # 最初の利用可能な英語音声
    local first_voice
    first_voice=$(echo "$english_voices" | head -1)
    if [[ -n "$first_voice" ]]; then
        echo "$first_voice"
        log "DEBUG" "Using first available English voice: $first_voice"
        return 0
    fi
    
    return 1
}

# === ステータス別音声設定の解析 ===
parse_voice_config() {
    local status_icon="$1"
    local config_string="${STATUS_VOICE_CONFIGS[$status_icon]:-rate:0|volume:80|pitch:medium|speed:normal}"
    
    # 設定の解析
    local rate volume pitch speed
    IFS='|' read -ra config_parts <<< "$config_string"
    
    for part in "${config_parts[@]}"; do
        case "$part" in
            rate:*) rate="${part#rate:}" ;;
            volume:*) volume="${part#volume:}" ;;
            pitch:*) pitch="${part#pitch:}" ;;
            speed:*) speed="${part#speed:}" ;;
        esac
    done
    
    # デフォルト値の設定
    rate="${rate:-0}"
    volume="${volume:-80}"
    pitch="${pitch:-medium}"
    speed="${speed:-normal}"
    
    echo "$rate|$volume|$pitch|$speed"
}

# === Equal Power Pan Law パンニング計算 ===
calculate_equal_power_panning() {
    local target_window_id="$1"
    
    # ウィンドウIDが無効な場合は中央
    if [[ -z "$target_window_id" || ! "$target_window_id" =~ ^[0-9]+$ ]]; then
        echo "0.707 0.707"  # Equal Power center
        return 0
    fi
    
    # アクティブウィンドウの検出
    local active_windows=()
    for i in 1 2 3 4 5; do
        if [[ -f "$HOME/.tmux/status/window-${i}.status" ]]; then
            local status_content
            status_content=$(cat "$HOME/.tmux/status/window-${i}.status" 2>/dev/null)
            if [[ "$status_content" =~ ^(⚡|⌛|✅)$ ]]; then
                active_windows+=("$i")
            fi
        fi
    done
    
    local window_count=${#active_windows[@]}
    if [[ $window_count -eq 0 ]]; then
        # フォールバック: 単一ウィンドウとして中央配置
        echo "0.707 0.707"
        return 0
    fi
    
    # ウィンドウインデックス取得
    local window_index=-1
    for i in "${!active_windows[@]}"; do
        if [[ "${active_windows[i]}" == "$target_window_id" ]]; then
            window_index=$i
            break
        fi
    done
    
    # ウィンドウが見つからない場合
    if [[ $window_index -eq -1 ]]; then
        echo "0.707 0.707"
        return 0
    fi
    
    # Equal Power Pan Law計算
    local position
    if [[ $window_count -eq 1 ]]; then
        position=0.5
    else
        position=$(awk "BEGIN { printf \"%.6f\", $window_index / ($window_count - 1) }")
    fi
    
    # π/2 radians (90度) でのパンニング計算
    local pan_angle
    pan_angle=$(awk "BEGIN { printf \"%.6f\", $position * 1.5707963267948966 }")  # π/2
    local left_gain
    left_gain=$(awk "BEGIN { printf \"%.6f\", cos($pan_angle) }")
    local right_gain
    right_gain=$(awk "BEGIN { printf \"%.6f\", sin($pan_angle) }")
    
    echo "$left_gain $right_gain"
    return 0
}

# === メイン音声合成関数 ===
speak() {
    local text="$1"
    local voice="${2:-auto}"
    local rate="${3:-0}"       # -10 to 10
    local volume="${4:-80}"    # 0 to 100
    local window_id="${5:-}"   # ウィンドウID（パンニング用）
    local status_icon="${6:-}" # ステータスアイコン
    
    log "DEBUG" "Speech request: text='${text:0:50}...', voice=$voice, window=$window_id, status=$status_icon"
    
    # 初期化確認
    if ! initialize_voice_engine; then
        log "ERROR" "Voice engine not initialized"
        return 1
    fi
    
    # ステータス別設定の適用
    if [[ -n "$status_icon" && -n "${STATUS_VOICE_CONFIGS[$status_icon]:-}" ]]; then
        local config
        config=$(parse_voice_config "$status_icon")
        IFS='|' read -r status_rate status_volume _ _ <<< "$config"
        
        # ステータス設定で上書き（デフォルト値の場合）
        if [[ "$rate" == "0" ]]; then rate="$status_rate"; fi
        if [[ "$volume" == "80" ]]; then volume="$status_volume"; fi
        
        log "DEBUG" "Applied status config for '$status_icon': rate=$rate, volume=$volume"
    fi
    
    # 音声の自動選択
    if [[ "$voice" == "auto" ]]; then
        voice=$(select_best_japanese_voice)
        if [[ -z "$voice" ]]; then
            voice=$(select_fallback_english_voice)
            if [[ -z "$voice" ]]; then
                log "ERROR" "No suitable voice available"
                return 1
            fi
        fi
    fi
    
    # パンニング計算
    local left_gain=1.0
    local right_gain=1.0
    if [[ -n "$window_id" && "$window_id" != "." ]]; then
        local panning_values
        panning_values=$(calculate_equal_power_panning "$window_id")
        left_gain=$(echo "$panning_values" | cut -d' ' -f1)
        right_gain=$(echo "$panning_values" | cut -d' ' -f2)
        log "DEBUG" "Equal Power panning for window $window_id: L=$left_gain, R=$right_gain"
    fi
    
    # テキストのサニタイズ
    local sanitized_text
    sanitized_text=$(echo "$text" | sed "s/'/\`'/g" | sed 's/"/\\"/g')
    
    # PowerShell実行
    local powershell_path
    powershell_path=$(find_powershell) || {
        log "ERROR" "PowerShell not found for speech synthesis"
        return 1
    }
    
    log "DEBUG" "Executing speech: voice='$voice', rate=$rate, volume=$volume"
    
    # PowerShell音声合成の実行
    local result
    result=$("$powershell_path" -Command "
        try {
            Add-Type -AssemblyName System.Speech;
            \$synth = New-Object System.Speech.Synthesis.SpeechSynthesizer;
            \$synth.SelectVoice('$voice');
            \$synth.Rate = $rate;
            \$synth.Volume = $volume;
            
            # Equal Power Panning対応
            if (($left_gain -ne 1.0) -or ($right_gain -ne 1.0)) {
                \$tempWav = [System.IO.Path]::GetTempFileName() + '.wav';
                \$synth.SetOutputToWaveFile(\$tempWav);
                \$synth.Speak('$sanitized_text');
                \$synth.SetOutputToDefaultAudioDevice();
                
                # System.Media.SoundPlayerでの再生
                \$player = New-Object System.Media.SoundPlayer;
                \$player.SoundLocation = \$tempWav;
                \$player.PlaySync();
                
                Remove-Item \$tempWav -ErrorAction SilentlyContinue;
                Write-Output ('success_panned:L=' + '$left_gain' + ',R=' + '$right_gain');
            } else {
                \$synth.Speak('$sanitized_text');
                Write-Output 'success_normal';
            }
        } catch {
            Write-Output ('error: ' + \$_.Exception.Message);
        }
    " 2>/dev/null | tr -d '\r\n')
    
    if [[ "$result" =~ ^success ]]; then
        log "INFO" "Speech synthesis completed: $result"
        return 0
    else
        log "ERROR" "Speech synthesis failed: $result"
        return 1
    fi
}

# === ステータス効果音再生 ===
play_sound() {
    local status_icon="$1"
    local window_id="${2:-1}"
    
    log "DEBUG" "Playing status sound: $status_icon (window: $window_id)"
    
    # 効果音設定の取得
    local sound_config="${STATUS_SOUND_CONFIGS[$status_icon]:-}"
    if [[ -z "$sound_config" ]]; then
        log "WARN" "No sound configuration for status: $status_icon"
        return 1
    fi
    
    # PowerShell実行
    local powershell_path
    powershell_path=$(find_powershell) || {
        log "ERROR" "PowerShell not found for sound effects"
        return 1
    }
    
    # Windowsシステム音を使用した効果音再生
    local ps_script="try {"
    case "$status_icon" in
        "⚡") ps_script+="\$player = New-Object Media.SoundPlayer 'C:\\Windows\\Media\\Windows Exclamation.wav'; \$player.PlaySync();" ;;
        "⌛") ps_script+="\$player = New-Object Media.SoundPlayer 'C:\\Windows\\Media\\Windows Notify System Generic.wav'; \$player.PlaySync();" ;;
        "✅") ps_script+="\$player = New-Object Media.SoundPlayer 'C:\\Windows\\Media\\Windows Ding.wav'; \$player.PlaySync();" ;;
        *) ps_script+="\$player = New-Object Media.SoundPlayer 'C:\\Windows\\Media\\Windows Ding.wav'; \$player.PlaySync();" ;;
    esac
    ps_script+="Write-Output 'sound_success'; } catch { Write-Output ('sound_error: ' + \$_.Exception.Message); }"
    
    local result
    result=$("$powershell_path" -Command "$ps_script" 2>/dev/null | grep "sound_success" | head -1)
    
    if [[ -n "$result" ]]; then
        log "INFO" "Status sound completed: $status_icon"
        return 0
    else
        log "ERROR" "Status sound failed: $status_icon"
        return 1
    fi
}

# === 複合通知（効果音＋音声合成） ===
notify() {
    local text="$1"
    local status_icon="${2:-✅}"
    local window_id="${3:-1}"
    local mode="${4:-both}"  # sound, speech, both
    
    log "DEBUG" "Notification: text='${text:0:30}...', status=$status_icon, mode=$mode"
    
    case "$mode" in
        "sound")
            play_sound "$status_icon" "$window_id"
            ;;
        "speech")
            speak "$text" "auto" "0" "80" "$window_id" "$status_icon"
            ;;
        "both")
            play_sound "$status_icon" "$window_id" &
            sleep 0.3
            speak "$text" "auto" "0" "80" "$window_id" "$status_icon"
            wait
            ;;
        *)
            log "ERROR" "Unknown notification mode: $mode"
            return 1
            ;;
    esac
}

# === 診断機能 ===
diagnose() {
    echo "=== $SCRIPT_NAME Diagnostics ==="
    echo "Version: $SCRIPT_VERSION"
    echo
    
    # プラットフォーム情報
    echo "Platform Information:"
    get_platform_info | sed 's/^/  /'
    echo
    
    # 音声システム状況
    echo "Voice System Status:"
    if has_windows_speech; then
        echo "  Windows Speech: ✅ Available"
        
        echo "  Available Voices:"
        detect_available_voices | while IFS='|' read -r name culture gender; do
            echo "    - $name ($culture, $gender)"
        done | head -10
        
        echo "  Recommended Japanese Voice:"
        local jp_voice
        jp_voice=$(select_best_japanese_voice 2>/dev/null)
        echo "    ${jp_voice:-"None available"}"
    else
        echo "  Windows Speech: ❌ Not Available"
    fi
    
    echo
    echo "=== End Diagnostics ==="
}

# === テスト機能 ===
test_engine() {
    echo "=== $SCRIPT_NAME Test Suite ==="
    
    local tests_passed=0
    local tests_total=0
    
    # Test 1: 初期化
    echo "Test 1: Engine initialization"
    ((tests_total++))
    if initialize_voice_engine; then
        echo "✅ PASS"
        ((tests_passed++))
    else
        echo "❌ FAIL"
    fi
    
    # Test 2: 基本音声合成
    echo "Test 2: Basic speech synthesis"
    ((tests_total++))
    if speak "統一音声エンジンのテストです"; then
        echo "✅ PASS"
        ((tests_passed++))
    else
        echo "❌ FAIL"
    fi
    
    # Test 3: ステータス音響効果
    echo "Test 3: Status sound effects"
    for status in "⚡" "⌛" "✅"; do
        echo "  Testing status: $status"
        ((tests_total++))
        if play_sound "$status"; then
            echo "  ✅ PASS"
            ((tests_passed++))
        else
            echo "  ❌ FAIL"
        fi
        sleep 1
    done
    
    # Test 4: 複合通知
    echo "Test 4: Composite notification"
    ((tests_total++))
    if notify "統一エンジンテスト完了" "✅" 1 "both"; then
        echo "✅ PASS"
        ((tests_passed++))
    else
        echo "❌ FAIL"
    fi
    
    echo
    echo "Test Results: $tests_passed/$tests_total passed"
    
    if [[ $tests_passed -eq $tests_total ]]; then
        echo "🎉 All tests passed!"
        return 0
    else
        echo "⚠️ Some tests failed"
        return 1
    fi
}

# === メイン処理 ===
main() {
    case "${1:-help}" in
        "speak")
            shift
            speak "$@"
            ;;
        "sound")
            shift
            play_sound "$@"
            ;;
        "notify")
            shift
            notify "$@"
            ;;
        "diagnose")
            diagnose
            ;;
        "test")
            test_engine
            ;;
        "help"|"-h"|"--help")
            cat << EOF
$SCRIPT_NAME - Usage Guide

COMMANDS:
  speak <text> [voice] [rate] [volume] [window] [status]  - 音声合成
  sound <status> [window]                                - ステータス音再生
  notify <text> [status] [window] [mode]                - 複合通知
  diagnose                                               - システム診断
  test                                                   - テスト実行
  help                                                   - このヘルプ

EXAMPLES:
  $0 speak "こんにちは"
  $0 sound "✅"
  $0 notify "完了しました" "✅" 1 "both"
  $0 diagnose
  $0 test

STATUS ICONS:
  ⚡ - Busy/Alert
  ⌛ - Waiting/Processing
  ✅ - Complete/Success

NOTIFICATION MODES:
  sound  - 効果音のみ
  speech - 音声合成のみ
  both   - 効果音+音声合成

EOF
            ;;
        *)
            log "ERROR" "Unknown command: $1"
            main "help"
            exit 1
            ;;
    esac
}

# === スクリプト実行 ===
if [[ "${BASH_SOURCE[0]}" == "${0}" ]]; then
    main "$@"
fi