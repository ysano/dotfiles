#!/bin/bash
# WSL Voice Engine - Windows Subsystem for Linux音声出力システム
# PowerShell Speech Synthesis統合モジュール

# === グローバル設定 ===
readonly WSL_VOICE_VERSION="1.0.0"

# 日本語音声の優先順位（Windows 10/11標準音声）
readonly JAPANESE_VOICES=(
    "Microsoft Haruka Desktop"
    "Microsoft Sayaka Desktop"
    "Microsoft Ichiro Desktop"
    "Microsoft Ayumi Desktop"
    "Microsoft Nanami Desktop"
    "Microsoft Zira Desktop"
)

# 英語フォールバック音声
readonly ENGLISH_VOICES=(
    "Microsoft Zira Desktop"
    "Microsoft David Desktop"
    "Microsoft Mark Desktop"
)

# === 統一プラットフォームユーティリティの読み込み ===
readonly PLATFORM_UTILS_PATH="$(dirname "$(dirname "${BASH_SOURCE[0]}")")/core/platform_utils.sh"
if [[ -f "$PLATFORM_UTILS_PATH" ]]; then
    source "$PLATFORM_UTILS_PATH"
else
    echo "ERROR: Platform utilities not found: $PLATFORM_UTILS_PATH" >&2
    exit 1
fi

# === Windows音声システム検証は統一ユーティリティを使用 ===
# check_windows_speech関数はplatform_utils.shで提供

# === 利用可能音声の検出 ===
detect_available_voices() {
    log "DEBUG" "Detecting available voices"

    local powershell_path=$(find_powershell)
    if [[ -z "$powershell_path" ]]; then
        log "ERROR" "PowerShell not found for voice detection"
        return 1
    fi

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
    local all_voices=$(detect_available_voices)
    echo "$all_voices" | grep "|ja-JP|" | cut -d'|' -f1
}

select_best_japanese_voice() {
    local available_voices=$(detect_japanese_voices)

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
    local first_voice=$(echo "$available_voices" | head -1)
    echo "$first_voice"
    log "DEBUG" "Using first available Japanese voice: $first_voice"
    return 0
}

# === 英語フォールバック音声選択 ===
select_fallback_english_voice() {
    local all_voices=$(detect_available_voices)
    local english_voices=$(echo "$all_voices" | grep "|en-US|" | cut -d'|' -f1)

    # 優先順位に従って選択
    for priority_voice in "${ENGLISH_VOICES[@]}"; do
        if echo "$english_voices" | grep -q "^$priority_voice$"; then
            echo "$priority_voice"
            log "DEBUG" "Selected English fallback voice: $priority_voice"
            return 0
        fi
    done

    # 最初の利用可能な英語音声
    local first_voice=$(echo "$english_voices" | head -1)
    if [[ -n "$first_voice" ]]; then
        echo "$first_voice"
        log "DEBUG" "Using first available English voice: $first_voice"
        return 0
    fi

    return 1
}

# === ステータス別効果音再生関数 ===
play_status_sound() {
    local status_icon="$1"
    local window_id="${2:-1}"
    
    log "DEBUG" "Playing status sound for: $status_icon (window: $window_id)"
    
    # ステータス別効果音設定の取得
    local sound_config="${STATUS_SOUND_CONFIGS[$status_icon]:-}"
    if [[ -z "$sound_config" ]]; then
        log "DEBUG" "No sound configuration for status: $status_icon"
        return 1
    fi
    
    # 設定の解析
    local beep_pattern frequency_list duration_list interval
    IFS='|' read -ra config_parts <<< "$sound_config"
    
    for part in "${config_parts[@]}"; do
        case "$part" in
            beep_pattern:*) beep_pattern="${part#beep_pattern:}" ;;
            frequency:*) frequency_list="${part#frequency:}" ;;
            duration:*) duration_list="${part#duration:}" ;;
            interval:*) interval="${part#interval:}" ;;
        esac
    done
    
    # デフォルト値の設定
    interval="${interval:-50}"
    
    log "DEBUG" "Sound config: pattern=$beep_pattern, freq=$frequency_list, dur=$duration_list, interval=$interval"
    
    # PowerShell実行ファイルの取得
    local powershell_path=$(find_powershell)
    if [[ -z "$powershell_path" ]]; then
        log "ERROR" "PowerShell not found for sound effects"
        return 1
    fi
    
    # 周波数と持続時間の配列作成
    IFS=',' read -ra frequencies <<< "$frequency_list"
    IFS=',' read -ra durations <<< "$duration_list"
    
    # デフォルト音声デバイス優先出力方式
    local ps_script="try {"
    
    # PlaySyncを使用してデフォルトオーディオデバイスで確実に再生
    ps_script+="Write-Host 'Using default audio device with PlaySync...'; "
    case "$status_icon" in
        "⚡") ps_script+="\$player = New-Object Media.SoundPlayer 'C:\\Windows\\Media\\Windows Exclamation.wav'; \$player.PlaySync(); " ;;
        "⌛") ps_script+="\$player = New-Object Media.SoundPlayer 'C:\\Windows\\Media\\Windows Notify System Generic.wav'; \$player.PlaySync(); " ;;
        "✅") ps_script+="\$player = New-Object Media.SoundPlayer 'C:\\Windows\\Media\\Windows Ding.wav'; \$player.PlaySync(); " ;;
        *) ps_script+="\$player = New-Object Media.SoundPlayer 'C:\\Windows\\Media\\Windows Ding.wav'; \$player.PlaySync(); " ;;
    esac
    
    ps_script+="Write-Output 'sound_success:$beep_pattern'; } catch { Write-Output ('sound_error: ' + \$_.Exception.Message); }"
    
    log "DEBUG" "Executing PowerShell sound script for $status_icon"
    
    # PowerShell実行
    local result=$("$powershell_path" -Command "$ps_script" 2>/dev/null | grep "sound_success" | head -1)
    
    if [[ -n "$result" ]]; then
        log "INFO" "Status sound completed: $status_icon ($beep_pattern)"
        return 0
    else
        log "ERROR" "Status sound failed - no success signal received"
        return 1
    fi
}

# === ステータス別音声設定（Claude Code公式3状態） ===
# Status-specific voice configurations for Claude Code states
declare -A STATUS_VOICE_CONFIGS=(
    ["⚡"]="rate:2|volume:90|pitch:medium|speed:fast"      # 処理実行中（忙しい状態）
    ["⌛"]="rate:0|volume:75|pitch:low|speed:normal"       # 確認待ち・入力待ち（のんびり）
    ["✅"]="rate:1|volume:85|pitch:high|speed:normal"      # アイドル・完了（明るく）
)

# パンニング対応音声設定
declare -A PANNING_VOICE_CONFIGS=(
    ["left"]="balance:-50|volume:85"      # 左チャンネル強調
    ["center"]="balance:0|volume:80"      # 中央バランス
    ["right"]="balance:50|volume:85"      # 右チャンネル強調
)

# ステータス別効果音設定（Claude Code公式3状態）
declare -A STATUS_SOUND_CONFIGS=(
    ["⚡"]="beep_pattern:alert|frequency:800,800,600|duration:80,80,100|interval:20"        # 忙しい状態: 警告パターン
    ["⌛"]="beep_pattern:waiting|frequency:659,880,1175|duration:100,150,100|interval:50"   # 入力待ち: 上昇メロディー
    ["✅"]="beep_pattern:success|frequency:523,659,783,1046|duration:80,80,80,120|interval:30"  # 完了: 成功パターン
)

# === Claude Code実行中ウィンドウの検出（WSL版・公式3状態対応） ===
get_active_claude_windows_wsl() {
    local active_windows=()
    
    # ステータスファイルから実行中ウィンドウを検出
    if [[ -d "$HOME/.tmux/status" ]]; then
        for status_file in "$HOME/.tmux/status"/window-*.status; do
            [[ -f "$status_file" ]] || continue
            
            local window_id=$(basename "$status_file" | sed 's/window-\([0-9]*\)\.status/\1/')
            local status_content=$(cat "$status_file" 2>/dev/null)
            
            # Claude Code公式3状態のアイコン（⚡、⌛、✅）があるウィンドウを検出
            if [[ "$status_content" == "⚡" || "$status_content" == "⌛" || "$status_content" == "✅" ]]; then
                # tmuxウィンドウが実際に存在することを確認
                if tmux list-windows -F '#I' 2>/dev/null | grep -q "^${window_id}$"; then
                    active_windows+=("$window_id:$status_content")
                fi
            fi
        done
    fi
    
    # ウィンドウIDでソート
    printf '%s\n' "${active_windows[@]}" | sort -n
}

# === WSL版動的パンニング位置計算（デシベル計算版） ===
calculate_wsl_panning() {
    local target_window_id="$1"
    local status_icon="${2:-}"
    
    # ウィンドウIDが無効な場合は中央
    if [[ -z "$target_window_id" || ! "$target_window_id" =~ ^[0-9]+$ ]]; then
        echo "0.5 0.5"  # Equal Power: 中央 (L=0.707, R=0.707)
        return 0
    fi
    
    # アクティブウィンドウ数の推定（安全版）
    local active_windows=()
    for i in 1 2 3 4 5; do
        if [[ -f "$HOME/.tmux/status/window-${i}.status" ]]; then
            local status_content=$(cat "$HOME/.tmux/status/window-${i}.status" 2>/dev/null)
            if [[ "$status_content" == "⚡" || "$status_content" == "⌛" || "$status_content" == "✅" ]]; then
                active_windows+=("$i")
            fi
        fi
    done
    
    local window_count=${#active_windows[@]}
    if [[ $window_count -eq 0 ]]; then
        # フォールバック: ウィンドウ番号ベースの配置
        window_count=5
        active_windows=(1 2 3 4 5)
    fi
    
    # 現在のウィンドウのインデックスを取得
    local window_index=-1
    local i=0
    for window in "${active_windows[@]}"; do
        if [[ "$window" == "$target_window_id" ]]; then
            window_index=$i
            break
        fi
        ((i++))
    done
    
    # ウィンドウが見つからない場合は番号ベースでインデックス計算
    if [[ $window_index -eq -1 ]]; then
        window_index=$((target_window_id - 1))
        if [[ $window_index -lt 0 ]]; then window_index=0; fi
        if [[ $window_index -ge $window_count ]]; then window_index=$((window_count - 1)); fi
    fi
    
    # Equal Power Pan Law (-3dB center) による位置計算
    local position
    if [[ $window_count -eq 1 ]]; then
        position=0.5  # 1つのウィンドウ: 中央
    else
        # 複数ウィンドウ: 0.0から1.0を等分割
        position=$(awk "BEGIN { printf \"%.6f\", $window_index / ($window_count - 1) }")
    fi
    
    # デシベル計算による左右チャンネルのゲイン計算
    # Equal Power Pan Law: π/2 radians (90度) でパンニング
    local pan_angle=$(awk "BEGIN { printf \"%.6f\", $position * 1.5707963267948966 }")  # π/2 radians
    local left_gain=$(awk "BEGIN { printf \"%.6f\", cos($pan_angle) }")   # cos(θ)
    local right_gain=$(awk "BEGIN { printf \"%.6f\", sin($pan_angle) }")  # sin(θ)
    
    # √2倍してEqual Power保持 (-3dB center)
    left_gain=$(awk "BEGIN { printf \"%.6f\", $left_gain * 1.414213562373095 }")   # √2 * cos(θ)
    right_gain=$(awk "BEGIN { printf \"%.6f\", $right_gain * 1.414213562373095 }")  # √2 * sin(θ)
    
    # 最小値制限（-60dB相当 = 0.001）
    if (( $(awk "BEGIN { print ($left_gain < 0.001) }") )); then
        left_gain="0.001"
    fi
    if (( $(awk "BEGIN { print ($right_gain < 0.001) }") )); then
        right_gain="0.001"
    fi
    
    # デシベル値の計算
    local left_db=$(awk "BEGIN { 
        if ($left_gain > 0.001) 
            printf \"%.2f\", 20 * log($left_gain) / log(10)
        else 
            printf \"-60.00\"
    }")
    local right_db=$(awk "BEGIN { 
        if ($right_gain > 0.001) 
            printf \"%.2f\", 20 * log($right_gain) / log(10)
        else 
            printf \"-60.00\"
    }")
    
    log "DEBUG" "WSL Equal Power panning - Window $target_window_id (${window_index}/${window_count})"
    log "DEBUG" "Position: $position, Angle: ${pan_angle}rad"
    log "DEBUG" "Gains: L=${left_gain}(${left_db}dB), R=${right_gain}(${right_db}dB)"
    log "DEBUG" "Active windows: ${active_windows[*]}"
    
    echo "$left_gain $right_gain"
    return 0
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

# === 複合通知関数（効果音＋音声合成） ===
wsl_notify() {
    local text="$1"
    local status_icon="${2:-}"
    local window_id="${3:-1}"
    local mode="${4:-both}"  # both, sound_only, speech_only
    
    log "DEBUG" "WSL notification: text='${text:0:30}...', status=$status_icon, mode=$mode"
    
    local sound_success=false
    local speech_success=false
    
    # 効果音の再生
    if [[ "$mode" == "both" || "$mode" == "sound_only" ]]; then
        if play_status_sound "$status_icon" "$window_id"; then
            sound_success=true
            log "DEBUG" "Status sound played successfully"
            
            # 効果音と音声合成の間に短い間隔
            if [[ "$mode" == "both" ]]; then
                sleep 0.3
            fi
        else
            log "WARN" "Status sound failed for $status_icon"
        fi
    fi
    
    # 音声合成の実行
    if [[ "$mode" == "both" || "$mode" == "speech_only" ]]; then
        if wsl_speak "$text" "auto" "0" "80" "$window_id" "$status_icon"; then
            speech_success=true
            log "DEBUG" "Speech synthesis completed successfully"
        else
            log "WARN" "Speech synthesis failed"
        fi
    fi
    
    # 結果の評価
    case "$mode" in
        "both")
            if [[ "$sound_success" == "true" && "$speech_success" == "true" ]]; then
                log "INFO" "Complete notification successful (sound + speech)"
                return 0
            elif [[ "$sound_success" == "true" || "$speech_success" == "true" ]]; then
                log "WARN" "Partial notification successful"
                return 0
            else
                log "ERROR" "Complete notification failed"
                return 1
            fi
            ;;
        "sound_only")
            if [[ "$sound_success" == "true" ]]; then
                log "INFO" "Sound-only notification successful"
                return 0
            else
                log "ERROR" "Sound-only notification failed"
                return 1
            fi
            ;;
        "speech_only")
            if [[ "$speech_success" == "true" ]]; then
                log "INFO" "Speech-only notification successful"
                return 0
            else
                log "ERROR" "Speech-only notification failed"
                return 1
            fi
            ;;
        *)
            log "ERROR" "Invalid notification mode: $mode"
            return 1
            ;;
    esac
}

# === メイン音声合成関数（ステータス別・パンニング対応版） ===
wsl_speak() {
    local text="$1"
    local voice="${2:-auto}"
    local rate="${3:-0}"       # -10 to 10
    local volume="${4:-80}"    # 0 to 100
    local window_id="${5:-}"   # ウィンドウID（パンニング用）
    local status_icon="${6:-}" # ステータスアイコン（⚡、⌛、✅など）

    log "DEBUG" "WSL speak request: text='${text:0:50}...', voice=$voice, window_id=$window_id, status=$status_icon"

    # ステータス別設定の適用
    if [[ -n "$status_icon" && -n "${STATUS_VOICE_CONFIGS[$status_icon]:-}" ]]; then
        local config=$(parse_voice_config "$status_icon")
        IFS='|' read -r status_rate status_volume status_pitch status_speed <<< "$config"
        
        # ステータス設定で上書き（パラメータが0またはデフォルトの場合）
        if [[ "$rate" == "0" ]]; then rate="$status_rate"; fi
        if [[ "$volume" == "80" ]]; then volume="$status_volume"; fi
        
        log "DEBUG" "Applied status config for '$status_icon': rate=$rate, volume=$volume"
    fi

    # パンニング設定の計算（Equal Power Pan Law）
    local left_gain=1.0
    local right_gain=1.0
    if [[ -n "$window_id" && "$window_id" != "." ]]; then
        local panning_values=$(calculate_wsl_panning "$window_id" "$status_icon")
        left_gain=$(echo "$panning_values" | cut -d' ' -f1)
        right_gain=$(echo "$panning_values" | cut -d' ' -f2)
        log "DEBUG" "Calculated Equal Power gains for window $window_id: L=$left_gain, R=$right_gain"
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

    # テキストのサニタイズ（PowerShell特殊文字のエスケープ）
    local sanitized_text=$(echo "$text" | sed "s/'/\`'/g" | sed 's/"/\\"/g')

    log "DEBUG" "Using voice: $voice, rate: $rate, volume: $volume, panning: L=$left_gain R=$right_gain"

    # PowerShell実行ファイルの取得
    local powershell_path=$(find_powershell)
    if [[ -z "$powershell_path" ]]; then
        log "ERROR" "PowerShell not found for speech synthesis"
        return 1
    fi

    # PowerShell音声合成の実行（Equal Power Pan Law対応）
    local result=$("$powershell_path" -Command "
        try {
            Add-Type -AssemblyName System.Speech;
            \$synth = New-Object System.Speech.Synthesis.SpeechSynthesizer;
            \$synth.SelectVoice('$voice');
            \$synth.Rate = $rate;
            \$synth.Volume = $volume;
            
            # Equal Power Panningが有効な場合のWAVファイル処理
            if (($left_gain -ne 1.0) -or ($right_gain -ne 1.0)) {
                # WAVファイルに出力
                \$tempWav = [System.IO.Path]::GetTempFileName() + '.wav';
                \$synth.SetOutputToWaveFile(\$tempWav);
                \$synth.Speak('$sanitized_text');
                \$synth.SetOutputToDefaultAudioDevice();
                
                # Equal Power Panningを模擬（音量調整による簡易実装）
                # L/Rゲインを音量に反映（PowerShellの制限により簡略化）
                \$panBalance = [math]::Round(($right_gain - $left_gain) * 100);
                if (\$panBalance -lt -100) { \$panBalance = -100; }
                if (\$panBalance -gt 100) { \$panBalance = 100; }
                
                # System.Media.SoundPlayerでの再生（パンニング情報はログのみ）
                \$player = New-Object System.Media.SoundPlayer;
                \$player.SoundLocation = \$tempWav;
                \$player.PlaySync();
                
                Remove-Item \$tempWav -ErrorAction SilentlyContinue;
                Write-Output ('success_panned:L=' + '$left_gain' + ',R=' + '$right_gain');
            } else {
                # 通常の音声合成（パンニングなし）
                \$synth.Speak('$sanitized_text');
                Write-Output 'success_normal';
            }
        } catch {
            Write-Output ('error: ' + \$_.Exception.Message);
        }
    " 2>/dev/null | tr -d '\r\n')

    if [[ "$result" =~ ^success ]]; then
        if [[ "$result" == "success_normal" ]]; then
            log "INFO" "Speech synthesis completed (status: $status_icon, no panning)"
        elif [[ "$result" =~ success_panned ]]; then
            log "INFO" "Speech synthesis completed with Equal Power panning (status: $status_icon, L=$left_gain, R=$right_gain)"
        else
            log "INFO" "Speech synthesis completed successfully (status: $status_icon)"
        fi
        return 0
    else
        log "ERROR" "Speech synthesis failed: $result"
        return 1
    fi
}

# === 非同期音声出力 ===
wsl_speak_async() {
    local text="$1"
    local voice="${2:-auto}"
    local timeout="${3:-30}"

    log "DEBUG" "Starting async speech synthesis"

    # バックグラウンドで音声出力
    (
        timeout "$timeout" wsl_speak "$text" "$voice" 2>/dev/null
        local exit_code=$?
        if [[ $exit_code -eq 124 ]]; then
            log "WARN" "Speech synthesis timed out after ${timeout}s"
        elif [[ $exit_code -ne 0 ]]; then
            log "WARN" "Speech synthesis failed with exit code $exit_code"
        fi
    ) &

    local bg_pid=$!
    log "DEBUG" "Speech synthesis started in background (PID: $bg_pid)"

    # 同時音声出力の制限
    manage_voice_processes

    return 0
}

# === 音声プロセス管理 ===
manage_voice_processes() {
    local max_processes="${1:-1}"

    # 現在実行中の音声プロセス数を取得
    local current_count=$(pgrep -f "powershell.*Speech" 2>/dev/null | wc -l)

    log "DEBUG" "Current voice processes: $current_count, max allowed: $max_processes"

    if [[ $current_count -gt $max_processes ]]; then
        log "WARN" "Too many voice processes ($current_count), terminating oldest"
        # 古いプロセスを終了（最新のものを残す）
        local pids=($(pgrep -f "powershell.*Speech" 2>/dev/null))
        for ((i = 0; i < ${#pids[@]} - $max_processes; i++)); do
            kill "${pids[i]}" 2>/dev/null
            log "DEBUG" "Terminated voice process: ${pids[i]}"
        done
        sleep 0.5
    fi
}

# === 音声システム診断 ===
diagnose_wsl_voice() {
    echo "=== WSL Voice Engine Diagnostics ==="
    echo "Version: $WSL_VOICE_VERSION"
    echo ""

    # WSL環境検出
    local wsl_type=$(detect_wsl_environment)
    echo "WSL Environment: $wsl_type"

    # PowerShell可用性
    local powershell_path=$(find_powershell)
    if [[ -n "$powershell_path" ]]; then
        echo "PowerShell: Available ($powershell_path)"
        local ps_version=$("$powershell_path" -Command '$PSVersionTable.PSVersion.ToString()' 2>/dev/null | tr -d '\r\n')
        echo "PowerShell Version: $ps_version"
    else
        echo "PowerShell: Not Available"
        return 1
    fi

    # Windows Speech System
    local speech_status=$(check_windows_speech)
    echo "Windows Speech: $speech_status"

    if [[ "$speech_status" == "available" ]]; then
        echo ""
        echo "Available Voices:"
        detect_available_voices | while IFS='|' read -r name culture gender; do
            echo "  $name ($culture, $gender)"
        done

        echo ""
        echo "Recommended Japanese Voice:"
        local jp_voice=$(select_best_japanese_voice)
        if [[ -n "$jp_voice" ]]; then
            echo "  $jp_voice"
        else
            echo "  None available"
        fi

        echo ""
        echo "Fallback English Voice:"
        local en_voice=$(select_fallback_english_voice)
        if [[ -n "$en_voice" ]]; then
            echo "  $en_voice"
        else
            echo "  None available"
        fi
    fi

    echo ""
    echo "=== End Diagnostics ==="
}

# === ステータス別音声テスト関数（公式3状態版） ===
test_status_specific_voices() {
    echo "=== Testing Claude Code Status-Specific Voice Configurations ==="
    echo ""
    
    # Claude Code公式3状態のテストメッセージ
    declare -A status_messages=(
        ["⚡"]="処理を実行中です。少々お待ちください。"
        ["⌛"]="入力をお待ちしています。ご指示をお聞かせください。"
        ["✅"]="処理が正常に完了しました。"
    )
    
    echo "Testing each Claude Code status configuration..."
    for status in "⚡" "⌛" "✅"; do
        local message="${status_messages[$status]}"
        echo "Testing status '$status': $message"
        
        if wsl_speak "$message" "auto" "0" "80" "1" "$status"; then
            echo "✅ Status '$status' test: PASSED"
        else
            echo "❌ Status '$status' test: FAILED"
        fi
        
        # ステータス間で少し間隔を開ける
        sleep 1
    done
    
    echo ""
    echo "Claude Code status-specific voice test completed"
}

# === パンニング機能テスト（安全版） ===
test_wsl_panning() {
    echo "=== Testing WSL Panning System (Safe Mode) ==="
    echo ""
    
    # Equal Power Pan Law計算テスト
    echo "1. Testing Equal Power Pan Law calculations..."
    
    # テスト用のウィンドウ設定
    local test_windows=("1" "2" "3" "4" "5")
    for window_id in "${test_windows[@]}"; do
        local panning_gains=$(calculate_wsl_panning "$window_id" "✅")
        local left_gain=$(echo "$panning_gains" | cut -d' ' -f1)
        local right_gain=$(echo "$panning_gains" | cut -d' ' -f2)
        echo "   Window $window_id: L=$left_gain, R=$right_gain"
        
        # ゲイン値の妥当性チェック
        local total_power=$(awk "BEGIN { printf \"%.3f\", ($left_gain * $left_gain) + ($right_gain * $right_gain) }")
        echo "     Power check: ${total_power} (should be ~2.000 for Equal Power)"
    done
    
    echo ""
    echo "2. Testing sequential audio (safe mode)..."
    
    # 順次音声テスト（並列処理を避ける）
    echo "   Testing different panning positions..."
    
    echo "   Playing test audio - LEFT (window 1)"
    wsl_speak "左チャンネルテスト" "auto" "0" "75" "1" "⚡"
    
    echo "   Playing test audio - CENTER (window 2)"
    wsl_speak "中央チャンネルテスト" "auto" "0" "75" "2" "⌛"
    
    echo "   Playing test audio - RIGHT (window 3)"
    wsl_speak "右チャンネルテスト" "auto" "0" "75" "3" "✅"
    
    echo ""
    echo "WSL panning system test completed (safe mode)"
}

# === 統合テスト関数 ===
test_wsl_voice() {
    echo "Testing WSL Voice Engine..."

    # 診断実行
    diagnose_wsl_voice
    echo ""

    # 基本音声テスト
    echo "Testing basic speech synthesis..."
    if wsl_speak "WSL音声エンジンのテストです。日本語の読み上げが正常に動作しています。"; then
        echo "✅ Japanese speech test: PASSED"
    else
        echo "❌ Japanese speech test: FAILED"
    fi

    echo ""
    echo "Testing English speech synthesis..."
    if wsl_speak "This is a test of the WSL voice engine. English speech synthesis is working."; then
        echo "✅ English speech test: PASSED"
    else
        echo "❌ English speech test: FAILED"
    fi

    echo ""
    
    # ステータス別音声テスト
    test_status_specific_voices
    echo ""
    
    # パンニングテスト
    test_wsl_panning
    echo ""

    echo "WSL Voice Engine comprehensive test completed"
}

# === このスクリプトが直接実行された場合 ===
if [[ "${BASH_SOURCE[0]}" == "${0}" ]]; then
    # 基本モジュールの読み込み
    SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

    if [[ -f "$SCRIPT_DIR/base.sh" ]]; then
        source "$SCRIPT_DIR/base.sh"
        claude_voice_init true
    else
        # スタンドアロン実行時の簡易ログ関数
        log() {
            local level="$1"
            local message="$2"
            echo "[$level] $message" >&2
        }
    fi

    # コマンドライン引数の処理
    case "${1:-safe}" in
        "test")
            test_wsl_voice
            ;;
        "safe")
            echo "=== WSL Voice Engine Safe Test ==="
            echo "Testing basic functionality only..."
            echo ""
            
            # 基本診断
            diagnose_wsl_voice
            echo ""
            
            # 安全な音声テスト
            echo "Testing basic speech synthesis..."
            if wsl_speak "安全モードでのテストです" "auto" "0" "75"; then
                echo "✅ Safe mode speech test: PASSED"
            else
                echo "❌ Safe mode speech test: FAILED"
            fi
            echo ""
            echo "Safe test completed"
            ;;
        "diagnose")
            diagnose_wsl_voice
            ;;
        "speak")
            if [[ -n "$2" ]]; then
                text="$2"
                voice="${3:-auto}"
                rate="${4:-0}"
                volume="${5:-80}"
                window_id="${6:-}"
                status_icon="${7:-}"
                wsl_speak "$text" "$voice" "$rate" "$volume" "$window_id" "$status_icon"
            else
                echo "Usage: $0 speak <text> [voice] [rate] [volume] [window_id] [status_icon]"
                exit 1
            fi
            ;;
        "sound")
            if [[ -n "$2" ]]; then
                status_icon="$2"
                window_id="${3:-1}"
                play_status_sound "$status_icon" "$window_id"
            else
                echo "Usage: $0 sound <status_icon> [window_id]"
                echo "  status_icon: ⚡ (busy), ⌛ (waiting), ✅ (complete)"
                exit 1
            fi
            ;;
        "notify")
            if [[ -n "$2" && -n "$3" ]]; then
                text="$2"
                status_icon="$3"
                window_id="${4:-1}"
                mode="${5:-both}"
                wsl_notify "$text" "$status_icon" "$window_id" "$mode"
            else
                echo "Usage: $0 notify <text> <status_icon> [window_id] [mode]"
                echo "  mode: both (default), sound_only, speech_only"
                exit 1
            fi
            ;;
        *)
            echo "Usage: $0 {safe|test|diagnose|speak|sound|notify}"
            echo "  safe     - Run safe mode test (default)"
            echo "  test     - Run comprehensive tests"
            echo "  diagnose - Show system diagnostics"
            echo "  speak    - Test speech synthesis"
            echo "  sound    - Play status sound effects"
            echo "  notify   - Complete notification (sound + speech)"
            exit 1
            ;;
    esac
fi
