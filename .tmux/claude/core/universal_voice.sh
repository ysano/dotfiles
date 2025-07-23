#!/bin/bash
# Universal Voice System for tmux Claude Integration
# WSL改善をmacOS環境に適用した統一音声システム

# 音声エンジンの検出
detect_voice_engine() {
    local os_type="$(uname)"
    
    case "$os_type" in
        "Darwin")
            if command -v say >/dev/null 2>&1; then
                echo "macos_say"
            else
                echo "none"
            fi
            ;;
        "Linux")
            if grep -qi microsoft /proc/version 2>/dev/null || [[ -n "$WSL_DISTRO_NAME" ]]; then
                # WSL環境
                if command -v powershell.exe >/dev/null 2>&1; then
                    echo "wsl_powershell"
                elif command -v espeak >/dev/null 2>&1; then
                    echo "linux_espeak"
                else
                    echo "none"
                fi
            else
                # 純粋なLinux環境
                if command -v espeak >/dev/null 2>&1; then
                    echo "linux_espeak"
                elif command -v festival >/dev/null 2>&1; then
                    echo "linux_festival"
                else
                    echo "none"
                fi
            fi
            ;;
        *)
            echo "none"
            ;;
    esac
}

# macOS Focus Mode / Do Not Disturb の高度な検出
check_macos_focus_mode() {
    if [[ "$(uname)" != "Darwin" ]]; then
        return 0  # macOS以外では制限しない
    fi
    
    # macOS Focus Mode検出の改善版
    if command -v shortcuts >/dev/null 2>&1; then
        local focus_mode=$(shortcuts run "Get Focus Mode" 2>/dev/null || echo "none")
        
        case "$focus_mode" in
            "Do Not Disturb"|"Work"|"Sleep"|"Personal")
                return 1  # 音声出力を抑制
                ;;
            *)
                return 0  # 音声出力を許可
                ;;
        esac
    fi
    
    # フォールバック: 従来のDNDチェック
    local dnd_status=$(plutil -extract dnd_prefs.userPref.enabled xml1 \
        "$HOME/Library/Preferences/com.apple.ncprefs.plist" 2>/dev/null | \
        grep -o "<true/>")
        
    if [[ -n "$dnd_status" ]]; then
        return 1  # DNDが有効
    else
        return 0  # DNDが無効
    fi
}

# 音声デバイスの智的選択（macOS）
select_macos_audio_device() {
    # 常にシステムのデフォルト出力設定を尊重
    echo "auto"
}

# 並行音声プロセスの制限（macOS）
limit_concurrent_voices_macos() {
    local max_processes="${1:-2}"
    
    # macOSの音声プロセス検出
    local say_pids=($(pgrep -f "^say " 2>/dev/null))
    local afplay_pids=($(pgrep -f "^afplay " 2>/dev/null))
    
    local total_count=$((${#say_pids[@]} + ${#afplay_pids[@]}))
    
    if [[ $total_count -gt $max_processes ]]; then
        # 古いプロセスを優先終了
        for pid in "${say_pids[@]:0:$((total_count - max_processes))}"; do
            kill "$pid" 2>/dev/null
        done
    fi
}

# ユニバーサル音声出力メイン関数
universal_speak() {
    local text="$1"
    local voice_setting="${2:-auto}"
    local speed="${3:-180}"  # 少し遅めで聞き取りやすく
    local max_length="${4:-300}"
    local engine="${5:-$(detect_voice_engine)}"
    
    # テキスト長制限
    if [[ ${#text} -gt $max_length ]]; then
        text="${text:0:$max_length}..."
    fi
    
    # Focus Mode / DND チェック
    if ! check_macos_focus_mode; then
        echo "[VOICE] Focus mode active - speech suppressed" >&2
        return 0
    fi
    
    case "$engine" in
        "macos_say")
            # 並行プロセス制限
            limit_concurrent_voices_macos 2
            
            # デバイス選択
            local audio_device=$(select_macos_audio_device)
            
            # 音声設定の最適化（Enhanced優先）
            local voice_name="Kyoko (Enhanced)"
            [[ "$voice_setting" != "auto" ]] && voice_name="$voice_setting"
            
            # 非同期実行 with timeout
            {
                if [[ "$audio_device" == "auto" ]]; then
                    say -v "$voice_name" -r "$speed" "$text"
                else
                    say -v "$voice_name" -r "$speed" -a "$audio_device" "$text"
                fi
            } &
            
            # プロセス完了待機（最大30秒）
            local say_pid=$!
            local timeout_count=0
            while kill -0 "$say_pid" 2>/dev/null && [[ $timeout_count -lt 30 ]]; do
                sleep 1
                ((timeout_count++))
            done
            
            # タイムアウト時は強制終了
            if [[ $timeout_count -ge 30 ]]; then
                kill "$say_pid" 2>/dev/null
                echo "[VOICE] Speech timeout - process terminated" >&2
                return 1
            fi
            ;;
            
        "wsl_powershell")
            # WSL PowerShell音声合成
            powershell.exe -Command "Add-Type -AssemblyName System.Speech; \
                \$synth = New-Object System.Speech.Synthesis.SpeechSynthesizer; \
                \$synth.Rate = $((speed / 25)); \
                \$synth.Speak('$text')" 2>/dev/null &
            ;;
            
        "linux_espeak")
            # Linux espeak
            espeak -s "$speed" "$text" 2>/dev/null &
            ;;
            
        "linux_festival")
            # Linux festival
            echo "$text" | festival --tts 2>/dev/null &
            ;;
            
        "none")
            echo "[VOICE] No suitable voice engine available" >&2
            return 1
            ;;
            
        *)
            echo "[VOICE] Unknown voice engine: $engine" >&2
            return 1
            ;;
    esac
    
    return 0
}

# 音声エンジンのテスト
test_voice_engine() {
    local engine="${1:-$(detect_voice_engine)}"
    
    echo "Testing voice engine: $engine"
    
    if universal_speak "音声システムテスト完了" "Kyoko" "200" "100" "$engine"; then
        echo "✅ Voice engine test passed: $engine"
        return 0
    else
        echo "❌ Voice engine test failed: $engine"
        return 1
    fi
}

# 利用可能音声エンジンの一覧
list_available_engines() {
    echo "Available voice engines:"
    
    local current_engine=$(detect_voice_engine)
    echo "  Current: $current_engine"
    
    if [[ "$(uname)" == "Darwin" ]]; then
        if command -v say >/dev/null 2>&1; then
            echo "  ✅ macos_say"
            echo "    Available voices:"
            say -v "?" | grep -E "(Kyoko|Alex|Victoria)" | head -3 | sed 's/^/      /'
        else
            echo "  ❌ macos_say (not available)"
        fi
    fi
    
    if grep -qi microsoft /proc/version 2>/dev/null || [[ -n "$WSL_DISTRO_NAME" ]]; then
        if command -v powershell.exe >/dev/null 2>&1; then
            echo "  ✅ wsl_powershell"
        else
            echo "  ❌ wsl_powershell (not available)"
        fi
    fi
    
    if command -v espeak >/dev/null 2>&1; then
        echo "  ✅ linux_espeak"
    else
        echo "  ❌ linux_espeak (not available)"
    fi
}

# CLIインターフェース
if [[ "${BASH_SOURCE[0]}" == "${0}" ]]; then
    case "${1:-}" in
        "test")
            test_voice_engine "$2"
            ;;
        "list")
            list_available_engines
            ;;
        "speak")
            shift
            universal_speak "$@"
            ;;
        "detect")
            detect_voice_engine
            ;;
        *)
            echo "Usage: $0 {test|list|speak|detect}"
            echo "  test [engine]     - Test voice engine"
            echo "  list              - List available engines"
            echo "  speak <text>      - Speak text using universal system"
            echo "  detect            - Detect current voice engine"
            exit 1
            ;;
    esac
fi