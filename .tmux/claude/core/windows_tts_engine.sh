#!/bin/bash
# Windows TTS Engine - Windows Text-to-Speech音声合成モジュール
# Windows固有のTTS機能と音声エンジン管理

# 必要なモジュールの読み込み
source "${CLAUDE_VOICE_HOME:-$HOME/.tmux/claude}/core/powershell_engine.sh" 2>/dev/null || {
    log "ERROR" "PowerShell engine module not found"
    return 1
}

# グローバル変数
declare -g WINDOWS_TTS_CACHE=""
declare -g SELECTED_JAPANESE_VOICE=""
declare -g TTS_VOICE_LIST=""

# Windows音声エンジンの検出
detect_windows_tts_voices() {
    local powershell_path
    powershell_path=$(find_powershell_path)
    
    if [[ -z "$powershell_path" ]]; then
        echo "Microsoft Haruka Desktop"  # デフォルト
        return 1
    fi
    
    log "DEBUG" "Detecting Windows TTS voices"
    
    # キャッシュされた結果があれば使用
    if [[ -n "$TTS_VOICE_LIST" ]]; then
        echo "$TTS_VOICE_LIST"
        return 0
    fi
    
    # PowerShellで利用可能な音声を取得
    local voices_script='
try {
    Add-Type -AssemblyName System.Speech
    $synth = New-Object System.Speech.Synthesis.SpeechSynthesizer
    $voices = $synth.GetInstalledVoices() | Where-Object { $_.Enabled } | ForEach-Object { 
        $voice = $_.VoiceInfo
        [PSCustomObject]@{
            Name = $voice.Name
            Language = $voice.Culture.Name
            Gender = $voice.Gender
            Age = $voice.Age
        }
    }
    $voices | ConvertTo-Json -Compress
} catch {
    Write-Output "ERROR: $($_.Exception.Message)"
}
'
    
    local result
    result=$(execute_powershell_script "$voices_script" 10 "$powershell_path")
    
    if [[ "$result" == ERROR:* ]]; then
        log "WARN" "TTS voice detection failed: ${result#ERROR: }"
        echo "Microsoft Haruka Desktop"  # フォールバック
        return 1
    fi
    
    # JSON結果をパース
    local voices
    if command -v jq >/dev/null 2>&1 && [[ "$result" == [* ]]; then
        voices=$(echo "$result" | jq -r '.[].Name' 2>/dev/null)
    else
        # JSON パースなしの簡易抽出
        voices=$(echo "$result" | grep -o '"Name":"[^"]*"' | sed 's/"Name":"\([^"]*\)"/\1/' | sort -u)
    fi
    
    if [[ -n "$voices" ]]; then
        TTS_VOICE_LIST="$voices"
        echo "$voices"
    else
        echo "Microsoft Haruka Desktop"  # フォールバック
    fi
}

# 詳細な音声情報取得
get_voice_details() {
    local voice_name="$1"
    local powershell_path
    powershell_path=$(find_powershell_path)
    
    if [[ -z "$powershell_path" ]] || [[ -z "$voice_name" ]]; then
        return 1
    fi
    
    local voice_info_script="
try {
    Add-Type -AssemblyName System.Speech
    \$synth = New-Object System.Speech.Synthesis.SpeechSynthesizer
    \$voice = \$synth.GetInstalledVoices() | Where-Object { \$_.VoiceInfo.Name -eq '$voice_name' } | Select-Object -First 1
    if (\$voice) {
        \$info = \$voice.VoiceInfo
        [PSCustomObject]@{
            Name = \$info.Name
            Language = \$info.Culture.Name
            Gender = \$info.Gender
            Age = \$info.Age
            Description = \$info.Description
            Enabled = \$voice.Enabled
        } | ConvertTo-Json -Compress
    } else {
        Write-Output 'VOICE_NOT_FOUND'
    }
} catch {
    Write-Output \"ERROR: \$(\$_.Exception.Message)\"
}
"
    
    execute_powershell_script "$voice_info_script" 10 "$powershell_path"
}

# 最適な日本語音声の選択
select_japanese_voice() {
    local force_refresh="${1:-false}"
    
    # キャッシュされた結果があれば使用
    if [[ -n "$SELECTED_JAPANESE_VOICE" ]] && [[ "$force_refresh" != "true" ]]; then
        echo "$SELECTED_JAPANESE_VOICE"
        return 0
    fi
    
    local available_voices
    available_voices=$(detect_windows_tts_voices)
    
    # 日本語音声の優先順位
    local preferred_voices=(
        "Microsoft Haruka Desktop"
        "Microsoft Sayaka Desktop"
        "Microsoft Ichiro Desktop"
        "Microsoft Haruka Mobile"
        "Microsoft Sayaka Mobile"
        "Microsoft Ayumi Desktop"
        "Microsoft Ayumi Mobile"
    )
    
    for voice in "${preferred_voices[@]}"; do
        if echo "$available_voices" | grep -Fq "$voice"; then
            SELECTED_JAPANESE_VOICE="$voice"
            log "DEBUG" "Selected Japanese voice: $voice"
            echo "$voice"
            return 0
        fi
    done
    
    # 日本語が含まれる音声を検索
    local japanese_voice
    japanese_voice=$(echo "$available_voices" | grep -i "haruka\|sayaka\|ichiro\|ayumi" | head -1)
    if [[ -n "$japanese_voice" ]]; then
        SELECTED_JAPANESE_VOICE="$japanese_voice"
        log "DEBUG" "Found Japanese voice: $japanese_voice"
        echo "$japanese_voice"
        return 0
    fi
    
    # 日本語ロケールの音声を検索
    local jp_voice
    jp_voice=$(echo "$available_voices" | grep -i "ja-jp\|japanese" | head -1)
    if [[ -n "$jp_voice" ]]; then
        SELECTED_JAPANESE_VOICE="$jp_voice"
        log "DEBUG" "Found JP locale voice: $jp_voice"
        echo "$jp_voice"
        return 0
    fi
    
    # フォールバック
    SELECTED_JAPANESE_VOICE="Microsoft Haruka Desktop"
    echo "Microsoft Haruka Desktop"
}

# 音声テキストの前処理（Windows向け）
preprocess_speech_text() {
    local text="$1"
    
    if [[ -z "$text" ]]; then
        return 1
    fi
    
    # PowerShell文字列エスケープ
    local processed
    processed=$(echo "$text" | \
        sed "s/'/\'\'/g" | \
        sed 's/"/\\"/g' | \
        sed 's/`/\\`/g' | \
        sed 's/\$/\\$/g')
    
    # 特殊文字の読み上げ対応
    processed=$(echo "$processed" | \
        sed 's/⏺/○/g' | \
        sed 's/✅/成功/g' | \
        sed 's/❌/エラー/g' | \
        sed 's/⚠️/警告/g' | \
        sed 's/📁/フォルダ/g' | \
        sed 's/🔧/設定/g' | \
        sed 's/📊/グラフ/g' | \
        sed 's/🎯/ターゲット/g' | \
        sed 's/🚀/ロケット/g' | \
        sed 's/&/アンド/g' | \
        sed 's/@/アット/g' | \
        sed 's/#/シャープ/g' | \
        sed 's/%/パーセント/g')
    
    # URL の簡略化
    processed=$(echo "$processed" | sed 's|https\?://[^ ]*|URL|g')
    
    # コードブロックの簡略化
    processed=$(echo "$processed" | sed 's/```[^`]*```/コードブロック/g')
    
    # 長すぎるテキストの短縮
    local max_length="${SPEECH_MAX_LENGTH:-400}"
    if [[ ${#processed} -gt $max_length ]]; then
        processed="${processed:0:$max_length}。以下省略。"
    fi
    
    echo "$processed"
}

# Windows TTS音声合成実行
speak_with_windows_tts() {
    local text="$1"
    local voice="${2:-auto}"
    local rate="${3:-0}"  # -10 to 10
    local volume="${4:-100}"  # 0 to 100
    
    if [[ -z "$text" ]]; then
        log "ERROR" "No text provided for TTS"
        return 1
    fi
    
    local powershell_path
    powershell_path=$(find_powershell_path)
    
    if [[ -z "$powershell_path" ]]; then
        log "ERROR" "PowerShell not available for TTS"
        return 1
    fi
    
    # 音声の選択
    local selected_voice
    if [[ "$voice" == "auto" ]] || [[ -z "$voice" ]]; then
        selected_voice=$(select_japanese_voice)
    else
        selected_voice="$voice"
    fi
    
    # テキストの前処理
    local processed_text
    processed_text=$(preprocess_speech_text "$text")
    
    log "DEBUG" "TTS: voice=$selected_voice, rate=$rate, volume=$volume"
    
    # PowerShell TTS スクリプト
    local tts_script="
try {
    Add-Type -AssemblyName System.Speech
    \$synth = New-Object System.Speech.Synthesis.SpeechSynthesizer
    
    # 音声設定
    try {
        \$synth.SelectVoice('$selected_voice')
    } catch {
        Write-Output \"WARN: Voice '$selected_voice' not found, using default\"
    }
    
    # レート設定 (-10 to 10)
    \$synth.Rate = [Math]::Max(-10, [Math]::Min(10, $rate))
    
    # ボリューム設定 (0 to 100)
    \$synth.Volume = [Math]::Max(0, [Math]::Min(100, $volume))
    
    # 音声合成実行
    \$synth.Speak('$processed_text')
    
    Write-Output 'TTS_SUCCESS'
} catch {
    Write-Output \"TTS_ERROR: \$(\$_.Exception.Message)\"
}
"
    
    local result
    result=$(execute_powershell_script "$tts_script" 30 "$powershell_path")
    
    # 統合エラーハンドリングを使用
    if [[ -n "${LOADED_MODULES[error_handler]:-}" ]] || load_module "error_handler" false; then
        case "$result" in
            "TTS_SUCCESS")
                log "DEBUG" "TTS execution successful"
                return 0
                ;;
            TTS_ERROR:*)
                local error_detail="${result#TTS_ERROR: }"
                handle_voice_error "TTS_ERROR" "$error_detail" "windows_tts_engine" "speak_with_windows_tts"
                return $?
                ;;
            WARN:*)
                log "WARN" "${result#WARN: }"
                return 0
                ;;
            *)
                handle_voice_error "TTS_ERROR" "Unexpected TTS result: $result" "windows_tts_engine" "speak_with_windows_tts"
                return $?
                ;;
        esac
    else
        # フォールバック: 従来のエラーハンドリング
        case "$result" in
            "TTS_SUCCESS")
                log "DEBUG" "TTS execution successful"
                return 0
                ;;
            TTS_ERROR:*)
                log "ERROR" "TTS failed: ${result#TTS_ERROR: }"
                return 1
                ;;
            WARN:*)
                log "WARN" "${result#WARN: }"
                return 0
                ;;
            *)
                log "WARN" "Unexpected TTS result: $result"
                return 1
                ;;
        esac
    fi
}

# 非同期音声合成
speak_with_windows_tts_async() {
    local text="$1"
    local voice="${2:-auto}"
    local rate="${3:-0}"
    local volume="${4:-100}"
    
    # バックグラウンドで音声合成実行
    (speak_with_windows_tts "$text" "$voice" "$rate" "$volume") &
    local pid=$!
    
    log "DEBUG" "TTS started in background (PID: $pid)"
    return 0
}

# Windows音声合成のテスト
test_windows_tts() {
    local test_text="${1:-テスト音声です。Windows TTS エンジンの動作確認中。}"
    
    echo "=== Windows TTS Engine Test ==="
    
    # PowerShellエンジンチェック
    if ! check_powershell_execution; then
        echo "❌ PowerShell not available for TTS testing"
        return 1
    fi
    
    # 音声検出テスト
    echo "音声検出中..."
    local voices
    voices=$(detect_windows_tts_voices)
    
    if [[ -n "$voices" ]]; then
        echo "✅ 検出された音声:"
        echo "$voices" | sed 's/^/  - /'
    else
        echo "❌ 音声が検出されませんでした"
        return 1
    fi
    
    # 日本語音声選択テスト
    echo ""
    echo "日本語音声選択中..."
    local jp_voice
    jp_voice=$(select_japanese_voice)
    echo "✅ 選択された日本語音声: $jp_voice"
    
    # テキスト前処理テスト
    echo ""
    echo "テキスト前処理テスト..."
    local processed
    processed=$(preprocess_speech_text "✅ テスト成功！ URL: https://example.com @user #tag")
    echo "✅ 前処理結果: $processed"
    
    # 音声合成テスト
    echo ""
    echo "音声合成テスト実行中..."
    if speak_with_windows_tts "$test_text" "$jp_voice" 0 80; then
        echo "✅ Windows TTS 音声合成成功"
    else
        echo "❌ Windows TTS 音声合成失敗"
        return 1
    fi
    
    return 0
}

# === Windows固有の音声合成（後方互換性） ===
speak_windows() {
    local text="$1"
    local voice="${2:-auto}"
    
    speak_with_windows_tts "$text" "$voice"
}

# Windows TTS エンジン初期化
init_windows_tts_engine() {
    log "DEBUG" "Initializing Windows TTS engine"
    
    # PowerShell依存関係チェック
    if ! check_powershell_dotnet_support; then
        log "ERROR" "Windows TTS engine requires PowerShell .NET Speech support"
        return 1
    fi
    
    # 音声リスト取得
    detect_windows_tts_voices >/dev/null
    
    # 日本語音声選択
    select_japanese_voice >/dev/null
    
    log "INFO" "Windows TTS Engine initialized with voice: ${SELECTED_JAPANESE_VOICE:-auto}"
    return 0
}

# Windows TTS エンジン情報取得
get_windows_tts_info() {
    local format="${1:-json}"
    
    case "$format" in
        "json")
            cat <<EOF
{
    "engine": "Windows TTS",
    "voices_available": $(echo "$TTS_VOICE_LIST" | wc -l),
    "selected_voice": "${SELECTED_JAPANESE_VOICE:-auto}",
    "powershell_required": true,
    "dotnet_speech_required": true
}
EOF
            ;;
        "text")
            echo "Windows TTS Engine Status:"
            echo "  Available voices: $(echo "$TTS_VOICE_LIST" | wc -l)"
            echo "  Selected voice: ${SELECTED_JAPANESE_VOICE:-auto}"
            echo "  PowerShell required: Yes"
            echo "  .NET Speech required: Yes"
            ;;
        *)
            log "ERROR" "Unknown format: $format"
            return 1
            ;;
    esac
}