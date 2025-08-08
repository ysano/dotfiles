# TMux Claude Voice - コンポーネント詳細設計

## C4 Model - Level 3: Component Diagram

### Voice Engine Core - 詳細コンポーネント構成

```mermaid
C4Component
    title Component Diagram - Voice Engine Core

    Container_Boundary(voice_engine, "Voice Engine Core") {
        Component(universal_api, "Universal Voice API", "Shell Interface", "Unified voice synthesis API across platforms")
        Component(audio_processor, "Audio Processor", "Shell + Audio Utils", "Sound effects and audio manipulation")
        Component(queue_manager, "Queue Manager", "Shell Scripts", "Voice request queuing and concurrency control")
        Component(platform_router, "Platform Router", "Shell Logic", "Route requests to appropriate platform adapters")
        Component(cache_manager, "Cache Manager", "File System", "Voice synthesis result caching")
        Component(metrics_collector, "Metrics Collector", "JSON Logging", "Performance and usage metrics")
    }
    
    Container_Boundary(platform_adapters, "Platform Adapters") {
        Component(wsl_adapter, "WSL Adapter", "PowerShell Integration", "Windows Speech Platform via PowerShell")
        Component(macos_adapter, "macOS Adapter", "osascript/say", "macOS native speech synthesis")
        Component(linux_adapter, "Linux Adapter", "espeak/festival", "Linux speech synthesis engines")
    }
    
    Container_Boundary(external_systems, "External Audio Systems") {
        ComponentExt(windows_speech, "Windows Speech API", "Microsoft TTS")
        ComponentExt(macos_speech, "macOS Speech Framework", "Core Audio")
        ComponentExt(linux_audio, "Linux Audio Stack", "ALSA/PulseAudio")
    }
    
    Rel(universal_api, platform_router, "Route request", "platform detection + parameters")
    Rel(platform_router, queue_manager, "Queue request", "speech request + priority")
    Rel(queue_manager, audio_processor, "Process audio", "text + voice settings")
    Rel(audio_processor, cache_manager, "Check cache", "cache key + parameters")
    
    Rel(platform_router, wsl_adapter, "WSL synthesis", "PowerShell command")
    Rel(platform_router, macos_adapter, "macOS synthesis", "osascript execution")
    Rel(platform_router, linux_adapter, "Linux synthesis", "command execution")
    
    Rel(wsl_adapter, windows_speech, "PowerShell interop", "TTS API calls")
    Rel(macos_adapter, macos_speech, "Native calls", "say command + Audio Units")
    Rel(linux_adapter, linux_audio, "Command line", "espeak/aplay pipeline")
    
    Rel(metrics_collector, universal_api, "Collect metrics", "API call statistics")
    Rel(metrics_collector, queue_manager, "Queue metrics", "processing time + queue depth")
```

## 主要コンポーネント実装詳細

### 1. Universal Voice API

#### 責任範囲
- クロスプラットフォーム音声合成の統一インターフェース
- パラメータ正規化と検証
- エラーハンドリングと復旧

#### 主要インターフェース
```bash
# Core Voice API
universal_speak() {
    local text="$1"
    local voice="${2:-auto}"
    local speed="${3:-200}"
    local volume="${4:-80}"
    local priority="${5:-normal}"
    
    # Input validation
    validate_text_input "$text" || return 1
    
    # Platform detection and routing
    local platform=$(detect_current_platform)
    route_voice_request "$platform" "$text" "$voice" "$speed" "$volume" "$priority"
}

# Sound Effects API
universal_play_sound() {
    local pattern="$1"      # success|warning|waiting
    local duration="${2:-1000}"
    local volume="${3:-70}"
    
    case "$pattern" in
        "success")   play_success_pattern "$duration" "$volume" ;;
        "warning")   play_warning_pattern "$duration" "$volume" ;;
        "waiting")   play_waiting_pattern "$duration" "$volume" ;;
        *)           play_custom_pattern "$pattern" "$duration" "$volume" ;;
    esac
}

# Notification API
universal_notify() {
    local text="$1"
    local status="${2:-info}"    # info|success|warning|error
    local mode="${3:-auto}"      # sound|speech|both|silent
    
    case "$mode" in
        "sound")  universal_play_sound "$status" ;;
        "speech") universal_speak "$text" ;;
        "both")   universal_play_sound "$status" && universal_speak "$text" ;;
        "silent") log_notification "$text" "$status" ;;
        "auto")   auto_select_notification_mode "$text" "$status" ;;
    esac
}
```

#### パフォーマンス特性
- **レスポンス時間**: <100ms (API call to platform routing)
- **スループット**: 10 requests/second (concurrent processing)
- **メモリ使用量**: <5MB (basic caching)

### 2. Platform Router

#### ルーティングロジック
```bash
route_voice_request() {
    local platform="$1"
    local text="$2"
    local voice="$3"
    local speed="$4"
    local volume="$5"
    local priority="$6"
    
    # Platform capability check
    check_platform_capability "$platform" "speech" || {
        fallback_to_beep "$text"
        return 1
    }
    
    # Route to appropriate adapter
    case "$platform" in
        "wsl")
            wsl_speak_text "$text" "$voice" "$speed" "$volume"
            ;;
        "macos")
            macos_speak_text "$text" "$voice" "$speed" "$volume"
            ;;
        "linux")
            linux_speak_text "$text" "$voice" "$speed" "$volume"
            ;;
        *)
            log_error "Unsupported platform: $platform"
            return 1
            ;;
    esac
}

# Platform capability detection
check_platform_capability() {
    local platform="$1"
    local capability="$2"
    
    # Use cached result if available
    local cache_key="${platform}_${capability}"
    local cached_result=$(get_cached_capability "$cache_key")
    [[ -n "$cached_result" ]] && return "$cached_result"
    
    # Perform capability check
    local result=1
    case "$platform:$capability" in
        "wsl:speech")    command -v powershell.exe >/dev/null && result=0 ;;
        "macos:speech")  command -v say >/dev/null && result=0 ;;
        "linux:speech")  command -v espeak >/dev/null || command -v festival >/dev/null && result=0 ;;
    esac
    
    # Cache result
    cache_capability "$cache_key" "$result"
    return "$result"
}
```

#### フォールバック戦略
1. **Primary**: Platform-native speech synthesis
2. **Secondary**: Alternative speech engine
3. **Tertiary**: Sound effects only
4. **Final**: Silent notification with logging

### 3. Queue Manager

#### 並行処理制御
```bash
# Queue configuration
declare -g voice_queue=()
declare -g max_concurrent_voices=2
declare -g current_voice_processes=()

enqueue_voice_request() {
    local request="$1"
    local priority="${2:-normal}"
    
    # Add to appropriate queue based on priority
    case "$priority" in
        "high")
            voice_queue=("$request" "${voice_queue[@]}")  # Prepend
            ;;
        "normal"|*)
            voice_queue+=("$request")  # Append
            ;;
    esac
    
    # Process queue if capacity available
    process_voice_queue
}

process_voice_queue() {
    # Check current capacity
    cleanup_finished_voices
    local current_count=${#current_voice_processes[@]}
    
    while [[ $current_count -lt $max_concurrent_voices && ${#voice_queue[@]} -gt 0 ]]; do
        local request="${voice_queue[0]}"
        voice_queue=("${voice_queue[@]:1}")  # Remove first element
        
        # Execute voice request in background
        execute_voice_request "$request" &
        local pid=$!
        current_voice_processes+=("$pid")
        
        ((current_count++))
    done
}

cleanup_finished_voices() {
    local active_processes=()
    for pid in "${current_voice_processes[@]}"; do
        if kill -0 "$pid" 2>/dev/null; then
            active_processes+=("$pid")
        fi
    done
    current_voice_processes=("${active_processes[@]}")
}
```

#### キューイング戦略
- **Priority Levels**: high, normal, low
- **Scheduling**: Priority-based with starvation prevention
- **Timeout**: 30 seconds per voice request
- **Capacity**: Configurable concurrent limit (default: 2)

### 4. Audio Processor

#### 音響効果処理
```bash
# Equal Power Pan Law implementation (3dB center)
apply_equal_power_pan() {
    local pan_position="$1"    # -1.0 to 1.0 (left to right)
    local input_audio="$2"
    local output_audio="$3"
    
    # Calculate pan coefficients
    local pan_angle=$(echo "$pan_position * 0.785398" | bc -l)  # π/4
    local left_gain=$(echo "c($pan_angle) * 0.707107" | bc -l)   # cos * √2/2
    local right_gain=$(echo "s($pan_angle) * 0.707107" | bc -l)  # sin * √2/2
    
    # Apply panning (implementation depends on available audio tools)
    if command -v sox >/dev/null; then
        sox "$input_audio" "$output_audio" remix "1*${left_gain}" "1*${right_gain}"
    else
        # Fallback: copy input to output without panning
        cp "$input_audio" "$output_audio"
    fi
}

# Status-specific sound patterns
generate_status_sound() {
    local status="$1"
    local duration="${2:-1000}"
    local volume="${3:-70}"
    local output_file="$4"
    
    case "$status" in
        "⚡")  # Busy - Warning pattern
            generate_beep_pattern "800,800,600" "80,80,100" "$volume" "$output_file"
            ;;
        "⌛")  # Waiting - Ascending melody
            generate_beep_pattern "659,880,1175" "100,150,100" "$volume" "$output_file"
            ;;
        "✅")  # Complete - Success pattern
            generate_beep_pattern "523,659,783,1046" "80,80,80,120" "$volume" "$output_file"
            ;;
        *)
            generate_default_beep "$duration" "$volume" "$output_file"
            ;;
    esac
}

generate_beep_pattern() {
    local frequencies="$1"     # Comma-separated frequencies
    local durations="$2"       # Comma-separated durations in ms
    local volume="$3"
    local output_file="$4"
    
    # Split frequencies and durations
    IFS=',' read -ra freq_array <<< "$frequencies"
    IFS=',' read -ra dur_array <<< "$durations"
    
    # Generate each tone
    local temp_files=()
    for i in "${!freq_array[@]}"; do
        local freq="${freq_array[$i]}"
        local dur="${dur_array[$i]:-100}"
        local temp_file="/tmp/beep_${i}_$$.wav"
        
        generate_single_tone "$freq" "$dur" "$volume" "$temp_file"
        temp_files+=("$temp_file")
    done
    
    # Concatenate tones
    concatenate_audio_files "${temp_files[@]}" "$output_file"
    
    # Cleanup
    rm -f "${temp_files[@]}"
}
```

#### 音声品質最適化
- **サンプルレート**: 44.1kHz (CD quality)
- **ビット深度**: 16-bit
- **チャンネル**: Stereo with pan control
- **音量正規化**: Peak normalization to prevent clipping
- **フォーマット**: WAV (uncompressed) for processing, MP3 for storage

### 5. WSL Adapter - Advanced Integration

#### PowerShell 統合パターン
```bash
wsl_speak_text() {
    local text="$1"
    local voice="${2:-Microsoft Haruka Desktop}"
    local speed="${3:-0}"      # -10 to 10
    local volume="${4:-80}"    # 0 to 100
    
    # Escape text for PowerShell
    local escaped_text=$(escape_powershell_string "$text")
    
    # Construct PowerShell command
    local ps_command=$(cat << EOF
Add-Type -AssemblyName System.Speech;
\$synth = New-Object System.Speech.Synthesis.SpeechSynthesizer;
\$synth.SelectVoice('$voice');
\$synth.Rate = $speed;
\$synth.Volume = $volume;
\$synth.Speak('$escaped_text');
EOF
)
    
    # Execute with timeout and error handling
    timeout 30s powershell.exe -Command "$ps_command" 2>/dev/null || {
        log_error "WSL speech synthesis failed for text: ${text:0:50}..."
        return 1
    }
}

# Advanced voice detection and selection
detect_available_japanese_voices() {
    local ps_command='
    Add-Type -AssemblyName System.Speech;
    $synth = New-Object System.Speech.Synthesis.SpeechSynthesizer;
    $synth.GetInstalledVoices() | Where-Object { $_.VoiceInfo.Culture.Name -like "ja*" } | ForEach-Object { $_.VoiceInfo.Name }
    '
    
    powershell.exe -Command "$ps_command" 2>/dev/null | while read -r voice; do
        echo "$voice"
    done
}

# Windows host IP detection for network-based integrations
get_windows_host_ip() {
    # Multiple detection methods for robustness
    local host_ip=""
    
    # Method 1: /etc/resolv.conf
    host_ip=$(grep nameserver /etc/resolv.conf | head -1 | awk '{print $2}' 2>/dev/null)
    [[ -n "$host_ip" && "$host_ip" != "127.0.0.1" ]] && echo "$host_ip" && return
    
    # Method 2: ip route
    host_ip=$(ip route show default | awk '/default/ {print $3}' 2>/dev/null)
    [[ -n "$host_ip" ]] && echo "$host_ip" && return
    
    # Method 3: hardcoded fallback
    echo "172.16.0.1"  # Common WSL2 gateway
}
```

#### WSL 環境最適化
- **プロセス分離**: WSL プロセスと Windows プロセスの適切な分離
- **ネットワーク統合**: WSL2 ネットワーク経由でのホストアクセス
- **ファイルシステム統合**: `/mnt/c` 経由でのWindows ファイルアクセス
- **権限管理**: Windows 実行ポリシーの動的調整

### 6. Cache Manager

#### キャッシュ戦略
```bash
# Multi-level caching system
declare -A memory_cache=()          # In-memory cache
declare -g disk_cache_dir="/tmp/tmux-claude-cache"
declare -g cache_ttl=300            # 5 minutes TTL

# Memory cache operations
memory_cache_get() {
    local key="$1"
    local value="${memory_cache[$key]}"
    [[ -n "$value" ]] && echo "$value"
}

memory_cache_set() {
    local key="$1"
    local value="$2"
    memory_cache["$key"]="$value"
}

# Disk cache operations
disk_cache_get() {
    local key="$1"
    local cache_file="$disk_cache_dir/$(echo "$key" | sha256sum | cut -d' ' -f1)"
    
    [[ -f "$cache_file" ]] || return 1
    
    # Check TTL
    local file_age=$(($(date +%s) - $(stat -c %Y "$cache_file")))
    [[ $file_age -lt $cache_ttl ]] || { rm -f "$cache_file"; return 1; }
    
    cat "$cache_file"
}

disk_cache_set() {
    local key="$1"
    local value="$2"
    local cache_file="$disk_cache_dir/$(echo "$key" | sha256sum | cut -d' ' -f1)"
    
    mkdir -p "$disk_cache_dir"
    echo "$value" > "$cache_file"
}

# Unified cache interface
cache_get() {
    local key="$1"
    
    # Try memory cache first
    local value=$(memory_cache_get "$key")
    [[ -n "$value" ]] && echo "$value" && return 0
    
    # Try disk cache
    value=$(disk_cache_get "$key")
    [[ -n "$value" ]] && {
        memory_cache_set "$key" "$value"  # Promote to memory
        echo "$value"
        return 0
    }
    
    return 1
}

cache_set() {
    local key="$1"
    local value="$2"
    
    memory_cache_set "$key" "$value"
    disk_cache_set "$key" "$value"
}
```

#### キャッシュ対象データ
- **プラットフォーム検出結果**: 60秒キャッシュ
- **音声エンジン能力**: 5分キャッシュ
- **設定値**: メモリキャッシュ（設定変更まで有効）
- **音声合成結果**: 同一テキスト・パラメータの再利用

### 7. Metrics Collector

#### メトリクス収集システム
```bash
# Metrics collection infrastructure
declare -g metrics_file="/tmp/tmux-claude-metrics.jsonl"
declare -g metrics_buffer=()
declare -g metrics_flush_interval=10  # seconds

collect_voice_metrics() {
    local operation="$1"
    local platform="$2"
    local duration_ms="$3"
    local text_length="$4"
    local success="$5"
    
    local metric=$(cat << EOF
{
  "timestamp": "$(date -Iseconds)",
  "type": "voice_operation",
  "operation": "$operation",
  "platform": "$platform",
  "duration_ms": $duration_ms,
  "text_length": $text_length,
  "success": $success,
  "session_id": "${TMUX_SESSION:-unknown}"
}
EOF
)
    
    metrics_buffer+=("$metric")
    
    # Flush buffer if it's getting large
    [[ ${#metrics_buffer[@]} -ge 10 ]] && flush_metrics_buffer
}

flush_metrics_buffer() {
    [[ ${#metrics_buffer[@]} -eq 0 ]] && return
    
    # Append all buffered metrics to file
    printf '%s\n' "${metrics_buffer[@]}" >> "$metrics_file"
    
    # Clear buffer
    metrics_buffer=()
    
    # Rotate log if it's getting large
    [[ -f "$metrics_file" && $(stat -c%s "$metrics_file") -gt 10485760 ]] && rotate_metrics_file  # 10MB
}

# Performance monitoring
monitor_system_resources() {
    local cpu_percent=$(top -bn1 | grep "Cpu(s)" | awk '{print $2}' | cut -d'%' -f1)
    local memory_mb=$(free -m | awk 'NR==2{print $3}')
    local disk_usage=$(df -h /tmp | awk 'NR==2{print $5}' | cut -d'%' -f1)
    
    local resource_metric=$(cat << EOF
{
  "timestamp": "$(date -Iseconds)",
  "type": "system_resources",
  "cpu_percent": $cpu_percent,
  "memory_mb": $memory_mb,
  "disk_usage_percent": $disk_usage,
  "active_sessions": $(tmux list-sessions 2>/dev/null | wc -l)
}
EOF
)
    
    echo "$resource_metric" >> "$metrics_file"
}
```

#### 監視対象メトリクス
- **応答時間**: API呼び出しから音声出力まで
- **成功率**: プラットフォーム別の音声合成成功率
- **リソース使用量**: CPU、メモリ、ディスク使用量
- **エラー率**: カテゴリ別エラー発生率
- **ユーザー行動**: 機能使用パターン

## 依存関係とインターフェース

### 内部依存関係
```
Universal API → Platform Router → Queue Manager → Audio Processor
     ↓               ↓                    ↓             ↓
Cache Manager ← Metrics Collector ← Queue Manager ← Platform Adapters
```

### 外部依存関係
- **必須依存**: bash, tmux, coreutils
- **プラットフォーム依存**: powershell.exe (WSL), say (macOS), espeak (Linux)
- **オプション依存**: sox (audio processing), curl (API calls), jq (JSON processing)

### API 契約
```bash
# All platform adapters must implement:
${platform}_speak_text(text, voice, speed, volume) -> exit_code
${platform}_play_sound(frequency, duration, volume) -> exit_code
${platform}_detect_capabilities() -> capability_list
${platform}_get_available_voices() -> voice_list
```

この詳細なコンポーネント設計により、システムの各部分が独立してテスト・開発可能となり、拡張性と保守性が大幅に向上しています。