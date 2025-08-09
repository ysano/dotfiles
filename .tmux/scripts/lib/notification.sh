#!/bin/bash
# TMux Scripts Notification Library
# Version: 1.0.0
#
# 通知・音声関連のユーティリティ関数
# - OS別通知送信
# - 音声再生
# - レート制限
# - DND（Do Not Disturb）管理

# インポートガード
[[ -n "${_TMUX_NOTIFICATION_LOADED}" ]] && return 0
declare -gr _TMUX_NOTIFICATION_LOADED=1

# 依存ライブラリ
source "$(dirname "${BASH_SOURCE[0]}")/core.sh"
source "$(dirname "${BASH_SOURCE[0]}")/platform.sh"

# === 定数定義 ===
readonly NOTIFICATION_CACHE_DIR="${NOTIFICATION_CACHE_DIR:-/tmp/.tmux_notifications}"
readonly RATE_LIMIT_DIR="${RATE_LIMIT_DIR:-/tmp/.tmux_rate_limits}"
readonly DEFAULT_NOTIFICATION_TIMEOUT=10  # 秒
readonly DEFAULT_RATE_LIMIT=60  # 秒

# 通知タイプ
readonly NOTIFY_TYPE_INFO="info"
readonly NOTIFY_TYPE_SUCCESS="success"
readonly NOTIFY_TYPE_WARNING="warning"
readonly NOTIFY_TYPE_ERROR="error"

# === 初期化 ===

# キャッシュディレクトリ作成
_init_notification_dirs() {
    [[ ! -d "$NOTIFICATION_CACHE_DIR" ]] && mkdir -p "$NOTIFICATION_CACHE_DIR"
    [[ ! -d "$RATE_LIMIT_DIR" ]] && mkdir -p "$RATE_LIMIT_DIR"
}

# === レート制限 ===

# レート制限チェック
check_rate_limit() {
    local key="${1:-default}"
    local limit_seconds="${2:-$DEFAULT_RATE_LIMIT}"
    local limit_file="$RATE_LIMIT_DIR/${key}.limit"
    
    local now=$(date +%s)
    local last_time=0
    
    if [[ -f "$limit_file" ]]; then
        last_time=$(cat "$limit_file" 2>/dev/null || echo 0)
    fi
    
    local elapsed=$((now - last_time))
    
    if [[ $elapsed -lt $limit_seconds ]]; then
        log_debug "レート制限中: key=$key, 残り時間=$((limit_seconds - elapsed))秒"
        return 1  # Rate limited
    fi
    
    echo "$now" > "$limit_file"
    return 0  # OK to proceed
}

# レート制限リセット
reset_rate_limit() {
    local key="${1:-default}"
    local limit_file="$RATE_LIMIT_DIR/${key}.limit"
    
    if [[ -f "$limit_file" ]]; then
        rm -f "$limit_file"
        log_debug "レート制限リセット: key=$key"
    fi
}

# === DND（Do Not Disturb）管理 ===

# DND状態ファイル
readonly DND_STATUS_FILE="$NOTIFICATION_CACHE_DIR/dnd.status"

# DND有効化
enable_dnd() {
    local duration="${1:-0}"  # 0 = 無期限
    
    if [[ $duration -gt 0 ]]; then
        local until=$(($(date +%s) + duration))
        echo "$until" > "$DND_STATUS_FILE"
        log_info "DND有効化: ${duration}秒間"
    else
        echo "0" > "$DND_STATUS_FILE"
        log_info "DND有効化: 無期限"
    fi
}

# DND無効化
disable_dnd() {
    if [[ -f "$DND_STATUS_FILE" ]]; then
        rm -f "$DND_STATUS_FILE"
        log_info "DND無効化"
    fi
}

# DND状態チェック
is_dnd_enabled() {
    if [[ ! -f "$DND_STATUS_FILE" ]]; then
        return 1  # DND無効
    fi
    
    local until=$(cat "$DND_STATUS_FILE" 2>/dev/null || echo 0)
    
    if [[ $until -eq 0 ]]; then
        return 0  # 無期限DND
    fi
    
    local now=$(date +%s)
    if [[ $now -lt $until ]]; then
        return 0  # DND有効
    else
        # 期限切れ
        rm -f "$DND_STATUS_FILE"
        return 1  # DND無効
    fi
}

# === OS別通知送信 ===

# 汎用通知送信関数
send_notification() {
    local title="$1"
    local message="$2"
    local type="${3:-$NOTIFY_TYPE_INFO}"
    local timeout="${4:-$DEFAULT_NOTIFICATION_TIMEOUT}"
    
    # DNDチェック
    if is_dnd_enabled; then
        log_debug "DND有効のため通知をスキップ: $title"
        return 0
    fi
    
    local platform=$(detect_platform)
    
    case "$platform" in
        "$PLATFORM_MACOS")
            _send_notification_macos "$title" "$message" "$type"
            ;;
        "$PLATFORM_LINUX")
            _send_notification_linux "$title" "$message" "$type" "$timeout"
            ;;
        "$PLATFORM_WSL")
            _send_notification_wsl "$title" "$message" "$type"
            ;;
        "$PLATFORM_WINDOWS")
            _send_notification_windows "$title" "$message" "$type"
            ;;
        *)
            log_warn "サポートされていないプラットフォーム: $platform"
            return 1
            ;;
    esac
    
    log_debug "通知送信: title=$title, type=$type"
}

# macOS通知
_send_notification_macos() {
    local title="$1"
    local message="$2"
    local type="$3"
    
    # osascriptを使用
    if command_exists osascript; then
        local sound=""
        case "$type" in
            "$NOTIFY_TYPE_ERROR") sound="Basso" ;;
            "$NOTIFY_TYPE_WARNING") sound="Ping" ;;
            "$NOTIFY_TYPE_SUCCESS") sound="Glass" ;;
            *) sound="default" ;;
        esac
        
        osascript -e "display notification \"$message\" with title \"$title\" sound name \"$sound\"" 2>/dev/null
    
    # terminal-notifierを使用（インストール済みの場合）
    elif command_exists terminal-notifier; then
        local args="-title '$title' -message '$message'"
        case "$type" in
            "$NOTIFY_TYPE_ERROR") args="$args -sound Basso" ;;
            "$NOTIFY_TYPE_WARNING") args="$args -sound Ping" ;;
            "$NOTIFY_TYPE_SUCCESS") args="$args -sound Glass" ;;
        esac
        eval "terminal-notifier $args"
    else
        log_error "通知コマンドが見つかりません (macOS)"
        return 1
    fi
}

# Linux通知
_send_notification_linux() {
    local title="$1"
    local message="$2"
    local type="$3"
    local timeout="$4"
    
    # notify-sendを使用
    if command_exists notify-send; then
        local urgency="normal"
        local icon=""
        
        case "$type" in
            "$NOTIFY_TYPE_ERROR")
                urgency="critical"
                icon="dialog-error"
                ;;
            "$NOTIFY_TYPE_WARNING")
                urgency="normal"
                icon="dialog-warning"
                ;;
            "$NOTIFY_TYPE_SUCCESS")
                urgency="low"
                icon="dialog-information"
                ;;
            *)
                urgency="normal"
                icon="dialog-information"
                ;;
        esac
        
        notify-send -u "$urgency" -t $((timeout * 1000)) -i "$icon" "$title" "$message" 2>/dev/null
    else
        log_error "notify-sendが見つかりません (Linux)"
        return 1
    fi
}

# WSL通知
_send_notification_wsl() {
    local title="$1"
    local message="$2"
    local type="$3"
    
    local powershell_path=$(find_powershell)
    
    if [[ -n "$powershell_path" ]]; then
        # PowerShellでWindows通知
        local ps_script="
[Windows.UI.Notifications.ToastNotificationManager, Windows.UI.Notifications, ContentType = WindowsRuntime] | Out-Null
[Windows.UI.Notifications.ToastNotification, Windows.UI.Notifications, ContentType = WindowsRuntime] | Out-Null
[Windows.Data.Xml.Dom.XmlDocument, Windows.Data.Xml.Dom.XmlDocument, ContentType = WindowsRuntime] | Out-Null

\$template = @\"
<toast>
    <visual>
        <binding template='ToastGeneric'>
            <text>$title</text>
            <text>$message</text>
        </binding>
    </visual>
</toast>
\"@

\$xml = New-Object Windows.Data.Xml.Dom.XmlDocument
\$xml.LoadXml(\$template)
\$toast = New-Object Windows.UI.Notifications.ToastNotification \$xml
[Windows.UI.Notifications.ToastNotificationManager]::CreateToastNotifier('TMux Scripts').Show(\$toast)
"
        
        echo "$ps_script" | "$powershell_path" -NoProfile -Command - 2>/dev/null
    else
        # フォールバック: wsl-notify-sendを試す
        if command_exists wsl-notify-send; then
            wsl-notify-send -c "TMux Scripts" "$title" "$message"
        else
            log_error "通知コマンドが見つかりません (WSL)"
            return 1
        fi
    fi
}

# Windows通知
_send_notification_windows() {
    local title="$1"
    local message="$2"
    local type="$3"
    
    # msg.exeを使用（簡易通知）
    if command_exists msg.exe; then
        msg.exe "*" "$title: $message" 2>/dev/null
    else
        log_error "通知コマンドが見つかりません (Windows)"
        return 1
    fi
}

# === 音声再生 ===

# 汎用音声再生関数
play_sound() {
    local sound_file="$1"
    local volume="${2:-50}"  # 0-100
    
    # DND チェック
    if is_dnd_enabled; then
        log_debug "DND有効のため音声再生をスキップ"
        return 0
    fi
    
    if [[ ! -f "$sound_file" ]]; then
        log_error "音声ファイルが見つかりません: $sound_file"
        return 1
    fi
    
    local platform=$(detect_platform)
    
    case "$platform" in
        "$PLATFORM_MACOS")
            _play_sound_macos "$sound_file" "$volume"
            ;;
        "$PLATFORM_LINUX")
            _play_sound_linux "$sound_file" "$volume"
            ;;
        "$PLATFORM_WSL")
            _play_sound_wsl "$sound_file" "$volume"
            ;;
        *)
            log_warn "サポートされていないプラットフォーム: $platform"
            return 1
            ;;
    esac
    
    log_debug "音声再生: $sound_file (volume=$volume)"
}

# macOS音声再生
_play_sound_macos() {
    local sound_file="$1"
    local volume="$2"
    
    if command_exists afplay; then
        # ボリューム調整（0-2の範囲に変換）
        local afplay_volume=$(echo "scale=2; $volume / 50" | bc)
        afplay -v "$afplay_volume" "$sound_file" &
    else
        log_error "afplayが見つかりません (macOS)"
        return 1
    fi
}

# Linux音声再生
_play_sound_linux() {
    local sound_file="$1"
    local volume="$2"
    
    if command_exists paplay; then
        # PulseAudio
        paplay --volume=$((volume * 655)) "$sound_file" &
    elif command_exists aplay; then
        # ALSA
        aplay "$sound_file" &
    elif command_exists play; then
        # SoX
        play -v $(echo "scale=2; $volume / 100" | bc) "$sound_file" &
    else
        log_error "音声再生コマンドが見つかりません (Linux)"
        return 1
    fi
}

# WSL音声再生
_play_sound_wsl() {
    local sound_file="$1"
    local volume="$2"
    
    local powershell_path=$(find_powershell)
    
    if [[ -n "$powershell_path" ]]; then
        # WSLパスをWindowsパスに変換
        local windows_path=$(wslpath -w "$sound_file" 2>/dev/null)
        
        if [[ -n "$windows_path" ]]; then
            "$powershell_path" -NoProfile -Command "
                \$player = New-Object System.Media.SoundPlayer '$windows_path'
                \$player.Play()
            " 2>/dev/null &
        fi
    else
        log_error "PowerShellが見つかりません (WSL)"
        return 1
    fi
}

# === システムビープ ===

# システムビープ再生
system_beep() {
    local frequency="${1:-800}"  # Hz
    local duration="${2:-200}"   # ミリ秒
    
    # DND チェック
    if is_dnd_enabled; then
        return 0
    fi
    
    local platform=$(detect_platform)
    
    case "$platform" in
        "$PLATFORM_MACOS")
            # macOS: システムサウンド
            osascript -e "beep" 2>/dev/null || printf '\a'
            ;;
        "$PLATFORM_LINUX")
            # Linux: beepコマンドまたはエスケープシーケンス
            if command_exists beep; then
                beep -f "$frequency" -l "$duration" 2>/dev/null
            else
                printf '\a'
            fi
            ;;
        "$PLATFORM_WSL")
            # WSL: PowerShell経由
            local powershell_path=$(find_powershell)
            if [[ -n "$powershell_path" ]]; then
                "$powershell_path" -NoProfile -Command "[Console]::Beep($frequency, $duration)" 2>/dev/null
            else
                printf '\a'
            fi
            ;;
        *)
            printf '\a'
            ;;
    esac
}

# === 通知モード管理 ===

readonly NOTIFICATION_MODE_FILE="$NOTIFICATION_CACHE_DIR/mode"

# 通知モード設定
set_notification_mode() {
    local mode="$1"  # off, minimal, normal, verbose
    
    echo "$mode" > "$NOTIFICATION_MODE_FILE"
    log_info "通知モード設定: $mode"
}

# 通知モード取得
get_notification_mode() {
    if [[ -f "$NOTIFICATION_MODE_FILE" ]]; then
        cat "$NOTIFICATION_MODE_FILE"
    else
        echo "normal"
    fi
}

# モード別通知送信
send_notification_smart() {
    local title="$1"
    local message="$2"
    local type="${3:-$NOTIFY_TYPE_INFO}"
    
    local mode=$(get_notification_mode)
    
    case "$mode" in
        "off")
            log_debug "通知モードoff: 通知をスキップ"
            ;;
        "minimal")
            # エラーのみ通知
            if [[ "$type" == "$NOTIFY_TYPE_ERROR" ]]; then
                send_notification "$title" "$message" "$type"
            fi
            ;;
        "normal")
            # エラーと警告を通知
            if [[ "$type" == "$NOTIFY_TYPE_ERROR" ]] || [[ "$type" == "$NOTIFY_TYPE_WARNING" ]]; then
                send_notification "$title" "$message" "$type"
            fi
            ;;
        "verbose")
            # すべて通知
            send_notification "$title" "$message" "$type"
            ;;
    esac
}

# === 初期化 ===

# ライブラリ初期化
_init_notification() {
    _init_notification_dirs
    
    if [[ "$TMUX_SCRIPTS_DEBUG" == "true" ]]; then
        log_debug "TMux Scripts Notification Library loaded"
        log_debug "Notification cache: $NOTIFICATION_CACHE_DIR"
        log_debug "Rate limit dir: $RATE_LIMIT_DIR"
        log_debug "Notification mode: $(get_notification_mode)"
        
        if is_dnd_enabled; then
            log_debug "DND is enabled"
        fi
    fi
}

# 自動初期化
_init_notification