#!/bin/bash
# Windows Notification System Module - Windows通知システム管理
# Windows Toast通知、GUI通知、システム通知統合

# 必要なモジュールの読み込み
source "${CLAUDE_VOICE_HOME:-$HOME/.tmux/claude}/core/powershell_engine.sh" 2>/dev/null || {
    log "ERROR" "PowerShell engine module not found"
    return 1
}

# グローバル変数
declare -g NOTIFICATION_CACHE=""
declare -g TOAST_NOTIFICATION_AVAILABLE=""
declare -g LAST_NOTIFICATION_TIME=""

# Windows Toast通知の可用性確認
check_toast_notification_support() {
    local powershell_path
    powershell_path=$(find_powershell_path)
    
    if [[ -z "$powershell_path" ]]; then
        TOAST_NOTIFICATION_AVAILABLE="false"
        return 1
    fi
    
    # Windows.UI.Notifications サポートチェック
    local toast_test_script="
try {
    [Windows.UI.Notifications.ToastNotificationManager, Windows.UI.Notifications, ContentType = WindowsRuntime] | Out-Null
    Write-Output 'TOAST_SUPPORTED'
} catch {
    Write-Output 'TOAST_NOT_SUPPORTED'
}
"
    
    local result
    result=$(execute_powershell_script "$toast_test_script" 10 "$powershell_path")
    
    if [[ "$result" == "TOAST_SUPPORTED" ]]; then
        TOAST_NOTIFICATION_AVAILABLE="true"
        log "DEBUG" "Windows Toast notifications supported"
        return 0
    else
        TOAST_NOTIFICATION_AVAILABLE="false"
        log "DEBUG" "Windows Toast notifications not supported"
        return 1
    fi
}

# Windows Toast通知の送信
send_toast_notification() {
    local title="$1"
    local message="$2"
    local icon="${3:-Information}"
    local duration="${4:-Short}"  # Short, Long
    local sound="${5:-Default}"
    
    if [[ -z "$title" ]] || [[ -z "$message" ]]; then
        log "ERROR" "Title and message required for toast notification"
        return 1
    fi
    
    local powershell_path
    powershell_path=$(find_powershell_path)
    
    if [[ -z "$powershell_path" ]] || [[ "$TOAST_NOTIFICATION_AVAILABLE" == "false" ]]; then
        log "WARN" "Toast notifications not available"
        return 1
    fi
    
    # テキストエスケープ
    local escaped_title=$(echo "$title" | sed 's/"/\\"/g' | sed "s/'/\\'/g")
    local escaped_message=$(echo "$message" | sed 's/"/\\"/g' | sed "s/'/\\'/g")
    
    # Toast通知スクリプト
    local toast_script="
try {
    # Windows.UI.Notifications を使用したトースト通知
    [Windows.UI.Notifications.ToastNotificationManager, Windows.UI.Notifications, ContentType = WindowsRuntime] | Out-Null
    [Windows.UI.Notifications.ToastNotification, Windows.UI.Notifications, ContentType = WindowsRuntime] | Out-Null
    [Windows.Data.Xml.Dom.XmlDocument, Windows.Data.Xml.Dom.XmlDocument, ContentType = WindowsRuntime] | Out-Null
    
    # トーストテンプレート
    \\$template = @\"
<toast duration=\"$duration\">
    <visual>
        <binding template=\"ToastGeneric\">
            <text>$escaped_title</text>
            <text>$escaped_message</text>
        </binding>
    </visual>
    <audio src=\"ms-winsoundevent:Notification.$sound\" />
</toast>
\"@
    
    \\$xml = New-Object Windows.Data.Xml.Dom.XmlDocument
    \\$xml.LoadXml(\\$template)
    
    \\$toast = New-Object Windows.UI.Notifications.ToastNotification \\$xml
    [Windows.UI.Notifications.ToastNotificationManager]::CreateToastNotifier('Claude Voice').Show(\\$toast)
    
    Write-Output 'TOAST_SUCCESS'
} catch {
    Write-Output 'TOAST_ERROR:'\\$(\\$_.Exception.Message)
}
"
    
    local result
    result=$(execute_powershell_script "$toast_script" 10 "$powershell_path")
    
    # 統合エラーハンドリングを使用
    if [[ -n "${LOADED_MODULES[error_handler]:-}" ]] || load_module "error_handler" false; then
        case "$result" in
            "TOAST_SUCCESS")
                log "DEBUG" "Toast notification sent successfully"
                LAST_NOTIFICATION_TIME=$(date +%s)
                return 0
                ;;
            TOAST_ERROR:*)
                local error_detail="${result#TOAST_ERROR:}"
                handle_notification_error "TOAST_ERROR" "$error_detail" "windows_notification_system" "send_toast_notification"
                return $?
                ;;
            *)
                handle_notification_error "TOAST_ERROR" "Unexpected toast result: $result" "windows_notification_system" "send_toast_notification"
                return $?
                ;;
        esac
    else
        # フォールバック: 従来のエラーハンドリング
        case "$result" in
            "TOAST_SUCCESS")
                log "DEBUG" "Toast notification sent successfully"
                LAST_NOTIFICATION_TIME=$(date +%s)
                return 0
                ;;
            TOAST_ERROR:*)
                log "ERROR" "Toast notification failed: ${result#TOAST_ERROR:}"
                return 1
                ;;
            *)
                log "WARN" "Unexpected toast notification result: $result"
                return 1
                ;;
        esac
    fi
}

# Windows MessageBox通知（フォールバック）
send_messagebox_notification() {
    local title="$1"
    local message="$2"
    local icon="${3:-Information}"  # Information, Warning, Error, Question
    local buttons="${4:-OK}"        # OK, OKCancel, YesNo, YesNoCancel
    
    if [[ -z "$title" ]] || [[ -z "$message" ]]; then
        log "ERROR" "Title and message required for MessageBox notification"
        return 1
    fi
    
    local powershell_path
    powershell_path=$(find_powershell_path)
    
    if [[ -z "$powershell_path" ]]; then
        log "WARN" "MessageBox notifications not available"
        return 1
    fi
    
    # テキストエスケープ
    local escaped_title=$(echo "$title" | sed 's/"/\\"/g' | sed "s/'/\\'/g")
    local escaped_message=$(echo "$message" | sed 's/"/\\"/g' | sed "s/'/\\'/g")
    
    # MessageBox通知スクリプト
    local messagebox_script="
try {
    [void] [System.Reflection.Assembly]::LoadWithPartialName('System.Windows.Forms')
    \\$result = [System.Windows.Forms.MessageBox]::Show('$escaped_message', '$escaped_title', '$buttons', '$icon')
    Write-Output 'MESSAGEBOX_SUCCESS:'\\$result
} catch {
    Write-Output 'MESSAGEBOX_ERROR:'\\$(\\$_.Exception.Message)
}
"
    
    local result
    result=$(execute_powershell_script "$messagebox_script" 30 "$powershell_path")
    
    case "$result" in
        MESSAGEBOX_SUCCESS:*)
            local user_response="${result#MESSAGEBOX_SUCCESS:}"
            log "DEBUG" "MessageBox notification sent successfully (response: $user_response)"
            echo "$user_response"
            return 0
            ;;
        MESSAGEBOX_ERROR:*)
            log "ERROR" "MessageBox notification failed: ${result#MESSAGEBOX_ERROR:}"
            return 1
            ;;
        *)
            log "WARN" "Unexpected MessageBox result: $result"
            return 1
            ;;
    esac
}

# システム通知の送信（命名規則統一版）
send_platform_notification() {
    local title="$1"
    local message="$2"
    local sound="${3:-default}"
    local urgency="${4:-normal}"
    local timeout="${5:-5000}"
    
    if [[ -z "$title" ]] || [[ -z "$message" ]]; then
        log "ERROR" "Title and message required for notification"
        return 1
    fi
    
    log "DEBUG" "Sending Windows notification: title=$title, urgency=$urgency"
    
    # 緊急度に基づく通知スタイル設定
    local toast_icon="Information"
    local toast_sound="Default"
    local toast_duration="Short"
    local msgbox_icon="Information"
    
    case "$urgency" in
        "critical"|"high")
            toast_icon="Warning"
            toast_sound="Alarm"
            toast_duration="Long"
            msgbox_icon="Error"
            ;;
        "warning")
            toast_icon="Warning"
            toast_sound="Exclamation"
            msgbox_icon="Warning"
            ;;
        "normal"|"info")
            toast_icon="Information"
            toast_sound="Default"
            msgbox_icon="Information"
            ;;
        "low")
            toast_icon="Information"
            toast_sound="SMS"
            msgbox_icon="Information"
            ;;
    esac
    
    # Toast通知を最初に試行
    if [[ "$TOAST_NOTIFICATION_AVAILABLE" != "false" ]]; then
        if send_toast_notification "$title" "$message" "$toast_icon" "$toast_duration" "$toast_sound"; then
            return 0
        fi
    fi
    
    # フォールバック: MessageBox通知
    log "DEBUG" "Falling back to MessageBox notification"
    if send_messagebox_notification "$title" "$message" "$msgbox_icon" "OK"; then
        return 0
    fi
    
    # 最終フォールバック: コンソール出力
    log "WARN" "All GUI notifications failed, using console fallback"
    echo "通知: $title - $message"
    return 1
}

# 後方互換性のためのエイリアス（非推奨）
send_notification() {
    log "WARN" "send_notification() is deprecated, use send_platform_notification()"
    send_platform_notification "$@"
}

# バルーン通知（システムトレイ）
send_balloon_notification() {
    local title="$1"
    local message="$2"
    local icon="${3:-Info}"  # None, Info, Warning, Error
    local timeout="${4:-5000}"
    
    if [[ -z "$title" ]] || [[ -z "$message" ]]; then
        log "ERROR" "Title and message required for balloon notification"
        return 1
    fi
    
    local powershell_path
    powershell_path=$(find_powershell_path)
    
    if [[ -z "$powershell_path" ]]; then
        log "WARN" "Balloon notifications not available"
        return 1
    fi
    
    # テキストエスケープ
    local escaped_title=$(echo "$title" | sed 's/"/\\"/g' | sed "s/'/\\'/g")
    local escaped_message=$(echo "$message" | sed 's/"/\\"/g' | sed "s/'/\\'/g")
    
    # バルーン通知スクリプト
    local balloon_script="
try {
    Add-Type -AssemblyName System.Windows.Forms
    \\$notification = New-Object System.Windows.Forms.NotifyIcon
    \\$notification.Icon = [System.Drawing.SystemIcons]::$icon
    \\$notification.BalloonTipTitle = '$escaped_title'
    \\$notification.BalloonTipText = '$escaped_message'
    \\$notification.Visible = \\$true
    \\$notification.ShowBalloonTip($timeout)
    Start-Sleep -Seconds 2
    \\$notification.Dispose()
    Write-Output 'BALLOON_SUCCESS'
} catch {
    Write-Output 'BALLOON_ERROR:'\\$(\\$_.Exception.Message)
}
"
    
    local result
    result=$(execute_powershell_script "$balloon_script" 15 "$powershell_path")
    
    case "$result" in
        "BALLOON_SUCCESS")
            log "DEBUG" "Balloon notification sent successfully"
            return 0
            ;;
        BALLOON_ERROR:*)
            log "ERROR" "Balloon notification failed: ${result#BALLOON_ERROR:}"
            return 1
            ;;
        *)
            log "WARN" "Unexpected balloon notification result: $result"
            return 1
            ;;
    esac
}

# 通知レート制限
is_notification_rate_limited() {
    local current_time=$(date +%s)
    local min_interval="${NOTIFICATION_MIN_INTERVAL:-2}"  # 秒
    
    if [[ -n "$LAST_NOTIFICATION_TIME" ]]; then
        local time_diff=$((current_time - LAST_NOTIFICATION_TIME))
        if [[ $time_diff -lt $min_interval ]]; then
            log "DEBUG" "Notification rate limited (${time_diff}s < ${min_interval}s)"
            return 0
        fi
    fi
    
    return 1
}

# 通知キューシステム
declare -g NOTIFICATION_QUEUE=()

add_to_notification_queue() {
    local title="$1"
    local message="$2"
    local urgency="${3:-normal}"
    
    local notification_data="$title|$message|$urgency|$(date +%s)"
    NOTIFICATION_QUEUE+=("$notification_data")
    
    log "DEBUG" "Added notification to queue (queue size: ${#NOTIFICATION_QUEUE[@]})"
}

process_notification_queue() {
    if [[ ${#NOTIFICATION_QUEUE[@]} -eq 0 ]]; then
        return 0
    fi
    
    # レート制限チェック
    if is_notification_rate_limited; then
        log "DEBUG" "Skipping queue processing due to rate limit"
        return 0
    fi
    
    # キューから最初の通知を取得
    local notification_data="${NOTIFICATION_QUEUE[0]}"
    NOTIFICATION_QUEUE=("${NOTIFICATION_QUEUE[@]:1}")  # 最初の要素を削除
    
    # 通知データをパース
    IFS='|' read -r title message urgency timestamp <<< "$notification_data"
    
    log "DEBUG" "Processing queued notification: $title"
    send_notification "$title" "$message" "default" "$urgency"
}

# 通知システムの初期化
init_windows_notification_system() {
    log "INFO" "Initializing Windows notification system"
    
    # PowerShell依存関係チェック
    if ! check_powershell_execution; then
        log "ERROR" "Windows notification system initialization failed - PowerShell not available"
        return 1
    fi
    
    # Toast通知サポートチェック
    check_toast_notification_support
    
    log "INFO" "Windows notification system initialized (Toast: ${TOAST_NOTIFICATION_AVAILABLE:-unknown})"
    return 0
}

# 通知システム情報取得
get_notification_system_info() {
    local format="${1:-json}"
    
    case "$format" in
        "json")
            cat <<EOF
{
    "toast_available": "${TOAST_NOTIFICATION_AVAILABLE:-unknown}",
    "powershell_available": $(if [[ -n "$(find_powershell_path 2>/dev/null)" ]]; then echo "true"; else echo "false"; fi),
    "queue_size": ${#NOTIFICATION_QUEUE[@]},
    "last_notification": "${LAST_NOTIFICATION_TIME:-none}"
}
EOF
            ;;
        "text")
            echo "Windows Notification System Status:"
            echo "  Toast Notifications: ${TOAST_NOTIFICATION_AVAILABLE:-unknown}"
            echo "  PowerShell Available: $(if [[ -n "$(find_powershell_path 2>/dev/null)" ]]; then echo "Yes"; else echo "No"; fi)"
            echo "  Queue Size: ${#NOTIFICATION_QUEUE[@]}"
            echo "  Last Notification: ${LAST_NOTIFICATION_TIME:-none}"
            ;;
        *)
            log "ERROR" "Unknown format: $format"
            return 1
            ;;
    esac
}

# Windows通知システムテスト
test_windows_notification_system() {
    echo "=== Windows Notification System Test ==="
    
    # PowerShell チェック
    if ! check_powershell_execution; then
        echo "❌ PowerShell not available for notification testing"
        return 1
    fi
    
    # Toast通知サポートテスト
    if check_toast_notification_support; then
        echo "✅ Toast notification support detected"
        
        # Toast通知テスト
        if send_toast_notification "Test Notification" "Toast notification test from Claude Voice" "Information" "Short" "Default"; then
            echo "✅ Toast notification test successful"
        else
            echo "❌ Toast notification test failed"
        fi
    else
        echo "⚠️  Toast notification not supported"
    fi
    
    # MessageBox通知テスト
    echo ""
    echo "Testing MessageBox notification..."
    if send_messagebox_notification "Test MessageBox" "MessageBox notification test from Claude Voice" "Information" "OK" >/dev/null; then
        echo "✅ MessageBox notification test successful"
    else
        echo "❌ MessageBox notification test failed"
    fi
    
    # バルーン通知テスト
    echo ""
    echo "Testing Balloon notification..."
    if send_balloon_notification "Test Balloon" "Balloon notification test from Claude Voice" "Info" 3000; then
        echo "✅ Balloon notification test successful"
    else
        echo "❌ Balloon notification test failed"
    fi
    
    # 統合通知テスト
    echo ""
    echo "Testing integrated notification system..."
    if send_notification "Claude Voice Test" "Integrated notification system test" "default" "normal"; then
        echo "✅ Integrated notification test successful"
    else
        echo "❌ Integrated notification test failed"
    fi
    
    # キューシステムテスト
    echo ""
    echo "Testing notification queue system..."
    add_to_notification_queue "Queue Test 1" "First queued notification" "normal"
    add_to_notification_queue "Queue Test 2" "Second queued notification" "normal"
    
    echo "Queue size: ${#NOTIFICATION_QUEUE[@]}"
    
    process_notification_queue
    sleep 1
    process_notification_queue
    
    echo "✅ Notification queue test completed"
    
    echo ""
    echo "Windows Notification System test completed"
    get_notification_system_info "text"
    
    return 0
}

# 緊急通知の送信
send_urgent_notification() {
    local title="$1"
    local message="$2"
    
    # レート制限を無視して緊急通知を送信
    local saved_last_time="$LAST_NOTIFICATION_TIME"
    LAST_NOTIFICATION_TIME=""
    
    send_notification "$title" "$message" "alarm" "critical"
    local result=$?
    
    LAST_NOTIFICATION_TIME="$saved_last_time"
    return $result
}