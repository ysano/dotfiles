#!/bin/bash
# WSL Integration Module - Windows Subsystem for Linux統合機能
# WSL固有の機能とWindows連携

# 必要なモジュールの読み込み
source "${CLAUDE_VOICE_HOME:-$HOME/.tmux/claude}/core/powershell_engine.sh" 2>/dev/null || {
    log "ERROR" "PowerShell engine module not found"
    return 1
}

# グローバル変数
declare -g WSL_INFO_CACHE=""
declare -g WSL_CLIPBOARD_AVAILABLE=""
declare -g WINDOWS_BUILD_VERSION=""

# WSL環境検出
is_wsl_environment() {
    # 複数の方法でWSL環境を検出
    if [[ -n "${WSL_DISTRO_NAME:-}" ]]; then
        return 0
    elif [[ -f /proc/version ]] && grep -qi microsoft /proc/version 2>/dev/null; then
        return 0
    elif [[ -f /proc/sys/kernel/osrelease ]] && grep -qi microsoft /proc/sys/kernel/osrelease 2>/dev/null; then
        return 0
    elif [[ -d /mnt/c/Windows ]] && [[ -f /mnt/c/Windows/System32/cmd.exe ]]; then
        return 0
    else
        return 1
    fi
}

# WSLバージョン検出
detect_wsl_version() {
    if ! is_wsl_environment; then
        echo "Not WSL"
        return 1
    fi
    
    if [[ -n "${WSL_DISTRO_NAME:-}" ]]; then
        # WSL2の場合はプロセス情報で判定
        if grep -qi "WSL2" /proc/version 2>/dev/null; then
            echo "WSL2"
        else
            echo "WSL1"
        fi
    elif grep -qi "WSL2" /proc/version 2>/dev/null; then
        echo "WSL2"
    elif grep -qi microsoft /proc/version 2>/dev/null; then
        echo "WSL1"
    else
        echo "Unknown"
    fi
}

# WSL固有のシステム情報取得
get_wsl_info() {
    local info_type="${1:-all}"
    
    if ! is_wsl_environment; then
        echo "Not running in WSL environment"
        return 1
    fi
    
    case "$info_type" in
        "version")
            detect_wsl_version
            ;;
        "distro")
            if [[ -n "${WSL_DISTRO_NAME:-}" ]]; then
                echo "$WSL_DISTRO_NAME"
            elif [[ -f /etc/os-release ]]; then
                grep -E "^NAME=" /etc/os-release | cut -d'"' -f2
            else
                echo "Unknown Distribution"
            fi
            ;;
        "windows_build")
            if [[ -n "$WINDOWS_BUILD_VERSION" ]]; then
                echo "$WINDOWS_BUILD_VERSION"
                return 0
            fi
            
            local build_version
            if command -v cmd.exe >/dev/null 2>&1; then
                build_version=$(cmd.exe /c "ver" 2>/dev/null | grep -oP "Version \K[0-9]+\.[0-9]+\.[0-9]+" || echo "Unknown")
                WINDOWS_BUILD_VERSION="$build_version"
                echo "$build_version"
            else
                echo "Unknown"
            fi
            ;;
        "memory")
            # WSL固有のメモリ情報
            if [[ -f /proc/meminfo ]]; then
                awk '/MemTotal:/ {total=$2} /MemAvailable:/ {avail=$2} END {
                    if (total > 0) {
                        used = total - avail
                        printf "%.1fGB/%.1fGB (%.1f%%)", used/1024/1024, total/1024/1024, (used*100)/total
                    } else {
                        print "Unknown"
                    }
                }' /proc/meminfo
            else
                echo "Unknown"
            fi
            ;;
        "network")
            # WSLネットワーク情報
            local wsl_ip
            wsl_ip=$(hostname -I 2>/dev/null | awk '{print $1}' || echo "Unknown")
            echo "WSL IP: $wsl_ip"
            
            # Windows側IPの取得
            local windows_ip
            if command -v powershell.exe >/dev/null 2>&1; then
                windows_ip=$(powershell.exe -Command "(Get-NetIPAddress -AddressFamily IPv4 -InterfaceAlias '*Ethernet*' | Select-Object -First 1).IPAddress" 2>/dev/null | tr -d '\r' || echo "Unknown")
                echo "Windows IP: $windows_ip"
            fi
            ;;
        "mounts")
            # Windowsマウント情報
            echo "Windows Mounts:"
            mount | grep -E "^[A-Z]: " | sed 's/^/  /' || echo "  None detected"
            ;;
        "all")
            echo "=== WSL System Information ==="
            echo "WSL Version: $(get_wsl_info version)"
            echo "Distribution: $(get_wsl_info distro)"
            echo "Windows Build: $(get_wsl_info windows_build)"
            echo "Memory Usage: $(get_wsl_info memory)"
            echo ""
            get_wsl_info network
            echo ""
            get_wsl_info mounts
            ;;
        *)
            log "ERROR" "Unknown info type: $info_type"
            return 1
            ;;
    esac
}

# WSL クリップボード統合機能
wsl_clipboard_copy() {
    local text="$1"
    
    if [[ -z "$text" ]]; then
        log "ERROR" "No text provided for clipboard copy"
        return 1
    fi
    
    # クリップボード機能が利用可能かチェック
    if [[ "$WSL_CLIPBOARD_AVAILABLE" == "false" ]]; then
        log "WARN" "Clipboard functionality not available"
        return 1
    fi
    
    # clip.exe の使用（推奨）
    if command -v clip.exe >/dev/null 2>&1; then
        echo -n "$text" | clip.exe
        log "DEBUG" "Text copied to Windows clipboard via clip.exe"
        WSL_CLIPBOARD_AVAILABLE="true"
        return 0
    fi
    
    # PowerShell経由のクリップボード
    local powershell_path
    powershell_path=$(find_powershell_path)
    if [[ -n "$powershell_path" ]]; then
        local clipboard_script="Set-Clipboard -Value @'\n${text}\n'@"
        if execute_powershell_script "$clipboard_script" 10 "$powershell_path" >/dev/null; then
            log "DEBUG" "Text copied to Windows clipboard via PowerShell"
            WSL_CLIPBOARD_AVAILABLE="true"
            return 0
        fi
    fi
    
    # Windows PowerShell直接実行（フォールバック）
    if command -v powershell.exe >/dev/null 2>&1; then
        echo -n "$text" | powershell.exe -Command "Set-Clipboard -Value (Get-Content -Raw)" 2>/dev/null
        log "DEBUG" "Text copied to Windows clipboard via powershell.exe"
        WSL_CLIPBOARD_AVAILABLE="true"
        return 0
    fi
    
    log "WARN" "No clipboard integration available"
    WSL_CLIPBOARD_AVAILABLE="false"
    return 1
}

# WSL クリップボードからの貼り付け
wsl_clipboard_paste() {
    # クリップボード機能が利用可能かチェック
    if [[ "$WSL_CLIPBOARD_AVAILABLE" == "false" ]]; then
        log "WARN" "Clipboard functionality not available"
        return 1
    fi
    
    # PowerShell経由の貼り付け（推奨）
    local powershell_path
    powershell_path=$(find_powershell_path)
    if [[ -n "$powershell_path" ]]; then
        local paste_result
        paste_result=$(execute_powershell_script "Get-Clipboard" 10 "$powershell_path" 2>/dev/null)
        if [[ -n "$paste_result" ]]; then
            echo "$paste_result" | sed 's/\r$//'
            return 0
        fi
    fi
    
    # Windows PowerShell直接実行（フォールバック）
    if command -v powershell.exe >/dev/null 2>&1; then
        local paste_content
        paste_content=$(powershell.exe -Command "Get-Clipboard" 2>/dev/null | sed 's/\r$//')
        if [[ -n "$paste_content" ]]; then
            echo "$paste_content"
            return 0
        fi
    fi
    
    log "WARN" "No clipboard paste capability available"
    return 1
}

# WSLクリップボード機能テスト
test_wsl_clipboard() {
    echo "=== WSL Clipboard Integration Test ==="
    
    if ! is_wsl_environment; then
        echo "❌ Not running in WSL environment"
        return 1
    fi
    
    local test_text="WSL Clipboard Test - $(date)"
    
    # コピーテスト
    echo "Testing clipboard copy..."
    if wsl_clipboard_copy "$test_text"; then
        echo "✅ Clipboard copy successful"
    else
        echo "❌ Clipboard copy failed"
        return 1
    fi
    
    # 貼り付けテスト
    echo "Testing clipboard paste..."
    local pasted_text
    pasted_text=$(wsl_clipboard_paste)
    
    if [[ "$pasted_text" == "$test_text" ]]; then
        echo "✅ Clipboard paste successful"
    else
        echo "❌ Clipboard paste failed or text mismatch"
        echo "  Expected: $test_text"
        echo "  Got: $pasted_text"
        return 1
    fi
    
    echo "WSL Clipboard test completed successfully"
    return 0
}

# WSL環境最適化
optimize_wsl_environment() {
    log "INFO" "Optimizing WSL environment for Claude Voice"
    
    if ! is_wsl_environment; then
        log "WARN" "Not running in WSL environment - skipping WSL optimization"
        return 1
    fi
    
    # WSL固有の環境変数設定
    export CLAUDE_WSL_MODE="true"
    export CLAUDE_AUDIO_BACKEND="windows"
    export WSL_VERSION=$(detect_wsl_version)
    
    log "DEBUG" "WSL environment variables set: WSL_VERSION=$WSL_VERSION"
    
    # Windows側アプリケーションパスの確認と設定
    local windows_paths=(
        "/mnt/c/Windows/System32"
        "/mnt/c/Program Files"
        "/mnt/c/Program Files (x86)"
    )
    
    for path in "${windows_paths[@]}"; do
        if [[ -d "$path" ]]; then
            case "$path" in
                "/mnt/c/Windows/System32")
                    export WINDOWS_SYSTEM32="$path"
                    log "DEBUG" "Windows System32 path detected: $WINDOWS_SYSTEM32"
                    ;;
                "/mnt/c/Program Files")
                    export WINDOWS_PROGRAM_FILES="$path"
                    log "DEBUG" "Windows Program Files path detected: $WINDOWS_PROGRAM_FILES"
                    ;;
                "/mnt/c/Program Files (x86)")
                    export WINDOWS_PROGRAM_FILES_X86="$path"
                    log "DEBUG" "Windows Program Files (x86) path detected: $WINDOWS_PROGRAM_FILES_X86"
                    ;;
            esac
        fi
    done
    
    # PowerShellの事前キャッシュ
    local powershell_path
    powershell_path=$(find_powershell_path)
    if [[ -n "$powershell_path" ]]; then
        export CLAUDE_POWERSHELL_PATH="$powershell_path"
        log "DEBUG" "PowerShell path cached: $CLAUDE_POWERSHELL_PATH"
    fi
    
    # クリップボード機能のテスト
    wsl_clipboard_copy "test" >/dev/null 2>&1
    local clipboard_test_result=$?
    if [[ $clipboard_test_result -eq 0 ]]; then
        export CLAUDE_CLIPBOARD_AVAILABLE="true"
        log "DEBUG" "WSL clipboard integration verified"
    else
        export CLAUDE_CLIPBOARD_AVAILABLE="false"
        log "WARN" "WSL clipboard integration not available"
    fi
    
    # WSL固有のパフォーマンス最適化
    # ファイルシステムキャッシュの最適化（可能な場合）
    if [[ -w /proc/sys/vm/drop_caches ]]; then
        log "DEBUG" "File system cache optimization available"
    fi
    
    # Windows側の時刻同期確認（WSL1の場合に重要）
    if [[ "$WSL_VERSION" == "WSL1" ]]; then
        log "DEBUG" "WSL1 detected - time sync considerations may apply"
    fi
    
    log "INFO" "WSL environment optimization completed"
    return 0
}

# WSL フォールバック通知（GUI通知が利用できない場合）
wsl_fallback_notification() {
    local title="$1"
    local message="$2"
    local urgency="${3:-normal}"
    
    # PowerShell通知の試行
    local powershell_path
    powershell_path=$(find_powershell_path)
    if [[ -n "$powershell_path" ]]; then
        local notification_script="
Add-Type -AssemblyName System.Windows.Forms
\$notification = New-Object System.Windows.Forms.NotifyIcon
\$notification.Icon = [System.Drawing.SystemIcons]::Information
\$notification.BalloonTipTitle = '$title'
\$notification.BalloonTipText = '$message'
\$notification.Visible = \$true
\$notification.ShowBalloonTip(5000)
Start-Sleep -Seconds 1
\$notification.Dispose()
"
        if execute_powershell_script "$notification_script" 10 "$powershell_path" >/dev/null 2>&1; then
            log "DEBUG" "WSL notification sent via PowerShell"
            return 0
        fi
    fi
    
    # コンソール通知（フォールバック）
    local urgency_prefix=""
    case "$urgency" in
        "critical") urgency_prefix="🚨 " ;;
        "high") urgency_prefix="⚠️  " ;;
        "normal") urgency_prefix="ℹ️  " ;;
        "low") urgency_prefix="💡 " ;;
    esac
    
    echo "${urgency_prefix}通知: $title"
    echo "  $message"
    
    # システムビープ（可能な場合）
    if command -v printf >/dev/null 2>&1; then
        printf '\a' 2>/dev/null
    fi
    
    return 0
}

# WSL統合のシンプルな情報取得
get_wsl_simple_info() {
    if ! is_wsl_environment; then
        echo "Not WSL"
        return 1
    fi
    
    echo "WSL $(detect_wsl_version)"
    if [[ -n "${WSL_DISTRO_NAME:-}" ]]; then
        echo "($WSL_DISTRO_NAME)"
    fi
}

# WSL基本機能テスト
test_wsl_basic_functions() {
    echo "=== WSL Basic Functions Test ==="
    
    # WSL環境検出テスト
    if is_wsl_environment; then
        echo "✅ WSL environment detected"
        echo "  Version: $(detect_wsl_version)"
        echo "  Distribution: $(get_wsl_info distro)"
    else
        echo "❌ Not running in WSL environment"
        return 1
    fi
    
    # PowerShell統合テスト
    local powershell_path
    powershell_path=$(find_powershell_path)
    if [[ -n "$powershell_path" ]]; then
        echo "✅ PowerShell integration available: $powershell_path"
    else
        echo "❌ PowerShell integration not available"
    fi
    
    # Windows側ファイルシステムアクセステスト
    if [[ -d "/mnt/c/Windows" ]]; then
        echo "✅ Windows filesystem accessible"
    else
        echo "❌ Windows filesystem not accessible"
    fi
    
    # クリップボード機能テスト
    test_wsl_clipboard
    
    return 0
}

# WSL統合初期化
init_wsl_integration() {
    log "DEBUG" "Initializing WSL integration"
    
    if ! is_wsl_environment; then
        log "WARN" "Not running in WSL - WSL integration disabled"
        return 1
    fi
    
    # 基本情報の取得とキャッシュ
    WSL_INFO_CACHE=$(get_wsl_info all)
    
    # 環境最適化
    optimize_wsl_environment
    
    log "INFO" "WSL Integration initialized: $(detect_wsl_version)"
    return 0
}

# WSL統合情報取得
get_wsl_integration_info() {
    local format="${1:-json}"
    
    case "$format" in
        "json")
            cat <<EOF
{
    "wsl_detected": $(if is_wsl_environment; then echo "true"; else echo "false"; fi),
    "wsl_version": "$(detect_wsl_version)",
    "distro": "$(get_wsl_info distro 2>/dev/null || echo "unknown")",
    "windows_build": "$(get_wsl_info windows_build 2>/dev/null || echo "unknown")",
    "clipboard_available": "${CLAUDE_CLIPBOARD_AVAILABLE:-unknown}",
    "powershell_available": $(if [[ -n "$(find_powershell_path 2>/dev/null)" ]]; then echo "true"; else echo "false"; fi)
}
EOF
            ;;
        "text")
            echo "WSL Integration Status:"
            echo "  WSL Environment: $(if is_wsl_environment; then echo "Yes"; else echo "No"; fi)"
            echo "  WSL Version: $(detect_wsl_version)"
            echo "  Distribution: $(get_wsl_info distro 2>/dev/null || echo "unknown")"
            echo "  Windows Build: $(get_wsl_info windows_build 2>/dev/null || echo "unknown")"
            echo "  Clipboard Available: ${CLAUDE_CLIPBOARD_AVAILABLE:-unknown}"
            echo "  PowerShell Available: $(if [[ -n "$(find_powershell_path 2>/dev/null)" ]]; then echo "Yes"; else echo "No"; fi)"
            ;;
        *)
            log "ERROR" "Unknown format: $format"
            return 1
            ;;
    esac
}