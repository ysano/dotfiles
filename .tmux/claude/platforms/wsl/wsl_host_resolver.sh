#!/bin/bash
# WSL Windows Host IP Dynamic Resolver
# WSL環境でWindowsホストIPを動的に取得するユーティリティ

set -euo pipefail

# バージョン情報
VERSION="1.0.0"
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
CACHE_FILE="$HOME/.tmux/claude/.wsl_host_cache"
CACHE_TTL=300  # 5分間キャッシュ

# ログ関数
log_info() {
    echo "[INFO] $*" >&2
}

log_debug() {
    [[ "${WSL_DEBUG:-}" == "1" ]] && echo "[DEBUG] $*" >&2
}

log_error() {
    echo "[ERROR] $*" >&2
}

# キャッシュの確認
check_cache() {
    if [[ -f "$CACHE_FILE" ]]; then
        local cache_time cache_ip
        cache_time=$(stat -c %Y "$CACHE_FILE" 2>/dev/null || echo 0)
        current_time=$(date +%s)
        
        if (( current_time - cache_time < CACHE_TTL )); then
            cache_ip=$(cat "$CACHE_FILE" 2>/dev/null || echo "")
            if [[ -n "$cache_ip" ]] && validate_ip "$cache_ip"; then
                log_debug "Using cached Windows host IP: $cache_ip"
                echo "$cache_ip"
                return 0
            fi
        fi
    fi
    return 1
}

# IPアドレスの検証
validate_ip() {
    local ip="$1"
    if [[ $ip =~ ^[0-9]{1,3}\.[0-9]{1,3}\.[0-9]{1,3}\.[0-9]{1,3}$ ]]; then
        # 各オクテットが0-255の範囲内かチェック
        IFS='.' read -ra ADDR <<< "$ip"
        for octet in "${ADDR[@]}"; do
            if (( octet > 255 )); then
                return 1
            fi
        done
        return 0
    fi
    return 1
}

# Method 1: デフォルトゲートウェイから取得 (最も確実)
get_host_ip_via_gateway() {
    log_debug "Trying method 1: Default gateway"
    local gateway_ip
    gateway_ip=$(ip route show default | awk '/default/ { print $3 }' | head -1)
    
    if validate_ip "$gateway_ip"; then
        log_debug "Found gateway IP: $gateway_ip"
        echo "$gateway_ip"
        return 0
    fi
    return 1
}

# Method 2: /proc/net/route解析 (フォールバック)
get_host_ip_via_route() {
    log_debug "Trying method 2: /proc/net/route"
    local route_line gateway_hex gateway_ip
    route_line=$(awk 'NR==2' /proc/net/route 2>/dev/null || echo "")
    
    if [[ -n "$route_line" ]]; then
        gateway_hex=$(echo "$route_line" | awk '{print $3}')
        if [[ -n "$gateway_hex" ]] && [[ "$gateway_hex" != "00000000" ]]; then
            # ヘックスをリトルエンディアンでIPに変換
            gateway_ip=$(printf "%d.%d.%d.%d\n" \
                $((0x${gateway_hex:6:2})) \
                $((0x${gateway_hex:4:2})) \
                $((0x${gateway_hex:2:2})) \
                $((0x${gateway_hex:0:2})))
            
            if validate_ip "$gateway_ip"; then
                log_debug "Found route IP: $gateway_ip"
                echo "$gateway_ip"
                return 0
            fi
        fi
    fi
    return 1
}

# Method 3: nameserver解析 (WSL特有)
get_host_ip_via_nameserver() {
    log_debug "Trying method 3: nameserver in /etc/resolv.conf"
    local nameserver_ip
    nameserver_ip=$(awk '/^nameserver/ { print $2; exit }' /etc/resolv.conf 2>/dev/null || echo "")
    
    if validate_ip "$nameserver_ip" && [[ "$nameserver_ip" != "127.0.0.53" ]]; then
        log_debug "Found nameserver IP: $nameserver_ip"
        echo "$nameserver_ip"
        return 0
    fi
    return 1
}

# Method 4: Windows環境変数経由 (WSL2特有)
get_host_ip_via_wsl_env() {
    log_debug "Trying method 4: WSL environment variables"
    
    # WSL_HOST_IPが設定されている場合
    if [[ -n "${WSL_HOST_IP:-}" ]] && validate_ip "$WSL_HOST_IP"; then
        log_debug "Found WSL_HOST_IP: $WSL_HOST_IP"
        echo "$WSL_HOST_IP"
        return 0
    fi
    
    return 1
}

# 接続テスト
test_connection() {
    local host_ip="$1"
    local port="${2:-11434}"
    
    log_debug "Testing connection to $host_ip:$port"
    
    # 高速接続テスト (1秒タイムアウト)
    if timeout 1 bash -c "echo >/dev/tcp/$host_ip/$port" 2>/dev/null; then
        log_debug "Connection test successful"
        return 0
    fi
    
    log_debug "Connection test failed"
    return 1
}

# メインIP取得関数
get_windows_host_ip() {
    local host_ip=""
    
    # キャッシュチェック
    if host_ip=$(check_cache); then
        echo "$host_ip"
        return 0
    fi
    
    log_info "Resolving Windows host IP dynamically..."
    
    # 複数の方法を順番に試行
    local methods=(
        "get_host_ip_via_gateway"
        "get_host_ip_via_route" 
        "get_host_ip_via_nameserver"
        "get_host_ip_via_wsl_env"
    )
    
    for method in "${methods[@]}"; do
        if host_ip=$($method) && [[ -n "$host_ip" ]]; then
            log_info "Resolved Windows host IP: $host_ip (method: $method)"
            
            # 接続テスト (オプション)
            if [[ "${WSL_TEST_CONNECTION:-1}" == "1" ]]; then
                if test_connection "$host_ip"; then
                    echo "$host_ip" > "$CACHE_FILE"
                    echo "$host_ip"
                    return 0
                else
                    log_debug "Connection test failed for $host_ip, trying next method"
                    continue
                fi
            else
                echo "$host_ip" > "$CACHE_FILE"
                echo "$host_ip"
                return 0
            fi
        fi
    done
    
    log_error "Failed to resolve Windows host IP using all methods"
    return 1
}

# Ollama URL生成
get_ollama_url() {
    local host_ip port
    host_ip=$(get_windows_host_ip) || return 1
    port="${1:-11434}"
    echo "http://$host_ip:$port"
}

# キャッシュクリア
clear_cache() {
    rm -f "$CACHE_FILE"
    log_info "Cache cleared"
}

# メイン処理
main() {
    case "${1:-}" in
        "ip"|"")
            get_windows_host_ip
            ;;
        "url")
            get_ollama_url "${2:-11434}"
            ;;
        "test")
            local ip
            ip=$(get_windows_host_ip) || exit 1
            test_connection "$ip" "${2:-11434}"
            ;;
        "clear-cache")
            clear_cache
            ;;
        "version")
            echo "WSL Host Resolver v$VERSION"
            ;;
        "debug")
            export WSL_DEBUG=1
            get_windows_host_ip
            ;;
        *)
            cat << 'EOF'
WSL Windows Host IP Dynamic Resolver

Usage:
  wsl_host_resolver.sh [command] [options]

Commands:
  ip                    Get Windows host IP (default)
  url [port]           Get Ollama URL (default port: 11434)
  test [port]          Test connection to resolved IP
  clear-cache          Clear cached IP
  debug                Enable debug mode and resolve IP
  version              Show version information

Environment Variables:
  WSL_HOST_IP          Override IP resolution
  WSL_DEBUG            Enable debug output (1=on)
  WSL_TEST_CONNECTION  Test connection before caching (1=on, default)

Examples:
  wsl_host_resolver.sh ip
  wsl_host_resolver.sh url 11434
  wsl_host_resolver.sh test
  WSL_DEBUG=1 wsl_host_resolver.sh debug
EOF
            ;;
    esac
}

# スクリプト実行
if [[ "${BASH_SOURCE[0]}" == "${0}" ]]; then
    main "$@"
fi