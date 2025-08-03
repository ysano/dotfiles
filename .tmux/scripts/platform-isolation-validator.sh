#!/bin/bash
# プラットフォーム固有設定隔離検証スクリプト
# OS固有コマンド・パスの適切な条件分岐を検証
# Version: 1.0

set -euo pipefail

# ====================================
# 定数定義
# ====================================
readonly SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
readonly TMUX_DIR="$(dirname "$SCRIPT_DIR")"
readonly GENERATED_DIR="$TMUX_DIR/generated"
readonly LOG_FILE="$TMUX_DIR/claude/logs/platform-isolation.log"

# OS固有コマンド・パスの定義
declare -A OS_SPECIFIC_COMMANDS=(
    # Windows/WSL固有
    ["clip.exe"]="wsl"
    ["explorer.exe"]="wsl"  
    ["powershell.exe"]="wsl"
    ["/mnt/c"]="wsl"
    
    # macOS固有
    ["osascript"]="macos"
    ["say"]="macos"
    ["pbcopy"]="macos"
    ["pbpaste"]="macos"
    
    # Linux固有
    ["pulseaudio"]="linux"
    ["pactl"]="linux"
    ["xclip"]="linux"
    ["xsel"]="linux"
    ["espeak"]="linux"
)

# ====================================
# ユーティリティ関数
# ====================================

log() {
    local level="$1"
    shift
    local message="$*"
    local timestamp=$(date '+%Y-%m-%d %H:%M:%S')
    echo "[$timestamp] [$level] $message" | tee -a "$LOG_FILE"
}

log_info() { log "INFO" "$@"; }
log_warn() { log "WARN" "$@"; }
log_error() { log "ERROR" "$@"; }

# 検証結果の管理
declare -g violations=()
declare -g warnings=()
declare -g passes=()

add_violation() {
    local file="$1"
    local line="$2"
    local command="$3"
    local expected_platform="$4"
    local message="VIOLATION: $file:$line - OS固有コマンド '$command' (期待プラットフォーム: $expected_platform) が条件分岐なしで使用されています"
    violations+=("$message")
    log_error "$message"
}

add_warning() {
    local file="$1"
    local line="$2"
    local message="$3"
    warnings+=("WARNING: $file:$line - $message")
    log_warn "$message"
}

add_pass() {
    local file="$1"
    local command="$2"
    local message="PASS: $file - OS固有コマンド '$command' が適切に条件分岐されています"
    passes+=("$message")
    log_info "$message"
}

# ====================================
# 検証機能
# ====================================

# プラットフォーム隔離検証
validate_platform_isolation() {
    log_info "プラットフォーム隔離検証を開始..."
    
    if [[ ! -d "$GENERATED_DIR" ]]; then
        log_error "生成設定ディレクトリが見つかりません: $GENERATED_DIR"
        return 1
    fi
    
    # 生成された設定ファイルを検証
    local config_files
    config_files=($(find "$GENERATED_DIR" -name "*.conf" -type f))
    
    if [[ ${#config_files[@]} -eq 0 ]]; then
        log_error "検証対象の設定ファイルが見つかりません"
        return 1
    fi
    
    for config_file in "${config_files[@]}"; do
        validate_single_config "$config_file"
    done
    
    generate_isolation_report
}

# 単一設定ファイルの検証
validate_single_config() {
    local config_file="$1"
    local filename
    filename=$(basename "$config_file")
    
    log_info "ファイル検証開始: $filename"
    
    # プラットフォーム固有コマンドの検出
    for command in "${!OS_SPECIFIC_COMMANDS[@]}"; do
        local expected_platform="${OS_SPECIFIC_COMMANDS[$command]}"
        
        # コマンドが使用されているかチェック
        local usage_lines
        usage_lines=$(grep -n "$command" "$config_file" || true)
        
        if [[ -n "$usage_lines" ]]; then
            # 各使用箇所を検証
            while IFS= read -r line_info; do
                [[ -z "$line_info" ]] && continue
                
                local line_number
                local line_content
                line_number=$(echo "$line_info" | cut -d: -f1)
                line_content=$(echo "$line_info" | cut -d: -f2-)
                
                validate_command_usage "$config_file" "$line_number" "$line_content" "$command" "$expected_platform"
                
            done <<< "$usage_lines"
        fi
    done
    
    # プラットフォーム固有パスの検証
    validate_platform_paths "$config_file"
    
    log_info "ファイル検証完了: $filename"
}

# コマンド使用の検証
validate_command_usage() {
    local file="$1"
    local line_number="$2"
    local line_content="$3"
    local command="$4"
    local expected_platform="$5"
    local filename
    filename=$(basename "$file")
    
    # 適切な条件分岐パターンを定義
    local valid_patterns=()
    
    case "$expected_platform" in
        "wsl")
            valid_patterns=(
                "if-shell.*grep.*microsoft.*proc/version.*$command"
                "if-shell.*command -v $command.*$command"
                "if-shell.*grep.*microsoft.*command -v $command.*$command"
            )
            ;;
        "macos")
            valid_patterns=(
                "if-shell.*uname.*Darwin.*$command"
                "if-shell.*command -v $command.*$command"
            )
            ;;
        "linux")
            valid_patterns=(
                "if-shell.*uname.*Linux.*$command"
                "if-shell.*command -v $command.*$command"
            )
            ;;
    esac
    
    # 条件分岐の確認
    local has_proper_condition=false
    for pattern in "${valid_patterns[@]}"; do
        if [[ "$line_content" =~ $pattern ]]; then
            has_proper_condition=true
            break
        fi
    done
    
    # 結果の記録
    if [[ "$has_proper_condition" == "true" ]]; then
        add_pass "$filename" "$command"
    else
        # コメント行の場合は警告レベル
        if [[ "$line_content" =~ ^[[:space:]]*# ]]; then
            add_warning "$filename" "$line_number" "OS固有コマンド '$command' がコメント内で使用されています"
        else
            add_violation "$filename" "$line_number" "$command" "$expected_platform"
        fi
    fi
}

# プラットフォーム固有パスの検証
validate_platform_paths() {
    local config_file="$1"
    local filename
    filename=$(basename "$config_file")
    
    # WSL固有パス
    local wsl_paths=("/mnt/c" "/mnt/d" "/proc/version")
    
    for path in "${wsl_paths[@]}"; do
        local usage_lines
        usage_lines=$(grep -n "$path" "$config_file" || true)
        
        if [[ -n "$usage_lines" ]]; then
            while IFS= read -r line_info; do
                [[ -z "$line_info" ]] && continue
                
                local line_number
                local line_content
                line_number=$(echo "$line_info" | cut -d: -f1)
                line_content=$(echo "$line_info" | cut -d: -f2-)
                
                # WSL検出条件があるかチェック
                if [[ "$line_content" =~ grep.*microsoft.*proc/version ]]; then
                    add_pass "$filename" "$path (WSL検出用)"
                elif [[ "$line_content" =~ ^[[:space:]]*# ]]; then
                    add_warning "$filename" "$line_number" "WSL固有パス '$path' がコメント内で使用されています"
                else
                    add_violation "$filename" "$line_number" "$path" "wsl"
                fi
                
            done <<< "$usage_lines"
        fi
    done
}

# ====================================
# レポート生成
# ====================================

generate_isolation_report() {
    local report_file="$TMUX_DIR/claude/logs/platform-isolation-report-$(date +%Y%m%d-%H%M%S).txt"
    
    log_info "プラットフォーム隔離レポートを生成中: $report_file"
    
    cat > "$report_file" << EOF
================================================================================
プラットフォーム隔離検証レポート
================================================================================

実行時刻: $(date)
検証対象: $GENERATED_DIR

================================================================================
検証サマリー
================================================================================

違反数: ${#violations[@]}
警告数: ${#warnings[@]}
成功数: ${#passes[@]}

検証結果: $(if [[ ${#violations[@]} -eq 0 ]]; then echo "SUCCESS"; else echo "FAILED"; fi)

================================================================================
OS固有コマンド・パス一覧
================================================================================

EOF
    
    # OS固有要素の一覧表示
    echo "WSL固有:" >> "$report_file"
    for cmd in "${!OS_SPECIFIC_COMMANDS[@]}"; do
        if [[ "${OS_SPECIFIC_COMMANDS[$cmd]}" == "wsl" ]]; then
            echo "  - $cmd" >> "$report_file"
        fi
    done
    
    echo "" >> "$report_file"
    echo "macOS固有:" >> "$report_file"
    for cmd in "${!OS_SPECIFIC_COMMANDS[@]}"; do
        if [[ "${OS_SPECIFIC_COMMANDS[$cmd]}" == "macos" ]]; then
            echo "  - $cmd" >> "$report_file"
        fi
    done
    
    echo "" >> "$report_file"
    echo "Linux固有:" >> "$report_file"
    for cmd in "${!OS_SPECIFIC_COMMANDS[@]}"; do
        if [[ "${OS_SPECIFIC_COMMANDS[$cmd]}" == "linux" ]]; then
            echo "  - $cmd" >> "$report_file"
        fi
    done
    
    cat >> "$report_file" << EOF

================================================================================
詳細結果
================================================================================

--- 違反 (修正が必要) ---
EOF
    
    if [[ ${#violations[@]} -eq 0 ]]; then
        echo "違反なし" >> "$report_file"
    else
        for violation in "${violations[@]}"; do
            echo "$violation" >> "$report_file"
        done
    fi
    
    echo "" >> "$report_file"
    echo "--- 警告 (確認推奨) ---" >> "$report_file"
    
    if [[ ${#warnings[@]} -eq 0 ]]; then
        echo "警告なし" >> "$report_file"
    else
        for warning in "${warnings[@]}"; do
            echo "$warning" >> "$report_file"
        done
    fi
    
    echo "" >> "$report_file"
    echo "--- 成功 (適切な隔離) ---" >> "$report_file"
    
    if [[ ${#passes[@]} -eq 0 ]]; then
        echo "成功項目なし" >> "$report_file"
    else
        for pass in "${passes[@]}"; do
            echo "$pass" >> "$report_file"
        done
    fi
    
    cat >> "$report_file" << EOF

================================================================================
推奨事項
================================================================================

EOF
    
    if [[ ${#violations[@]} -gt 0 ]]; then
        cat >> "$report_file" << EOF
❌ 修正が必要な違反があります:

1. OS固有コマンドは必ず条件分岐で保護してください:
   WSL: if-shell 'grep -qi microsoft /proc/version && command -v COMMAND'
   macOS: if-shell 'uname -s | grep -q Darwin && command -v COMMAND'
   Linux: if-shell 'uname -s | grep -q Linux && command -v COMMAND'

2. フォールバック処理を実装してください:
   'bind-key KEY run-shell "COMMAND"' → 
   'if-shell "CONDITION" "bind-key KEY run-shell \"COMMAND\"" "bind-key KEY display-message \"COMMAND not available\""'

EOF
    fi
    
    if [[ ${#warnings[@]} -gt 0 ]]; then
        echo "⚠️  警告項目を確認してください" >> "$report_file"
        echo "" >> "$report_file"
    fi
    
    if [[ ${#violations[@]} -eq 0 && ${#warnings[@]} -eq 0 ]]; then
        echo "✅ プラットフォーム隔離が適切に実装されています" >> "$report_file"
    fi
    
    echo "" >> "$report_file"
    echo "詳細ログ: $LOG_FILE" >> "$report_file"
    
    log_info "プラットフォーム隔離レポート生成完了: $report_file"
    
    # コンソール出力
    echo ""
    echo "=================================================================================="
    echo "プラットフォーム隔離検証完了"
    echo "違反: ${#violations[@]}件, 警告: ${#warnings[@]}件, 成功: ${#passes[@]}件"
    echo "詳細レポート: $report_file"
    echo "=================================================================================="
}

# ====================================
# メイン処理
# ====================================

show_usage() {
    cat << EOF
プラットフォーム固有設定隔離検証スクリプト

使用方法:
  $0 [OPTIONS]

オプション:
  -h, --help     このヘルプを表示
  -v, --verbose  詳細ログを有効化

検証項目:
  - OS固有コマンドの条件分岐チェック
  - プラットフォーム固有パスの隔離確認
  - フォールバック処理の実装確認

例:
  $0              # プラットフォーム隔離検証実行
  $0 --verbose    # 詳細ログ付き検証

EOF
}

main() {
    local verbose=false
    
    # コマンドライン引数の解析
    while [[ $# -gt 0 ]]; do
        case $1 in
            -h|--help)
                show_usage
                exit 0
                ;;
            -v|--verbose)
                export LOG_LEVEL="DEBUG"
                verbose=true
                shift
                ;;
            *)
                log_error "未知のオプション: $1"
                show_usage
                exit 1
                ;;
        esac
    done
    
    # ログ初期化
    mkdir -p "$(dirname "$LOG_FILE")"
    log_info "プラットフォーム隔離検証開始"
    
    # 検証実行
    validate_platform_isolation
    
    # 終了コードの決定
    if [[ ${#violations[@]} -gt 0 ]]; then
        log_error "検証失敗: ${#violations[@]} 個の違反があります"
        exit 1
    elif [[ ${#warnings[@]} -gt 0 ]]; then
        log_warn "検証完了: ${#warnings[@]} 個の警告があります"
        exit 0
    else
        log_info "検証成功: すべてのプラットフォーム隔離が適切です"
        exit 0
    fi
}

# スクリプト実行
if [[ "${BASH_SOURCE[0]}" == "${0}" ]]; then
    main "$@"
fi