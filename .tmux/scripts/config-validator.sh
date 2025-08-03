#!/bin/bash
# TMux統一設定検証システム
# YAML設定ファイルと生成された設定の整合性・安全性・パフォーマンスを検証
# Version: 1.0

set -euo pipefail

# ====================================
# 定数定義
# ====================================
readonly SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
readonly TMUX_DIR="$(dirname "$SCRIPT_DIR")"
readonly YAML_CONFIG="$TMUX_DIR/config/tmux-unified.yaml"
readonly GENERATED_DIR="$TMUX_DIR/generated"
readonly LOG_FILE="$TMUX_DIR/claude/logs/config-validator.log"
readonly TEMP_DIR="/tmp/tmux-validator-$$"

# 検証結果の定数
readonly PASS=0
readonly WARNING=1
readonly ERROR=2

# ====================================
# ユーティリティ関数
# ====================================

# ログ出力
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
log_debug() { 
    [[ "${LOG_LEVEL:-INFO}" == "DEBUG" ]] && log "DEBUG" "$@" || true
}

# 検証結果の管理
declare -g validation_results=()
declare -g error_count=0
declare -g warning_count=0

add_result() {
    local severity="$1"
    local category="$2"
    local message="$3"
    local suggestion="${4:-}"
    
    local result="[$severity] [$category] $message"
    [[ -n "$suggestion" ]] && result="$result | 提案: $suggestion"
    
    validation_results+=("$result")
    
    case "$severity" in
        "ERROR")
            ((error_count++))
            log_error "$category: $message"
            ;;
        "WARNING")
            ((warning_count++))
            log_warn "$category: $message"
            ;;
        "PASS")
            log_info "$category: $message"
            ;;
    esac
}

# 清掃処理
cleanup() {
    [[ -d "$TEMP_DIR" ]] && rm -rf "$TEMP_DIR"
}
trap cleanup EXIT

# 初期化
initialize() {
    mkdir -p "$TEMP_DIR"
    mkdir -p "$(dirname "$LOG_FILE")"
    
    log_info "TMux設定検証システム開始"
    log_info "YAML設定: $YAML_CONFIG"
    log_info "生成済み設定: $GENERATED_DIR"
}

# ====================================
# YAML設定検証
# ====================================

# YAML構文検証
validate_yaml_syntax() {
    log_info "YAML構文検証を開始..."
    
    if ! [[ -f "$YAML_CONFIG" ]]; then
        add_result "ERROR" "YAML_SYNTAX" "YAML設定ファイルが見つかりません: $YAML_CONFIG"
        return $ERROR
    fi
    
    # yqによる構文チェック
    if command -v yq &> /dev/null; then
        if yq eval '.' "$YAML_CONFIG" > /dev/null 2>&1; then
            add_result "PASS" "YAML_SYNTAX" "YAML構文チェック成功"
        else
            add_result "ERROR" "YAML_SYNTAX" "YAML構文エラーを検出" "yq eval でエラー詳細を確認してください"
            return $ERROR
        fi
    else
        # 基本的な構文チェック
        if python3 -c "import yaml; yaml.safe_load(open('$YAML_CONFIG'))" 2>/dev/null; then
            add_result "PASS" "YAML_SYNTAX" "YAML構文チェック成功 (Python yaml)"
        elif python -c "import yaml; yaml.safe_load(open('$YAML_CONFIG'))" 2>/dev/null; then
            add_result "PASS" "YAML_SYNTAX" "YAML構文チェック成功 (Python2 yaml)"
        else
            add_result "WARNING" "YAML_SYNTAX" "YAML構文チェックをスキップ (yq/python-yaml未インストール)"
        fi
    fi
    
    return $PASS
}

# YAML構造検証
validate_yaml_structure() {
    log_info "YAML構造検証を開始..."
    
    # 必須セクションの確認
    local required_sections=(
        "base"
        "platforms"
        "claude_voice"
        "shared"
        "metadata"
    )
    
    for section in "${required_sections[@]}"; do
        if command -v yq &> /dev/null; then
            if [[ "$(yq eval ".$section" "$YAML_CONFIG")" == "null" ]]; then
                add_result "ERROR" "YAML_STRUCTURE" "必須セクション未定義: $section"
            else
                add_result "PASS" "YAML_STRUCTURE" "必須セクション確認: $section"
            fi
        else
            if grep -q "^${section}:" "$YAML_CONFIG"; then
                add_result "PASS" "YAML_STRUCTURE" "必須セクション確認: $section"
            else
                add_result "ERROR" "YAML_STRUCTURE" "必須セクション未定義: $section"
            fi
        fi
    done
    
    # メタデータ検証
    validate_metadata
    
    return $PASS
}

# メタデータ検証
validate_metadata() {
    log_info "メタデータ検証を開始..."
    
    local version schema_version
    if command -v yq &> /dev/null; then
        version=$(yq eval '.metadata.version' "$YAML_CONFIG")
        schema_version=$(yq eval '.metadata.schema_version' "$YAML_CONFIG")
    else
        version=$(grep -E "^[[:space:]]*version:" "$YAML_CONFIG" | head -1 | sed 's/.*:[[:space:]]*//' | sed 's/["'\'']//g' || echo "")
        schema_version=$(grep -E "^[[:space:]]*schema_version:" "$YAML_CONFIG" | head -1 | sed 's/.*:[[:space:]]*//' | sed 's/["'\'']//g' || echo "")
    fi
    
    if [[ -z "$version" || "$version" == "null" ]]; then
        add_result "ERROR" "METADATA" "バージョン情報が未定義" "metadata.versionを設定してください"
    else
        add_result "PASS" "METADATA" "バージョン情報: $version"
    fi
    
    if [[ -z "$schema_version" || "$schema_version" == "null" ]]; then
        add_result "WARNING" "METADATA" "スキーマバージョンが未定義" "metadata.schema_versionを設定することを推奨"
    else
        add_result "PASS" "METADATA" "スキーマバージョン: $schema_version"
    fi
}

# ====================================
# TMux設定検証
# ====================================

# tmux設定構文検証
validate_tmux_syntax() {
    log_info "TMux設定構文検証を開始..."
    
    if ! [[ -d "$GENERATED_DIR" ]]; then
        add_result "WARNING" "TMUX_SYNTAX" "生成済み設定ディレクトリが見つかりません: $GENERATED_DIR" "設定生成スクリプトを先に実行してください"
        return $WARNING
    fi
    
    # 生成済み設定ファイルの検証
    local config_files
    config_files=($(find "$GENERATED_DIR" -name "*.conf" -type f))
    
    if [[ ${#config_files[@]} -eq 0 ]]; then
        add_result "WARNING" "TMUX_SYNTAX" "検証対象の設定ファイルが見つかりません"
        return $WARNING
    fi
    
    for config_file in "${config_files[@]}"; do
        validate_single_tmux_config "$config_file"
    done
    
    return $PASS
}

# 単一tmux設定ファイルの検証
validate_single_tmux_config() {
    local config_file="$1"
    local filename
    filename=$(basename "$config_file")
    
    log_debug "TMux設定ファイル検証: $filename"
    
    # tmux構文チェック
    if tmux -f "$config_file" list-sessions -F "#{session_name}" 2>/dev/null | head -1 >/dev/null; then
        add_result "PASS" "TMUX_SYNTAX" "構文チェック成功: $filename"
    else
        # エラー詳細の取得
        local error_output
        error_output=$(tmux -f "$config_file" list-sessions 2>&1 | head -5 || true)
        add_result "ERROR" "TMUX_SYNTAX" "構文エラー: $filename" "エラー詳細: $error_output"
    fi
    
    # 危険な設定パターンのチェック
    check_dangerous_patterns "$config_file"
    
    # パフォーマンス影響の確認
    check_performance_impact "$config_file"
}

# 危険な設定パターンのチェック
check_dangerous_patterns() {
    local config_file="$1"
    local filename
    filename=$(basename "$config_file")
    
    # 危険なパターンのリスト
    local dangerous_patterns=(
        "run-shell.*rm.*-rf"          # 危険な削除コマンド
        "run-shell.*sudo"             # sudo実行
        "run-shell.*curl.*\|.*sh"     # 外部スクリプトの直接実行
        "run-shell.*wget.*\|.*sh"     # 外部スクリプトの直接実行
        "set.*status-interval.*0"     # 無限ループの可能性
    )
    
    for pattern in "${dangerous_patterns[@]}"; do
        if grep -qE "$pattern" "$config_file"; then
            add_result "ERROR" "SECURITY" "危険なパターンを検出: $filename" "パターン: $pattern を確認してください"
        fi
    done
    
    # パスワード・秘密情報の確認
    if grep -qiE "(password|secret|key|token).*=" "$config_file"; then
        add_result "WARNING" "SECURITY" "機密情報の可能性: $filename" "パスワードや秘密鍵が含まれていないか確認してください"
    fi
}

# パフォーマンス影響の確認
check_performance_impact() {
    local config_file="$1"
    local filename
    filename=$(basename "$config_file")
    
    # status-intervalの確認
    local status_interval
    status_interval=$(grep -E "set.*status-interval" "$config_file" | tail -1 | sed 's/.*status-interval[[:space:]]*//' || echo "")
    
    if [[ -n "$status_interval" ]]; then
        if [[ "$status_interval" -lt 2 ]]; then
            add_result "WARNING" "PERFORMANCE" "高頻度更新設定: $filename" "status-interval $status_interval は CPU使用率を上げる可能性があります"
        elif [[ "$status_interval" -gt 30 ]]; then
            add_result "WARNING" "PERFORMANCE" "低頻度更新設定: $filename" "status-interval $status_interval は情報が古くなる可能性があります"
        else
            add_result "PASS" "PERFORMANCE" "適切な更新間隔: $filename (${status_interval}秒)"
        fi
    fi
    
    # history-limitの確認
    local history_limit
    history_limit=$(grep -E "set.*history-limit" "$config_file" | tail -1 | sed 's/.*history-limit[[:space:]]*//' || echo "")
    
    if [[ -n "$history_limit" ]]; then
        if [[ "$history_limit" -gt 100000 ]]; then
            add_result "WARNING" "PERFORMANCE" "大きなヒストリ制限: $filename" "history-limit $history_limit は メモリ使用量を増加させる可能性があります"
        else
            add_result "PASS" "PERFORMANCE" "適切なヒストリ制限: $filename ($history_limit)"
        fi
    fi
    
    # 重いrun-shellコマンドの確認
    local heavy_commands
    heavy_commands=$(grep -c "run-shell.*\(find\|grep\|curl\|wget\)" "$config_file" || echo "0")
    
    if [[ "$heavy_commands" -gt 5 ]]; then
        add_result "WARNING" "PERFORMANCE" "重い外部コマンドが多数: $filename" "$heavy_commands 個の重いrun-shellコマンドがあります"
    fi
}

# ====================================
# 依存関係検証
# ====================================

# 依存関係検証
validate_dependencies() {
    log_info "依存関係検証を開始..."
    
    # コマンド依存関係の確認
    check_command_dependencies
    
    # ファイル依存関係の確認
    check_file_dependencies
    
    # プラットフォーム依存関係の確認
    check_platform_dependencies
    
    return $PASS
}

# コマンド依存関係の確認
check_command_dependencies() {
    # 必須コマンド
    local required_commands=(
        "tmux"
        "bash"
    )
    
    # オプションコマンド（警告のみ）
    local optional_commands=(
        "yq"          # YAML解析
        "clip.exe"    # WSLクリップボード
        "explorer.exe" # WSLエクスプローラー
        "powershell.exe" # WSL PowerShell
        "osascript"   # macOS音声
    )
    
    for cmd in "${required_commands[@]}"; do
        if command -v "$cmd" &> /dev/null; then
            add_result "PASS" "DEPENDENCY" "必須コマンド利用可能: $cmd"
        else
            add_result "ERROR" "DEPENDENCY" "必須コマンド未インストール: $cmd" "$cmd をインストールしてください"
        fi
    done
    
    for cmd in "${optional_commands[@]}"; do
        if command -v "$cmd" &> /dev/null; then
            add_result "PASS" "DEPENDENCY" "オプションコマンド利用可能: $cmd"
        else
            add_result "WARNING" "DEPENDENCY" "オプションコマンド未インストール: $cmd" "一部機能が制限される可能性があります"
        fi
    done
}

# ファイル依存関係の確認
check_file_dependencies() {
    # 重要なディレクトリ・ファイル
    local important_paths=(
        "$TMUX_DIR/claude/core"
        "$TMUX_DIR/claude/platforms"
        "$TMUX_DIR/scripts"
    )
    
    for path in "${important_paths[@]}"; do
        if [[ -e "$path" ]]; then
            add_result "PASS" "FILE_DEPENDENCY" "重要パス存在確認: $(basename "$path")"
        else
            add_result "WARNING" "FILE_DEPENDENCY" "重要パス未発見: $path" "関連機能が動作しない可能性があります"
        fi
    done
}

# プラットフォーム依存関係の確認
check_platform_dependencies() {
    local platform
    platform=$(detect_current_platform)
    
    add_result "PASS" "PLATFORM" "検出されたプラットフォーム: $platform"
    
    case "$platform" in
        "wsl")
            check_wsl_dependencies
            ;;
        "macos")
            check_macos_dependencies
            ;;
        "linux")
            check_linux_dependencies
            ;;
    esac
}

# WSL依存関係の確認
check_wsl_dependencies() {
    # WSL固有コマンド
    local wsl_commands=("clip.exe" "explorer.exe" "powershell.exe")
    
    for cmd in "${wsl_commands[@]}"; do
        if command -v "$cmd" &> /dev/null; then
            add_result "PASS" "WSL_DEPENDENCY" "WSLコマンド利用可能: $cmd"
        else
            add_result "WARNING" "WSL_DEPENDENCY" "WSLコマンド未利用可能: $cmd" "WSL環境が正しく設定されていない可能性があります"
        fi
    done
    
    # Windows パスの確認
    if [[ -d "/mnt/c" ]]; then
        add_result "PASS" "WSL_DEPENDENCY" "Windows Cドライブアクセス可能"
    else
        add_result "WARNING" "WSL_DEPENDENCY" "Windows Cドライブアクセス不可" "WSLマウント設定を確認してください"
    fi
}

# macOS依存関係の確認
check_macos_dependencies() {
    if command -v osascript &> /dev/null; then
        add_result "PASS" "MACOS_DEPENDENCY" "macOS音声システム利用可能"
    else
        add_result "WARNING" "MACOS_DEPENDENCY" "macOS音声システム利用不可"
    fi
}

# Linux依存関係の確認
check_linux_dependencies() {
    # Linux音声システム
    local audio_systems=("pulseaudio" "alsamixer" "pactl")
    local audio_available=false
    
    for cmd in "${audio_systems[@]}"; do
        if command -v "$cmd" &> /dev/null; then
            add_result "PASS" "LINUX_DEPENDENCY" "Linux音声システム利用可能: $cmd"
            audio_available=true
            break
        fi
    done
    
    if [[ "$audio_available" == "false" ]]; then
        add_result "WARNING" "LINUX_DEPENDENCY" "Linux音声システム未検出" "pulseaudio または alsa をインストールしてください"
    fi
}

# ====================================
# 整合性検証
# ====================================

# YAML設定と生成設定の整合性確認
validate_consistency() {
    log_info "設定整合性検証を開始..."
    
    if ! [[ -d "$GENERATED_DIR" ]]; then
        add_result "WARNING" "CONSISTENCY" "生成済み設定が見つかりません" "設定生成スクリプトを実行してください"
        return $WARNING
    fi
    
    # プラットフォーム別設定の整合性確認
    local platforms=("wsl" "macos" "linux")
    
    for platform in "${platforms[@]}"; do
        local config_file="$GENERATED_DIR/tmux-${platform}.conf"
        if [[ -f "$config_file" ]]; then
            validate_platform_consistency "$platform" "$config_file"
        fi
    done
    
    return $PASS
}

# プラットフォーム別整合性確認
validate_platform_consistency() {
    local platform="$1"
    local config_file="$2"
    
    log_debug "プラットフォーム整合性確認: $platform"
    
    # YAML設定値と生成設定値の比較例
    if command -v yq &> /dev/null; then
        # プレフィックスキーの確認
        local yaml_prefix generated_prefix
        yaml_prefix=$(yq eval '.base.prefix.key' "$YAML_CONFIG")
        generated_prefix=$(grep -E "set.*prefix" "$config_file" | head -1 | awk '{print $4}' || echo "")
        
        if [[ "$yaml_prefix" == "$generated_prefix" ]]; then
            add_result "PASS" "CONSISTENCY" "プレフィックスキー整合性: $platform ($yaml_prefix)"
        else
            add_result "ERROR" "CONSISTENCY" "プレフィックスキー不整合: $platform" "YAML: $yaml_prefix, 生成: $generated_prefix"
        fi
        
        # 他の重要設定の確認...
    fi
}

# ====================================
# ユーティリティ関数
# ====================================

# プラットフォーム検出
detect_current_platform() {
    case "$(uname -s)" in
        Linux)
            if grep -qi microsoft /proc/version 2>/dev/null; then
                echo "wsl"
            else
                echo "linux"
            fi
            ;;
        Darwin) echo "macos" ;;
        CYGWIN*|MINGW*|MSYS*) echo "windows" ;;
        FreeBSD) echo "freebsd" ;;
        *) echo "unknown" ;;
    esac
}

# ====================================
# レポート生成
# ====================================

# 検証結果レポートの生成
generate_report() {
    local report_file="$TMUX_DIR/claude/logs/validation-report-$(date +%Y%m%d-%H%M%S).txt"
    
    log_info "検証レポートを生成中: $report_file"
    
    cat > "$report_file" << EOF
================================================================================
TMux統一設定検証レポート
================================================================================

実行時刻: $(date)
YAML設定: $YAML_CONFIG
生成設定: $GENERATED_DIR

================================================================================
検証サマリー
================================================================================

総検証項目数: ${#validation_results[@]}
エラー数: $error_count
警告数: $warning_count
成功数: $((${#validation_results[@]} - error_count - warning_count))

検証結果: $(if [[ $error_count -eq 0 ]]; then echo "SUCCESS"; else echo "FAILED"; fi)

================================================================================
詳細結果
================================================================================

EOF
    
    # 結果をカテゴリ別に分類
    local categories=("YAML_SYNTAX" "YAML_STRUCTURE" "METADATA" "TMUX_SYNTAX" "SECURITY" "PERFORMANCE" "DEPENDENCY" "WSL_DEPENDENCY" "MACOS_DEPENDENCY" "LINUX_DEPENDENCY" "FILE_DEPENDENCY" "PLATFORM" "CONSISTENCY")
    
    for category in "${categories[@]}"; do
        local category_results=()
        
        for result in "${validation_results[@]}"; do
            if [[ "$result" =~ \[$category\] ]]; then
                category_results+=("$result")
            fi
        done
        
        if [[ ${#category_results[@]} -gt 0 ]]; then
            echo "" >> "$report_file"
            echo "--- $category ---" >> "$report_file"
            for result in "${category_results[@]}"; do
                echo "$result" >> "$report_file"
            done
        fi
    done
    
    # 推奨事項
    cat >> "$report_file" << EOF

================================================================================
推奨事項
================================================================================

EOF
    
    if [[ $error_count -gt 0 ]]; then
        echo "❌ $error_count 個のエラーを修正する必要があります" >> "$report_file"
    fi
    
    if [[ $warning_count -gt 0 ]]; then
        echo "⚠️  $warning_count 個の警告を確認することを推奨します" >> "$report_file"
    fi
    
    if [[ $error_count -eq 0 && $warning_count -eq 0 ]]; then
        echo "✅ すべての検証をパスしました" >> "$report_file"
    fi
    
    echo "" >> "$report_file"
    echo "詳細ログ: $LOG_FILE" >> "$report_file"
    
    log_info "検証レポート生成完了: $report_file"
    
    # コンソール出力
    echo ""
    echo "=================================================================================="
    echo "検証完了: エラー $error_count件, 警告 $warning_count件"
    echo "詳細レポート: $report_file"
    echo "=================================================================================="
}

# ====================================
# メイン処理
# ====================================

# 使用方法の表示
show_usage() {
    cat << EOF
TMux統一設定検証システム

使用方法:
  $0 [OPTIONS]

オプション:
  -h, --help     このヘルプを表示
  -v, --verbose  詳細ログを有効化
  -q, --quiet    エラーと警告のみ表示
  -r, --report   レポートファイルを生成 (デフォルト: 有効)
  --no-report    レポートファイルを生成しない

検証項目:
  - YAML構文・構造チェック
  - TMux設定構文チェック
  - セキュリティチェック
  - パフォーマンス影響チェック
  - 依存関係チェック
  - 設定整合性チェック

例:
  $0                # 標準検証実行
  $0 --verbose      # 詳細ログ付き検証
  $0 --quiet        # 簡潔出力

EOF
}

# メイン処理
main() {
    local verbose=false
    local quiet=false
    local generate_report=true
    
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
            -q|--quiet)
                quiet=true
                shift
                ;;
            -r|--report)
                generate_report=true
                shift
                ;;
            --no-report)
                generate_report=false
                shift
                ;;
            *)
                log_error "未知のオプション: $1"
                show_usage
                exit 1
                ;;
        esac
    done
    
    # 初期化
    initialize
    
    # 検証実行
    validate_yaml_syntax
    validate_yaml_structure
    validate_tmux_syntax
    validate_dependencies
    validate_consistency
    
    # レポート生成
    if [[ "$generate_report" == "true" ]]; then
        generate_report
    fi
    
    # 終了コードの決定
    if [[ $error_count -gt 0 ]]; then
        log_error "検証失敗: $error_count 個のエラーがあります"
        exit 1
    elif [[ $warning_count -gt 0 ]]; then
        log_warn "検証完了: $warning_count 個の警告があります"
        exit 0
    else
        log_info "検証成功: すべての検証をパスしました"
        exit 0
    fi
}

# スクリプト実行
if [[ "${BASH_SOURCE[0]}" == "${0}" ]]; then
    main "$@"
fi