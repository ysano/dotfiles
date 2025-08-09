#!/bin/bash
# TMux Scripts Validation Library
# Version: 1.0.0
#
# 検証・テスト関連のユーティリティ関数
# - ファイル検証
# - YAML検証
# - 設定検証
# - テストアサーション

# インポートガード
[[ -n "${_TMUX_VALIDATION_LOADED}" ]] && return 0
declare -gr _TMUX_VALIDATION_LOADED=1

# 依存ライブラリ
source "$(dirname "${BASH_SOURCE[0]}")/core.sh"
source "$(dirname "${BASH_SOURCE[0]}")/platform.sh"

# === 定数定義 ===
readonly VALIDATION_TEMP_DIR="${VALIDATION_TEMP_DIR:-/tmp/.tmux_validation_$$}"
readonly VALIDATION_REPORT_FILE="${VALIDATION_REPORT_FILE:-$VALIDATION_TEMP_DIR/report.txt}"

# 検証結果タイプ
readonly VALIDATION_PASS="PASS"
readonly VALIDATION_FAIL="FAIL"
readonly VALIDATION_WARN="WARN"
readonly VALIDATION_SKIP="SKIP"

# === 検証結果管理 ===

# 検証結果格納配列
declare -ga _VALIDATION_RESULTS=()
declare -gi _VALIDATION_PASS_COUNT=0
declare -gi _VALIDATION_FAIL_COUNT=0
declare -gi _VALIDATION_WARN_COUNT=0
declare -gi _VALIDATION_SKIP_COUNT=0

# 検証結果追加
add_validation_result() {
    local type="$1"
    local category="$2"
    local message="$3"
    local details="${4:-}"
    
    local result="[$type] $category: $message"
    [[ -n "$details" ]] && result="$result - $details"
    
    _VALIDATION_RESULTS+=("$result")
    
    case "$type" in
        "$VALIDATION_PASS") ((_VALIDATION_PASS_COUNT++)) ;;
        "$VALIDATION_FAIL") ((_VALIDATION_FAIL_COUNT++)) ;;
        "$VALIDATION_WARN") ((_VALIDATION_WARN_COUNT++)) ;;
        "$VALIDATION_SKIP") ((_VALIDATION_SKIP_COUNT++)) ;;
    esac
    
    log_debug "$result"
}

# 検証結果クリア
clear_validation_results() {
    _VALIDATION_RESULTS=()
    _VALIDATION_PASS_COUNT=0
    _VALIDATION_FAIL_COUNT=0
    _VALIDATION_WARN_COUNT=0
    _VALIDATION_SKIP_COUNT=0
}

# 検証レポート生成
generate_validation_report() {
    local output_file="${1:-$VALIDATION_REPORT_FILE}"
    
    mkdir -p "$(dirname "$output_file")"
    
    {
        echo "=== TMux Scripts Validation Report ==="
        echo "Generated: $(date '+%Y-%m-%d %H:%M:%S')"
        echo ""
        echo "Summary:"
        echo "  PASS: $_VALIDATION_PASS_COUNT"
        echo "  FAIL: $_VALIDATION_FAIL_COUNT"
        echo "  WARN: $_VALIDATION_WARN_COUNT"
        echo "  SKIP: $_VALIDATION_SKIP_COUNT"
        echo ""
        echo "Details:"
        for result in "${_VALIDATION_RESULTS[@]}"; do
            echo "  $result"
        done
    } > "$output_file"
    
    log_info "検証レポート生成: $output_file"
    
    # 結果を返す（0 = すべて成功、1 = 失敗あり）
    [[ $_VALIDATION_FAIL_COUNT -eq 0 ]]
}

# === ファイル検証 ===

# ファイル存在検証
validate_file_exists() {
    local file="$1"
    local category="${2:-File}"
    
    if [[ -f "$file" ]]; then
        add_validation_result "$VALIDATION_PASS" "$category" "ファイル存在確認" "$file"
        return 0
    else
        add_validation_result "$VALIDATION_FAIL" "$category" "ファイルが見つかりません" "$file"
        return 1
    fi
}

# ディレクトリ存在検証
validate_dir_exists() {
    local dir="$1"
    local category="${2:-Directory}"
    
    if [[ -d "$dir" ]]; then
        add_validation_result "$VALIDATION_PASS" "$category" "ディレクトリ存在確認" "$dir"
        return 0
    else
        add_validation_result "$VALIDATION_FAIL" "$category" "ディレクトリが見つかりません" "$dir"
        return 1
    fi
}

# ファイル権限検証
validate_file_permissions() {
    local file="$1"
    local expected_perms="$2"  # 例: "755", "644"
    local category="${3:-Permissions}"
    
    if [[ ! -e "$file" ]]; then
        add_validation_result "$VALIDATION_SKIP" "$category" "ファイルが存在しません" "$file"
        return 2
    fi
    
    local actual_perms=$(stat -f%OLp "$file" 2>/dev/null || stat -c%a "$file" 2>/dev/null)
    
    if [[ "$actual_perms" == "$expected_perms" ]]; then
        add_validation_result "$VALIDATION_PASS" "$category" "権限確認" "$file: $actual_perms"
        return 0
    else
        add_validation_result "$VALIDATION_FAIL" "$category" "権限不一致" "$file: expected=$expected_perms, actual=$actual_perms"
        return 1
    fi
}

# === YAML検証 ===

# YAML構文検証
validate_yaml_syntax() {
    local yaml_file="$1"
    local category="${2:-YAML}"
    
    if [[ ! -f "$yaml_file" ]]; then
        add_validation_result "$VALIDATION_SKIP" "$category" "YAMLファイルが存在しません" "$yaml_file"
        return 2
    fi
    
    # yqを使用（利用可能な場合）
    if command_exists yq; then
        if yq eval '.' "$yaml_file" >/dev/null 2>&1; then
            add_validation_result "$VALIDATION_PASS" "$category" "YAML構文検証" "$yaml_file"
            return 0
        else
            add_validation_result "$VALIDATION_FAIL" "$category" "YAML構文エラー" "$yaml_file"
            return 1
        fi
    
    # python-yamlを使用（利用可能な場合）
    elif command_exists python3; then
        if python3 -c "import yaml; yaml.safe_load(open('$yaml_file'))" 2>/dev/null; then
            add_validation_result "$VALIDATION_PASS" "$category" "YAML構文検証" "$yaml_file"
            return 0
        else
            add_validation_result "$VALIDATION_FAIL" "$category" "YAML構文エラー" "$yaml_file"
            return 1
        fi
    else
        add_validation_result "$VALIDATION_WARN" "$category" "YAML検証ツールが利用できません" "$yaml_file"
        return 2
    fi
}

# YAMLキー存在検証
validate_yaml_key() {
    local yaml_file="$1"
    local key_path="$2"  # 例: ".integration.enabled"
    local category="${3:-YAML Key}"
    
    if [[ ! -f "$yaml_file" ]]; then
        add_validation_result "$VALIDATION_SKIP" "$category" "YAMLファイルが存在しません" "$yaml_file"
        return 2
    fi
    
    # yqを使用
    if command_exists yq; then
        local value=$(yq eval "$key_path" "$yaml_file" 2>/dev/null)
        if [[ "$value" != "null" ]]; then
            add_validation_result "$VALIDATION_PASS" "$category" "キー存在確認" "$key_path in $yaml_file"
            return 0
        else
            add_validation_result "$VALIDATION_FAIL" "$category" "キーが見つかりません" "$key_path in $yaml_file"
            return 1
        fi
    else
        add_validation_result "$VALIDATION_WARN" "$category" "yqが利用できません" "$yaml_file"
        return 2
    fi
}

# === tmux設定検証 ===

# tmux構文検証
validate_tmux_syntax() {
    local config_file="$1"
    local category="${2:-TMux Config}"
    
    if [[ ! -f "$config_file" ]]; then
        add_validation_result "$VALIDATION_SKIP" "$category" "設定ファイルが存在しません" "$config_file"
        return 2
    fi
    
    if ! command_exists tmux; then
        add_validation_result "$VALIDATION_SKIP" "$category" "tmuxが利用できません" ""
        return 2
    fi
    
    # tmux構文チェック
    if tmux -f "$config_file" start-server \; list-sessions \; kill-server 2>/dev/null; then
        add_validation_result "$VALIDATION_PASS" "$category" "tmux構文検証" "$config_file"
        return 0
    else
        add_validation_result "$VALIDATION_FAIL" "$category" "tmux構文エラー" "$config_file"
        return 1
    fi
}

# === コマンド検証 ===

# コマンド存在検証
validate_command_exists() {
    local command="$1"
    local category="${2:-Command}"
    
    if command_exists "$command"; then
        local path=$(command -v "$command")
        add_validation_result "$VALIDATION_PASS" "$category" "コマンド存在確認" "$command: $path"
        return 0
    else
        add_validation_result "$VALIDATION_FAIL" "$category" "コマンドが見つかりません" "$command"
        return 1
    fi
}

# 複数コマンド検証
validate_commands_exist() {
    local category="${1:-Commands}"
    shift
    local commands=("$@")
    local all_pass=true
    
    for cmd in "${commands[@]}"; do
        if ! validate_command_exists "$cmd" "$category"; then
            all_pass=false
        fi
    done
    
    [[ "$all_pass" == "true" ]]
}

# === パフォーマンス検証 ===

# スクリプト実行時間検証
validate_performance() {
    local command="$1"
    local max_time_ms="$2"  # ミリ秒
    local category="${3:-Performance}"
    
    local start_time=$(date +%s%N)
    eval "$command" >/dev/null 2>&1
    local exit_code=$?
    local end_time=$(date +%s%N)
    
    local duration=$(( (end_time - start_time) / 1000000 ))
    
    if [[ $exit_code -ne 0 ]]; then
        add_validation_result "$VALIDATION_FAIL" "$category" "コマンド実行失敗" "$command"
        return 1
    elif [[ $duration -le $max_time_ms ]]; then
        add_validation_result "$VALIDATION_PASS" "$category" "パフォーマンス検証" "$command: ${duration}ms <= ${max_time_ms}ms"
        return 0
    else
        add_validation_result "$VALIDATION_WARN" "$category" "パフォーマンス警告" "$command: ${duration}ms > ${max_time_ms}ms"
        return 1
    fi
}

# === 危険パターン検証 ===

# 危険なコマンドパターンチェック
validate_no_dangerous_patterns() {
    local file="$1"
    local category="${2:-Security}"
    
    if [[ ! -f "$file" ]]; then
        add_validation_result "$VALIDATION_SKIP" "$category" "ファイルが存在しません" "$file"
        return 2
    fi
    
    local dangerous_patterns=(
        'rm -rf /'
        'rm -rf /*'
        'chmod 777'
        'eval.*\$\('
        'curl.*\|.*bash'
        'wget.*\|.*sh'
    )
    
    local found_patterns=()
    for pattern in "${dangerous_patterns[@]}"; do
        if grep -qE "$pattern" "$file"; then
            found_patterns+=("$pattern")
        fi
    done
    
    if [[ ${#found_patterns[@]} -eq 0 ]]; then
        add_validation_result "$VALIDATION_PASS" "$category" "危険パターンなし" "$file"
        return 0
    else
        add_validation_result "$VALIDATION_WARN" "$category" "危険パターン検出" "$file: ${found_patterns[*]}"
        return 1
    fi
}

# === テストアサーション ===

# 値の等価性検証
assert_equals() {
    local expected="$1"
    local actual="$2"
    local message="${3:-値の等価性}"
    local category="${4:-Assert}"
    
    if [[ "$expected" == "$actual" ]]; then
        add_validation_result "$VALIDATION_PASS" "$category" "$message" "expected=$expected"
        return 0
    else
        add_validation_result "$VALIDATION_FAIL" "$category" "$message" "expected=$expected, actual=$actual"
        return 1
    fi
}

# 文字列包含検証
assert_contains() {
    local haystack="$1"
    local needle="$2"
    local message="${3:-文字列包含}"
    local category="${4:-Assert}"
    
    if [[ "$haystack" == *"$needle"* ]]; then
        add_validation_result "$VALIDATION_PASS" "$category" "$message" "'$needle' found"
        return 0
    else
        add_validation_result "$VALIDATION_FAIL" "$category" "$message" "'$needle' not found in '$haystack'"
        return 1
    fi
}

# 条件検証
assert_true() {
    local condition="$1"
    local message="${2:-条件検証}"
    local category="${3:-Assert}"
    
    if eval "$condition"; then
        add_validation_result "$VALIDATION_PASS" "$category" "$message" "$condition"
        return 0
    else
        add_validation_result "$VALIDATION_FAIL" "$category" "$message" "$condition"
        return 1
    fi
}

# ファイル内容検証
assert_file_contains() {
    local file="$1"
    local pattern="$2"
    local message="${3:-ファイル内容}"
    local category="${4:-Assert}"
    
    if [[ ! -f "$file" ]]; then
        add_validation_result "$VALIDATION_SKIP" "$category" "$message" "ファイルが存在しません: $file"
        return 2
    fi
    
    if grep -q "$pattern" "$file"; then
        add_validation_result "$VALIDATION_PASS" "$category" "$message" "'$pattern' found in $file"
        return 0
    else
        add_validation_result "$VALIDATION_FAIL" "$category" "$message" "'$pattern' not found in $file"
        return 1
    fi
}

# === 一括検証 ===

# 設定ファイル一括検証
validate_config_files() {
    local config_dir="$1"
    local category="${2:-Config Files}"
    
    if [[ ! -d "$config_dir" ]]; then
        add_validation_result "$VALIDATION_FAIL" "$category" "設定ディレクトリが見つかりません" "$config_dir"
        return 1
    fi
    
    local all_pass=true
    
    # YAMLファイル検証
    for yaml_file in "$config_dir"/*.yaml "$config_dir"/*.yml; do
        [[ -f "$yaml_file" ]] && validate_yaml_syntax "$yaml_file" "$category" || true
    done
    
    # tmux設定ファイル検証
    for tmux_file in "$config_dir"/*.conf; do
        [[ -f "$tmux_file" ]] && validate_tmux_syntax "$tmux_file" "$category" || true
    done
    
    [[ "$all_pass" == "true" ]]
}

# === 初期化 ===

# ライブラリ初期化
_init_validation() {
    mkdir -p "$VALIDATION_TEMP_DIR"
    register_cleanup "$VALIDATION_TEMP_DIR"
    
    if [[ "$TMUX_SCRIPTS_DEBUG" == "true" ]]; then
        log_debug "TMux Scripts Validation Library loaded"
        log_debug "Validation temp dir: $VALIDATION_TEMP_DIR"
    fi
}

# 自動初期化
_init_validation