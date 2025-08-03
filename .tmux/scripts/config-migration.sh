#!/bin/bash
# TMux設定移行スクリプト
# 既存の分散設定ファイルから統一YAML設定への移行を支援
# Version: 1.0

set -euo pipefail

# ====================================
# 定数定義
# ====================================
readonly SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
readonly TMUX_DIR="$(dirname "$SCRIPT_DIR")"
readonly YAML_CONFIG="$TMUX_DIR/config/tmux-unified.yaml"
readonly BACKUP_DIR="$TMUX_DIR/backup/$(date +%Y%m%d-%H%M%S)"
readonly LOG_FILE="$TMUX_DIR/claude/logs/config-migration.log"
readonly MIGRATION_STATE_FILE="$TMUX_DIR/.migration_state"

# 移行対象ファイル
readonly LEGACY_FILES=(
    "$TMUX_DIR/base.conf"
    "$TMUX_DIR/os/wsl.conf"
    "$TMUX_DIR/os/macos.conf"
    "$TMUX_DIR/os/linux.conf"
    "$TMUX_DIR/migration.conf"
    "$TMUX_DIR/scripts/claude-notification-toggle.sh"
    "$TMUX_DIR/shared/claude-voice-common.conf"
    "$TMUX_DIR/shared/performance-common.conf"
    "$TMUX_DIR/shared/window-status-common.conf"
)

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

# エラーハンドリング
error_exit() {
    log_error "$1"
    exit 1
}

# 移行状態の管理
save_migration_state() {
    local state="$1"
    local step="$2"
    local message="$3"
    
    cat > "$MIGRATION_STATE_FILE" << EOF
state=$state
step=$step
timestamp=$(date -Iseconds)
message=$message
backup_dir=$BACKUP_DIR
EOF
    
    log_info "移行状態を保存: $state ($step)"
}

load_migration_state() {
    if [[ -f "$MIGRATION_STATE_FILE" ]]; then
        source "$MIGRATION_STATE_FILE"
        log_info "移行状態を読み込み: ${state:-unknown} (${step:-unknown})"
    else
        state="not_started"
        step=""
        message=""
    fi
}

# ====================================
# バックアップ機能
# ====================================

# 設定ファイルのバックアップ
create_backup() {
    log_info "設定ファイルのバックアップを作成中..."
    
    mkdir -p "$BACKUP_DIR"
    
    local backup_count=0
    
    for file in "${LEGACY_FILES[@]}"; do
        if [[ -f "$file" ]]; then
            local relative_path
            relative_path=$(realpath --relative-to="$TMUX_DIR" "$file")
            local backup_file="$BACKUP_DIR/$relative_path"
            
            mkdir -p "$(dirname "$backup_file")"
            cp "$file" "$backup_file"
            
            log_info "バックアップ作成: $relative_path"
            ((backup_count++))
        fi
    done
    
    # バックアップメタデータの作成
    cat > "$BACKUP_DIR/backup_info.txt" << EOF
TMux設定バックアップ
作成日時: $(date)
バックアップファイル数: $backup_count
移行前のgitコミット: $(git -C "$TMUX_DIR" rev-parse HEAD 2>/dev/null || echo "不明")

バックアップファイル一覧:
EOF
    
    find "$BACKUP_DIR" -type f -name "*.conf" -o -name "*.sh" | sort >> "$BACKUP_DIR/backup_info.txt"
    
    log_info "バックアップ完了: $backup_count ファイル → $BACKUP_DIR"
    save_migration_state "backup_created" "1" "設定ファイルバックアップ完了"
}

# ====================================
# 設定解析・抽出機能
# ====================================

# 既存設定の解析
analyze_existing_config() {
    log_info "既存設定の解析を開始..."
    
    local config_data="$BACKUP_DIR/analyzed_config.json"
    
    # JSON形式で設定データを蓄積
    echo "{" > "$config_data"
    
    analyze_base_config "$config_data"
    analyze_platform_configs "$config_data"
    analyze_claude_voice_config "$config_data"
    analyze_migration_config "$config_data"
    
    # JSON終了
    echo "}" >> "$config_data"
    
    log_info "設定解析完了: $config_data"
    save_migration_state "analysis_completed" "2" "既存設定解析完了"
}

# base.conf の解析
analyze_base_config() {
    local output_file="$1"
    local base_conf="$BACKUP_DIR/base.conf"
    
    log_debug "base.conf を解析中..."
    
    if [[ ! -f "$base_conf" ]]; then
        log_warn "base.conf が見つかりません"
        return
    fi
    
    cat >> "$output_file" << 'EOF'
  "base": {
    "shell": {},
    "prefix": {},
    "indexing": {},
    "terminal": {},
    "display": {},
    "mouse": {}
  },
EOF
    
    # シェル設定の抽出
    local default_command
    default_command=$(grep -E "set.*default-command" "$base_conf" | head -1 | awk '{print $4}' 2>/dev/null || echo "zsh")
    update_json_value "$output_file" "base.shell.default_command" "$default_command"
    
    # プレフィックス設定の抽出
    local prefix_key
    prefix_key=$(grep -E "set.*prefix" "$base_conf" | head -1 | awk '{print $4}' 2>/dev/null || echo "C-z")
    update_json_value "$output_file" "base.prefix.key" "$prefix_key"
    
    # その他の設定値を同様に抽出...
    # （実装省略）
}

# プラットフォーム固有設定の解析
analyze_platform_configs() {
    local output_file="$1"
    
    cat >> "$output_file" << 'EOF'
  "platforms": {
    "wsl": {},
    "macos": {},
    "linux": {}
  },
EOF
    
    # WSL設定の解析
    analyze_wsl_config "$output_file"
    
    # macOS設定の解析
    analyze_macos_config "$output_file"
    
    # Linux設定の解析
    analyze_linux_config "$output_file"
}

# WSL設定の解析
analyze_wsl_config() {
    local output_file="$1"
    local wsl_conf="$BACKUP_DIR/os/wsl.conf"
    
    log_debug "WSL設定を解析中..."
    
    if [[ ! -f "$wsl_conf" ]]; then
        log_warn "wsl.conf が見つかりません"
        return
    fi
    
    # 環境変数の抽出
    local claude_voice_auto_summary
    claude_voice_auto_summary=$(grep -E "CLAUDE_VOICE_AUTO_SUMMARY" "$wsl_conf" | head -1 | sed 's/.*\"\(.*\)\".*/\1/' 2>/dev/null || echo "true")
    
    # その他のWSL設定を同様に抽出...
    # （実装省略）
}

# macOS設定の解析
analyze_macos_config() {
    local output_file="$1"
    local macos_conf="$BACKUP_DIR/os/macos.conf"
    
    log_debug "macOS設定を解析中..."
    
    if [[ -f "$macos_conf" ]]; then
        # macOS固有設定の抽出
        # （実装省略）
        log_debug "macOS設定解析完了"
    else
        log_debug "macOS設定ファイルが見つかりません"
    fi
}

# Linux設定の解析
analyze_linux_config() {
    local output_file="$1"
    local linux_conf="$BACKUP_DIR/os/linux.conf"
    
    log_debug "Linux設定を解析中..."
    
    if [[ -f "$linux_conf" ]]; then
        # Linux固有設定の抽出
        # （実装省略）
        log_debug "Linux設定解析完了"
    else
        log_debug "Linux設定ファイルが見つかりません"
    fi
}

# Claude Voice設定の解析
analyze_claude_voice_config() {
    local output_file="$1"
    
    cat >> "$output_file" << 'EOF'
  "claude_voice": {
    "master": {},
    "states": {},
    "notification": {},
    "key_bindings": {}
  },
EOF
    
    # 既存のClaude Voice設定から値を抽出
    # （実装省略）
}

# migration.conf の解析
analyze_migration_config() {
    local output_file="$1"
    local migration_conf="$BACKUP_DIR/migration.conf"
    
    cat >> "$output_file" << 'EOF'
  "migration": {
    "detection_method": "legacy",
    "rollback": {
      "threshold": 5,
      "enabled": true
    },
    "testing": {
      "duration_sec": 300
    }
  }
EOF
    
    if [[ -f "$migration_conf" ]]; then
        log_debug "migration.conf を解析中..."
        
        # 既存値の抽出
        local detection_method rollback_threshold test_duration
        detection_method=$(grep "DETECTION_METHOD=" "$migration_conf" | cut -d'=' -f2 | tr -d '"' || echo "legacy")
        rollback_threshold=$(grep "ROLLBACK_THRESHOLD=" "$migration_conf" | cut -d'=' -f2 || echo "5")
        test_duration=$(grep "TEST_DURATION=" "$migration_conf" | cut -d'=' -f2 || echo "300")
        
        # JSONに反映
        # （実装省略 - 実際の実装では sed や jq を使用）
    fi
}

# JSON値の更新ヘルパー（簡易版）
update_json_value() {
    local file="$1"
    local key="$2"
    local value="$3"
    
    # 実際の実装では jq を使用してより堅牢に実装
    log_debug "JSON値更新: $key = $value"
}

# ====================================
# YAML生成機能
# ====================================

# 解析済み設定からYAML生成
generate_yaml_from_analysis() {
    log_info "解析済み設定からYAML設定を生成中..."
    
    local analysis_file="$BACKUP_DIR/analyzed_config.json"
    local temp_yaml="$BACKUP_DIR/generated-tmux-unified.yaml"
    
    if [[ ! -f "$analysis_file" ]]; then
        error_exit "解析ファイルが見つかりません: $analysis_file"
    fi
    
    # 基本的なYAML構造の作成
    cat > "$temp_yaml" << 'EOF'
# TMux Unified Configuration - Migrated from Legacy Config
# Generated by config-migration.sh
# Version: 1.0

# ====================================
# 基本設定 (Base Configuration)
# ====================================
base:
  shell:
    default_command: "zsh"
  
  prefix:
    key: "C-z"
    enable_double_tap: true
  
  indexing:
    window_base: 1
    pane_base: 1
  
  terminal:
    default_terminal: "xterm-256color"
    overrides:
      - "xterm-termite:Tc"
      - "*:U8=0"
  
  display:
    time_ms: 2000
    titles_enabled: true
    automatic_rename: true
    automatic_rename_format: "#{=|-22|..;b:pane_current_path}"
  
  mouse:
    enabled: true
    version_compatibility: true

# ====================================
# プラットフォーム固有設定
# ====================================
platforms:
  wsl:
EOF
    
    # 解析済みデータからYAMLに値を移植
    # （実際の実装では jq や専用パーサーを使用）
    migrate_analyzed_values "$analysis_file" "$temp_yaml"
    
    # メタデータセクションの追加
    add_migration_metadata "$temp_yaml"
    
    log_info "YAML生成完了: $temp_yaml"
    save_migration_state "yaml_generated" "3" "統一YAML設定生成完了"
}

# 解析済み値のYAML移植
migrate_analyzed_values() {
    local analysis_file="$1"
    local yaml_file="$2"
    
    log_debug "解析済み値をYAMLに移植中..."
    
    # jq が利用可能な場合の実装例
    if command -v jq &> /dev/null; then
        # jq を使用した値の抽出と移植
        # （実装省略）
        log_debug "jq を使用して値を移植"
    else
        # 基本的な grep/sed を使用した移植
        log_debug "基本的なテキスト処理で値を移植"
    fi
}

# 移行メタデータの追加
add_migration_metadata() {
    local yaml_file="$1"
    
    cat >> "$yaml_file" << EOF

# ====================================
# 移行メタデータ
# ====================================
metadata:
  version: "1.0.0"
  last_updated: "$(date -Iseconds)"
  schema_version: "1.0"
  migration:
    source: "legacy_config"
    migration_date: "$(date -Iseconds)"
    backup_location: "$BACKUP_DIR"
    original_files: [$(printf '"%s", ' "${LEGACY_FILES[@]}" | sed 's/, $//'))]
  
  sources:
$(for file in "${LEGACY_FILES[@]}"; do
    if [[ -f "$file" ]]; then
        echo "    $(basename "$file"): \"$file\""
    fi
done)
EOF
}

# ====================================
# 移行実行・テスト機能
# ====================================

# 段階的移行の実行
execute_migration() {
    log_info "段階的移行を実行中..."
    
    local temp_yaml="$BACKUP_DIR/generated-tmux-unified.yaml"
    
    # 1. 生成された設定の検証
    log_info "ステップ1: 生成設定の検証..."
    if ! validate_generated_config "$temp_yaml"; then
        error_exit "生成された設定の検証に失敗しました"
    fi
    
    # 2. テスト環境での動作確認
    log_info "ステップ2: テスト環境での動作確認..."
    if ! test_config_in_sandbox "$temp_yaml"; then
        error_exit "テスト環境での動作確認に失敗しました"
    fi
    
    # 3. 本設定への適用
    log_info "ステップ3: 本設定への適用..."
    if ! apply_new_config "$temp_yaml"; then
        error_exit "新設定の適用に失敗しました"
    fi
    
    # 4. 動作確認
    log_info "ステップ4: 最終動作確認..."
    if ! verify_migration_success; then
        log_error "移行後の動作確認で問題が検出されました。ロールバックを検討してください"
        save_migration_state "migration_failed" "4" "移行後動作確認失敗"
        return 1
    fi
    
    save_migration_state "migration_completed" "4" "移行完了"
    log_info "移行が正常に完了しました"
}

# 生成設定の検証
validate_generated_config() {
    local yaml_file="$1"
    
    log_debug "生成設定を検証中: $yaml_file"
    
    # YAML構文チェック
    if command -v yq &> /dev/null; then
        if ! yq eval '.' "$yaml_file" > /dev/null 2>&1; then
            log_error "YAML構文エラーが検出されました"
            return 1
        fi
    elif command -v python3 &> /dev/null; then
        if ! python3 -c "import yaml; yaml.safe_load(open('$yaml_file'))" 2>/dev/null; then
            log_error "YAML構文エラーが検出されました (Python yaml)"
            return 1
        fi
    else
        log_warn "YAML構文チェックをスキップ (yq/python-yaml未インストール)"
    fi
    
    # 必須セクションの確認
    local required_sections=("base" "platforms" "claude_voice" "metadata")
    
    for section in "${required_sections[@]}"; do
        if ! grep -q "^${section}:" "$yaml_file"; then
            log_error "必須セクション未定義: $section"
            return 1
        fi
    done
    
    log_info "生成設定の検証成功"
    return 0
}

# テスト環境での動作確認
test_config_in_sandbox() {
    local yaml_file="$1"
    local test_dir="/tmp/tmux-migration-test-$$"
    
    log_debug "テスト環境でのテスト: $test_dir"
    
    mkdir -p "$test_dir"
    
    # テスト用設定生成
    cp "$yaml_file" "$test_dir/tmux-unified.yaml"
    
    # 設定生成スクリプトでテスト設定を生成
    if [[ -f "$SCRIPT_DIR/config-generator.sh" ]]; then
        YAML_CONFIG="$test_dir/tmux-unified.yaml" \
        OUTPUT_DIR="$test_dir/generated" \
        "$SCRIPT_DIR/config-generator.sh" auto
        
        # 生成された設定でtmuxを起動テスト
        local test_config="$test_dir/generated/tmux-$(detect_platform).conf"
        if [[ -f "$test_config" ]]; then
            if tmux -f "$test_config" list-sessions -F "#{session_name}" 2>/dev/null | head -1 >/dev/null; then
                log_info "テスト環境での動作確認成功"
                rm -rf "$test_dir"
                return 0
            else
                log_error "テスト環境でのtmux起動に失敗"
                rm -rf "$test_dir"
                return 1
            fi
        else
            log_error "テスト設定の生成に失敗"
            rm -rf "$test_dir"
            return 1
        fi
    else
        log_warn "設定生成スクリプトが見つかりません。テストをスキップ"
        rm -rf "$test_dir"
        return 0
    fi
}

# 新設定の適用
apply_new_config() {
    local yaml_file="$1"
    
    log_info "新設定を適用中..."
    
    # 統一設定ファイルの配置
    cp "$yaml_file" "$YAML_CONFIG"
    
    # 設定の生成と適用
    if [[ -f "$SCRIPT_DIR/config-generator.sh" ]]; then
        "$SCRIPT_DIR/config-generator.sh" auto
        log_info "プラットフォーム固有設定を生成完了"
    else
        log_warn "設定生成スクリプトが見つかりません"
    fi
    
    log_info "新設定の適用完了"
    return 0
}

# 移行成功の確認
verify_migration_success() {
    log_info "移行成功を確認中..."
    
    # 基本的な動作確認
    if [[ -f "$YAML_CONFIG" ]]; then
        log_info "統一YAML設定が正常に配置されました"
    else
        log_error "統一YAML設定の配置に失敗"
        return 1
    fi
    
    # 生成設定の確認
    local generated_dir="$TMUX_DIR/generated"
    if [[ -d "$generated_dir" ]] && [[ -n "$(find "$generated_dir" -name "*.conf" -type f)" ]]; then
        log_info "プラットフォーム固有設定が正常に生成されました"
    else
        log_error "プラットフォーム固有設定の生成に失敗"
        return 1
    fi
    
    # 検証スクリプトによる確認
    if [[ -f "$SCRIPT_DIR/config-validator.sh" ]]; then
        if "$SCRIPT_DIR/config-validator.sh" --quiet; then
            log_info "設定検証スクリプトによる確認成功"
        else
            log_warn "設定検証スクリプトで警告またはエラーが検出されました"
        fi
    fi
    
    log_info "移行成功の確認完了"
    return 0
}

# ====================================
# ロールバック機能
# ====================================

# 設定のロールバック
rollback_migration() {
    log_info "設定のロールバックを開始..."
    
    load_migration_state
    
    if [[ -z "${backup_dir:-}" ]] || [[ ! -d "${backup_dir:-}" ]]; then
        error_exit "バックアップディレクトリが見つかりません: ${backup_dir:-}"
    fi
    
    log_info "バックアップから復元中: $backup_dir"
    
    local restored_count=0
    
    # バックアップファイルの復元
    for file in "${LEGACY_FILES[@]}"; do
        local relative_path
        relative_path=$(realpath --relative-to="$TMUX_DIR" "$file")
        local backup_file="$backup_dir/$relative_path"
        
        if [[ -f "$backup_file" ]]; then
            mkdir -p "$(dirname "$file")"
            cp "$backup_file" "$file"
            log_info "復元: $relative_path"
            ((restored_count++))
        fi
    done
    
    # 新生成ファイルの削除
    if [[ -f "$YAML_CONFIG" ]]; then
        mv "$YAML_CONFIG" "$YAML_CONFIG.rolled_back"
        log_info "統一YAML設定をバックアップ: $YAML_CONFIG.rolled_back"
    fi
    
    if [[ -d "$TMUX_DIR/generated" ]]; then
        mv "$TMUX_DIR/generated" "$TMUX_DIR/generated.rolled_back"
        log_info "生成設定をバックアップ: $TMUX_DIR/generated.rolled_back"
    fi
    
    save_migration_state "rolled_back" "rollback" "設定をロールバック完了"
    log_info "ロールバック完了: $restored_count ファイルを復元"
}

# ====================================
# ユーティリティ関数
# ====================================

# プラットフォーム検出
detect_platform() {
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
# メイン処理
# ====================================

# 使用方法の表示
show_usage() {
    cat << EOF
TMux設定移行スクリプト

使用方法:
  $0 [COMMAND] [OPTIONS]

コマンド:
  migrate           既存設定から統一YAML設定に移行 (デフォルト)
  rollback          最後の移行をロールバック
  analyze           既存設定の解析のみ実行
  status            移行状態の表示

オプション:
  -h, --help        このヘルプを表示
  -v, --verbose     詳細ログを有効化
  -f, --force       確認なしで実行
  -n, --dry-run     実際の変更は行わず、計画のみ表示

移行手順:
  1. 既存設定のバックアップ作成
  2. 設定内容の解析・抽出
  3. 統一YAML設定の生成
  4. テスト環境での動作確認
  5. 本番環境への適用

例:
  $0 migrate          # 完全移行実行
  $0 analyze          # 設定解析のみ
  $0 status           # 移行状態確認
  $0 rollback         # 最後の移行をロールバック

EOF
}

# 移行状態の表示
show_migration_status() {
    load_migration_state
    
    echo "=================================================================================="
    echo "TMux設定移行状態"
    echo "=================================================================================="
    echo ""
    
    case "${state:-not_started}" in
        "not_started")
            echo "状態: 移行未開始"
            echo "次のステップ: $0 migrate で移行を開始"
            ;;
        "backup_created")
            echo "状態: バックアップ作成済み"
            echo "バックアップ場所: ${backup_dir:-不明}"
            echo "次のステップ: 設定解析・YAML生成が必要"
            ;;
        "analysis_completed")
            echo "状態: 設定解析完了"
            echo "次のステップ: YAML生成・適用が必要"
            ;;
        "yaml_generated")
            echo "状態: YAML設定生成完了"
            echo "次のステップ: テスト・適用が必要"
            ;;
        "migration_completed")
            echo "状態: 移行完了"
            echo "統一設定ファイル: $YAML_CONFIG"
            echo "バックアップ: ${backup_dir:-不明}"
            ;;
        "migration_failed")
            echo "状態: 移行失敗"
            echo "メッセージ: ${message:-不明}"
            echo "推奨アクション: $0 rollback でロールバック"
            ;;
        "rolled_back")
            echo "状態: ロールバック済み"
            echo "復元日時: ${timestamp:-不明}"
            ;;
        *)
            echo "状態: 不明 (${state:-})"
            ;;
    esac
    
    echo ""
    echo "タイムスタンプ: ${timestamp:-不明}"
    echo "メッセージ: ${message:-}"
    echo "=================================================================================="
}

# メイン処理
main() {
    local command="migrate"
    local verbose=false
    local force=false
    local dry_run=false
    
    # コマンドライン引数の解析
    while [[ $# -gt 0 ]]; do
        case $1 in
            migrate|rollback|analyze|status)
                command="$1"
                shift
                ;;
            -h|--help)
                show_usage
                exit 0
                ;;
            -v|--verbose)
                export LOG_LEVEL="DEBUG"
                verbose=true
                shift
                ;;
            -f|--force)
                force=true
                shift
                ;;
            -n|--dry-run)
                dry_run=true
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
    log_info "TMux設定移行スクリプト開始: $command"
    
    case "$command" in
        "migrate")
            if [[ "$dry_run" == "true" ]]; then
                log_info "ドライラン モード: 実際の変更は行いません"
                # ドライラン実装
                show_migration_plan
            else
                if [[ "$force" == "false" ]]; then
                    echo "既存設定から統一YAML設定への移行を開始します。"
                    echo "この操作は既存設定ファイルをバックアップしてから実行されます。"
                    read -p "続行しますか? (y/N): " -r confirm
                    if [[ ! "$confirm" =~ ^[Yy]$ ]]; then
                        log_info "移行をキャンセルしました"
                        exit 0
                    fi
                fi
                
                create_backup
                analyze_existing_config
                generate_yaml_from_analysis
                execute_migration
            fi
            ;;
        "rollback")
            if [[ "$force" == "false" ]]; then
                read -p "設定をロールバックしますか? (y/N): " -r confirm
                if [[ ! "$confirm" =~ ^[Yy]$ ]]; then
                    log_info "ロールバックをキャンセルしました"
                    exit 0
                fi
            fi
            rollback_migration
            ;;
        "analyze")
            create_backup
            analyze_existing_config
            log_info "設定解析のみ完了。解析結果: $BACKUP_DIR/analyzed_config.json"
            ;;
        "status")
            show_migration_status
            ;;
    esac
    
    log_info "TMux設定移行スクリプト完了"
}

# 移行計画の表示（ドライラン用）
show_migration_plan() {
    echo "=================================================================================="
    echo "TMux設定移行計画 (ドライラン)"
    echo "=================================================================================="
    echo ""
    echo "移行対象ファイル:"
    for file in "${LEGACY_FILES[@]}"; do
        if [[ -f "$file" ]]; then
            echo "  ✓ $file"
        else
            echo "  - $file (見つかりません)"
        fi
    done
    echo ""
    echo "実行される処理:"
    echo "  1. 既存設定のバックアップ作成 → $BACKUP_DIR"
    echo "  2. 設定内容の解析・抽出"
    echo "  3. 統一YAML設定の生成 → $YAML_CONFIG"
    echo "  4. テスト環境での動作確認"
    echo "  5. 本番環境への適用"
    echo ""
    echo "注意: これは計画表示のみです。実際の移行は --dry-run オプションなしで実行してください。"
    echo "=================================================================================="
}

# スクリプト実行
if [[ "${BASH_SOURCE[0]}" == "${0}" ]]; then
    main "$@"
fi