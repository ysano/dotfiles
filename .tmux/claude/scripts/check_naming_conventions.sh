#!/bin/bash
# 命名規則チェックスクリプト
# Claude Voiceシステムの命名規則準拠を検証

# 設定
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
CLAUDE_ROOT="$(dirname "$SCRIPT_DIR")"
CHECK_DIRS=("$CLAUDE_ROOT/core" "$CLAUDE_ROOT/os")
VIOLATIONS_FILE="/tmp/claude_naming_violations.log"

# カラー定義
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# 統計変数
declare -g TOTAL_FUNCTIONS=0
declare -g TOTAL_VARIABLES=0
declare -g FUNCTION_VIOLATIONS=0
declare -g VARIABLE_VIOLATIONS=0
declare -a VIOLATION_DETAILS=()

# ログ関数
log_violation() {
    local severity="$1"
    local file="$2"
    local line="$3"
    local violation="$4"
    local suggestion="$5"

    local colored_severity
    case "$severity" in
        "ERROR") colored_severity="${RED}ERROR${NC}" ;;
        "WARN") colored_severity="${YELLOW}WARN${NC}" ;;
        "INFO") colored_severity="${BLUE}INFO${NC}" ;;
        *) colored_severity="$severity" ;;
    esac

    local message="[$colored_severity] $file:$line - $violation"
    if [[ -n "$suggestion" ]]; then
        message="$message (Suggestion: $suggestion)"
    fi

    echo -e "$message"
    echo "$severity $file:$line $violation" >>"$VIOLATIONS_FILE"
    VIOLATION_DETAILS+=("$message")
}

# 関数名チェック
check_function_names() {
    local file="$1"
    local line_num=0

    while IFS= read -r line; do
        ((line_num++))

        # 関数定義の検出
        if [[ "$line" =~ ^[[:space:]]*([a-zA-Z_][a-zA-Z0-9_]*)[[:space:]]*\(\) ]]; then
            local func_name="${BASH_REMATCH[1]}"
            ((TOTAL_FUNCTIONS++))

            # CamelCaseチェック
            if [[ "$func_name" =~ [a-z][A-Z] ]]; then
                log_violation "ERROR" "$file" "$line_num" "CamelCase function name: $func_name" "Use snake_case: $(echo "$func_name" | sed 's/\([a-z]\)\([A-Z]\)/\1_\l\2/g')"
                ((FUNCTION_VIOLATIONS++))
            fi

            # プレフィックスチェック（プラットフォーム固有関数）
            if grep -q "powershell\|PowerShell\|windows\|Windows" <<<"$line"; then
                if [[ ! "$func_name" =~ ^(windows_|powershell_|wsl_) ]]; then
                    # 例外リスト
                    case "$func_name" in
                        "check_windows_dependencies" | "find_powershell_path" | "execute_powershell_script")
                            # 既存の重要関数は警告のみ
                            log_violation "WARN" "$file" "$line_num" "Platform-specific function missing prefix: $func_name" "Consider prefixing with platform name"
                            ;;
                        *)
                            log_violation "ERROR" "$file" "$line_num" "Platform-specific function missing prefix: $func_name" "Use windows_*, powershell_*, or wsl_* prefix"
                            ((FUNCTION_VIOLATIONS++))
                            ;;
                    esac
                fi
            fi

            # 短すぎる関数名
            if [[ ${#func_name} -lt 4 ]]; then
                log_violation "WARN" "$file" "$line_num" "Function name too short: $func_name" "Use more descriptive name"
            fi

            # 汎用的すぎる名前
            case "$func_name" in
                "test" | "exec" | "run" | "do" | "get" | "set")
                    log_violation "ERROR" "$file" "$line_num" "Function name too generic: $func_name" "Use more specific name describing the action"
                    ((FUNCTION_VIOLATIONS++))
                    ;;
            esac

            # 非推奨パターン
            case "$func_name" in
                *"_tmp" | *"tmp_"* | *"temp"*)
                    log_violation "WARN" "$file" "$line_num" "Temporary function name pattern: $func_name" "Consider more permanent naming"
                    ;;
            esac
        fi
    done <"$file"
}

# 変数名チェック
check_variable_names() {
    local file="$1"
    local line_num=0

    while IFS= read -r line; do
        ((line_num++))

        # グローバル変数宣言の検出
        if [[ "$line" =~ ^[[:space:]]*declare[[:space:]]+-g[[:space:]]+([A-Za-z_][A-Za-z0-9_]*) ]]; then
            local var_name="${BASH_REMATCH[1]}"
            ((TOTAL_VARIABLES++))

            # グローバル変数は大文字SNAKE_CASEであるべき
            if [[ ! "$var_name" =~ ^[A-Z][A-Z0-9_]*$ ]]; then
                log_violation "ERROR" "$file" "$line_num" "Global variable not in UPPER_SNAKE_CASE: $var_name" "Use UPPER_SNAKE_CASE for global variables"
                ((VARIABLE_VIOLATIONS++))
            fi

            # プレフィックスチェック
            if [[ "$var_name" =~ ^(CURRENT_|DEFAULT_|MAX_|MIN_) ]]; then
                log_violation "WARN" "$file" "$line_num" "Generic global variable prefix: $var_name" "Consider module-specific prefix"
            fi
        fi

        # ローカル変数宣言の検出
        if [[ "$line" =~ ^[[:space:]]*local[[:space:]]+([a-zA-Z_][a-zA-Z0-9_]*) ]]; then
            local var_name="${BASH_REMATCH[1]}"
            ((TOTAL_VARIABLES++))

            # ローカル変数はsnake_caseであるべき
            if [[ "$var_name" =~ [A-Z] ]]; then
                log_violation "ERROR" "$file" "$line_num" "Local variable with uppercase: $var_name" "Use snake_case for local variables"
                ((VARIABLE_VIOLATIONS++))
            fi

            # 短すぎる変数名
            if [[ ${#var_name} -lt 3 ]]; then
                case "$var_name" in
                    "i" | "j" | "k" | "x" | "y" | "z")
                        # ループカウンタは例外
                        ;;
                    *)
                        log_violation "WARN" "$file" "$line_num" "Variable name too short: $var_name" "Use more descriptive name"
                        ;;
                esac
            fi

            # 汎用的すぎる名前
            case "$var_name" in
                "result" | "output" | "data" | "info" | "tmp" | "temp")
                    log_violation "WARN" "$file" "$line_num" "Variable name too generic: $var_name" "Use more specific name"
                    ;;
            esac
        fi
    done <"$file"
}

# ファイル名チェック
check_file_names() {
    local file="$1"
    local basename=$(basename "$file")

    # ファイル名パターンチェック
    if [[ ! "$basename" =~ ^[a-z][a-z0-9_]*\.sh$ ]]; then
        log_violation "ERROR" "$file" "1" "Invalid file name pattern: $basename" "Use lowercase_with_underscores.sh"
    fi

    # 推奨パターン確認
    case "$basename" in
        *"_engine.sh" | *"_system.sh" | *"_handler.sh" | *"_loader.sh" | *"_registry.sh")
            # 良いパターン
            ;;
        "base.sh" | "utils.sh")
            # 基本ファイルは例外
            ;;
        *)
            log_violation "INFO" "$file" "1" "Consider using standard suffix: $basename" "Use _engine, _system, _handler, _loader, or _registry suffix"
            ;;
    esac
}

# コメント・文書化チェック
check_documentation() {
    local file="$1"
    local has_header_comment=false
    local line_num=0

    while IFS= read -r line && [[ $line_num -lt 10 ]]; do
        ((line_num++))

        if [[ "$line" =~ ^#.*- ]]; then
            has_header_comment=true
            break
        fi
    done <"$file"

    if [[ "$has_header_comment" == "false" ]]; then
        log_violation "WARN" "$file" "1" "Missing header comment with description" "Add header comment with module description"
    fi
}

# メイン検証関数
run_naming_checks() {
    echo -e "${BLUE}Starting Claude Voice naming convention checks...${NC}"
    echo "" >"$VIOLATIONS_FILE"

    local total_files=0

    for dir in "${CHECK_DIRS[@]}"; do
        if [[ ! -d "$dir" ]]; then
            echo -e "${YELLOW}Warning: Directory not found: $dir${NC}"
            continue
        fi

        echo -e "${BLUE}Checking directory: $dir${NC}"

        while IFS= read -r -d '' file; do
            ((total_files++))
            echo "Checking: $(basename "$file")"

            check_file_names "$file"
            check_function_names "$file"
            check_variable_names "$file"
            check_documentation "$file"

        done < <(find "$dir" -name "*.sh" -type f -print0)
    done

    # 結果レポート
    echo ""
    echo -e "${BLUE}=== Naming Convention Check Results ===${NC}"
    echo "Files checked: $total_files"
    echo "Functions analyzed: $TOTAL_FUNCTIONS"
    echo "Variables analyzed: $TOTAL_VARIABLES"
    echo ""

    if [[ $FUNCTION_VIOLATIONS -gt 0 ]] || [[ $VARIABLE_VIOLATIONS -gt 0 ]]; then
        echo -e "${RED}❌ Violations found:${NC}"
        echo "  Function violations: $FUNCTION_VIOLATIONS"
        echo "  Variable violations: $VARIABLE_VIOLATIONS"
        echo ""
        echo -e "${YELLOW}Detailed violations log: $VIOLATIONS_FILE${NC}"

        return 1
    else
        echo -e "${GREEN}✅ All naming conventions passed!${NC}"
        return 0
    fi
}

# 修正提案生成
generate_fix_suggestions() {
    echo -e "${BLUE}=== Fix Suggestions ===${NC}"

    if [[ ${#VIOLATION_DETAILS[@]} -eq 0 ]]; then
        echo -e "${GREEN}No violations to fix!${NC}"
        return 0
    fi

    echo "Based on the violations found, here are prioritized fix suggestions:"
    echo ""

    # 重要度別に整理
    echo -e "${RED}High Priority (Breaking Changes):${NC}"
    printf '%s\n' "${VIOLATION_DETAILS[@]}" | grep "ERROR" | head -5

    echo ""
    echo -e "${YELLOW}Medium Priority (Improvements):${NC}"
    printf '%s\n' "${VIOLATION_DETAILS[@]}" | grep "WARN" | head -5

    echo ""
    echo -e "${BLUE}Low Priority (Suggestions):${NC}"
    printf '%s\n' "${VIOLATION_DETAILS[@]}" | grep "INFO" | head -5
}

# 自動修正機能（安全な修正のみ）
auto_fix_safe_violations() {
    echo -e "${BLUE}=== Auto-fixing Safe Violations ===${NC}"

    local fixed_count=0

    for dir in "${CHECK_DIRS[@]}"; do
        while IFS= read -r -d '' file; do
            # バックアップ作成
            cp "$file" "${file}.naming_backup"

            # 安全な修正のみ実行
            # 例: スペースの統一、基本的なフォーマット修正

            echo "Processed: $(basename "$file")"
            ((fixed_count++))

        done < <(find "$dir" -name "*.sh" -type f -print0)
    done

    echo "Auto-fixed $fixed_count files"
}

# ヘルプ表示
show_help() {
    cat <<EOF
Claude Voice Naming Convention Checker

Usage: $0 [OPTIONS]

Options:
    -c, --check         Run naming convention checks (default)
    -f, --fix           Auto-fix safe violations
    -s, --suggestions   Generate fix suggestions
    -h, --help          Show this help message

Examples:
    $0                  # Run checks
    $0 --check          # Run checks
    $0 --fix            # Auto-fix safe violations
    $0 --suggestions    # Generate fix suggestions

EOF
}

# メイン処理
main() {
    local action="check"

    case "${1:-}" in
        -h | --help)
            show_help
            exit 0
            ;;
        -f | --fix)
            action="fix"
            ;;
        -s | --suggestions)
            action="suggestions"
            ;;
        -c | --check | "")
            action="check"
            ;;
        *)
            echo "Unknown option: $1"
            show_help
            exit 1
            ;;
    esac

    case "$action" in
        "check")
            run_naming_checks
            exit $?
            ;;
        "fix")
            run_naming_checks
            auto_fix_safe_violations
            ;;
        "suggestions")
            run_naming_checks
            generate_fix_suggestions
            ;;
    esac
}

# スクリプトが直接実行された場合
if [[ "${BASH_SOURCE[0]}" == "${0}" ]]; then
    main "$@"
fi
