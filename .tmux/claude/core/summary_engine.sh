#!/bin/bash
# Claude Voice Core - Summary Engine Module
# 要約生成のメインエンジン（OS非依存）

# 要約エンジンのメイン処理
generate_summary() {
    local input_text="$1"
    local summary_type="${2:-brief}"
    local model="${3:-auto}"
    local options="${4:-}"

    local start_time=$(start_timer)

    log "INFO" "Starting summary generation (type: $summary_type, model: $model)"

    # 入力検証
    if [[ -z "$input_text" ]]; then
        log "ERROR" "No input text provided"
        echo "要約する内容がありません。"
        return 1
    fi

    # コンテキスト情報の収集
    local context=$(collect_context_information)

    # 入力テキストの前処理
    local processed_text=$(preprocess_input_text "$input_text")

    # 要約品質の事前評価
    local input_quality=$(evaluate_input_quality "$processed_text")
    log "DEBUG" "Input quality score: $input_quality"

    # 要約の生成
    local summary=""
    local generation_method=""

    if [[ $input_quality -lt 30 ]]; then
        log "WARN" "Low input quality, using enhanced simple summary"
        summary=$(generate_enhanced_simple_summary "$processed_text" "$summary_type" "$context")
        generation_method="enhanced_simple"
    else
        # LLM による要約を試行
        summary=$(generate_llm_summary "$processed_text" "$summary_type" "$model" "$context")
        generation_method="llm"
    fi

    # 要約の後処理
    local final_summary=$(postprocess_summary "$summary" "$summary_type")

    # 品質チェック
    local output_quality=$(evaluate_summary_quality "$final_summary" "$summary_type")
    log "DEBUG" "Output quality score: $output_quality"

    # 統計記録
    local duration=$(end_timer "$start_time")
    record_summary_stats "$summary_type" "$model" "$generation_method" "$duration" "$input_quality" "$output_quality"

    log "INFO" "Summary generation completed (${duration}s, method: $generation_method)"
    echo "$final_summary"
    return 0
}

# コンテキスト情報の収集
collect_context_information() {
    local context_parts=()

    # 現在のディレクトリ
    local current_dir=$(basename "$PWD" 2>/dev/null)
    if [[ -n "$current_dir" ]]; then
        context_parts+=("dir:$current_dir")
    fi

    # Git情報
    if git rev-parse --is-inside-work-tree >/dev/null 2>&1; then
        local git_branch=$(git branch --show-current 2>/dev/null)
        if [[ -n "$git_branch" ]]; then
            context_parts+=("git:$git_branch")
        fi

        local git_status=$(git status --porcelain 2>/dev/null | wc -l | tr -d ' ')
        if [[ $git_status -gt 0 ]]; then
            context_parts+=("changes:$git_status")
        fi
    fi

    # プロジェクトタイプの推定
    local project_type=$(detect_project_type)
    if [[ -n "$project_type" ]]; then
        context_parts+=("type:$project_type")
    fi

    # tmuxセッション情報
    if [[ -n "$TMUX" ]]; then
        local session_name=$(tmux display-message -p "#{session_name}" 2>/dev/null)
        if [[ -n "$session_name" ]]; then
            context_parts+=("session:$session_name")
        fi
    fi

    # コンテキスト情報の結合
    local IFS=","
    echo "${context_parts[*]}"
}

# プロジェクトタイプの検出
detect_project_type() {
    # ファイル存在による判定
    if [[ -f "package.json" ]]; then
        echo "nodejs"
    elif [[ -f "requirements.txt" ]] || [[ -f "setup.py" ]] || [[ -f "pyproject.toml" ]]; then
        echo "python"
    elif [[ -f "Cargo.toml" ]]; then
        echo "rust"
    elif [[ -f "go.mod" ]]; then
        echo "go"
    elif [[ -f "Dockerfile" ]]; then
        echo "docker"
    elif [[ -f "docker-compose.yml" ]] || [[ -f "docker-compose.yaml" ]]; then
        echo "docker-compose"
    elif [[ -f "Makefile" ]]; then
        echo "make"
    elif [[ -f ".github/workflows"* ]]; then
        echo "ci-cd"
    elif git rev-parse --is-inside-work-tree >/dev/null 2>&1; then
        echo "git"
    else
        echo ""
    fi
}

# 入力テキストの前処理
preprocess_input_text() {
    local input_text="$1"

    # 重複行の除去
    local deduplicated=$(echo "$input_text" | awk '!seen[$0]++')

    # 意味のない行の除去
    local filtered=$(echo "$deduplicated" |
        grep -v '^[[:space:]]*$' |
        grep -v '^[-=_]\+$' |
        grep -v '^[[:space:]]*[│┌┐└┘├┤┬┴┼]\+[[:space:]]*$')

    # 長すぎる行の短縮
    local truncated=$(echo "$filtered" |
        sed 's/^\(.\{150\}\).*/\1.../')

    echo "$truncated"
}

# 要約の後処理
postprocess_summary() {
    local summary="$1"
    local summary_type="$2"

    # 基本的な整形
    local cleaned=$(echo "$summary" |
        sed 's/^[[:space:]]\+//; s/[[:space:]]\+$//' |
        sed 's/[[:space:]]\+/ /g')

    # 長さの調整
    local max_length
    case "$summary_type" in
        "brief")
            max_length=200
            ;;
        "detailed")
            max_length=500
            ;;
        "technical")
            max_length=400
            ;;
        *)
            max_length=300
            ;;
    esac

    if [[ ${#cleaned} -gt $max_length ]]; then
        cleaned="${cleaned:0:$max_length}..."
    fi

    # 敬語の統一（簡易）
    cleaned=$(echo "$cleaned" |
        sed 's/である。/です。/g' |
        sed 's/する。/します。/g' |
        sed 's/した。/しました。/g')

    echo "$cleaned"
}

# 入力品質の評価
evaluate_input_quality() {
    local input_text="$1"
    local quality_score=50

    # 文字数チェック
    local char_count=${#input_text}
    if [[ $char_count -gt 100 ]]; then
        quality_score=$((quality_score + 20))
    elif [[ $char_count -lt 50 ]]; then
        quality_score=$((quality_score - 20))
    fi

    # 構造化テキストの検出
    if echo "$input_text" | grep -q "⏺\|✅\|❌\|📁\|🔧"; then
        quality_score=$((quality_score + 15))
    fi

    # エラー情報の検出
    if echo "$input_text" | grep -qi "error\|warning\|exception"; then
        quality_score=$((quality_score + 10))
    fi

    # コマンド実行結果の検出
    if echo "$input_text" | grep -q "exit code\|failed\|success"; then
        quality_score=$((quality_score + 10))
    fi

    # 意味のないテキストの検出（改行文字を除去）
    local meaningless_ratio=$(echo "$input_text" | grep -c '^[[:space:]]*$\|^[-=_]\+$' || echo 0)
    local total_lines=$(echo "$input_text" | wc -l)
    meaningless_ratio=$(echo "$meaningless_ratio" | tr -d '\n\r')
    total_lines=$(echo "$total_lines" | tr -d '\n\r')
    local half_lines=0
    if [[ $total_lines -gt 0 ]] 2>/dev/null; then
        half_lines=$((total_lines / 2))
        if [[ $meaningless_ratio -gt $half_lines ]] 2>/dev/null; then
            quality_score=$((quality_score - 25))
        fi
    fi

    # 0-100の範囲に正規化（改行文字を除去）
    quality_score=$(echo "$quality_score" | tr -d '\n\r')
    if [[ $quality_score -lt 0 ]] 2>/dev/null; then
        quality_score=0
    elif [[ $quality_score -gt 100 ]] 2>/dev/null; then
        quality_score=100
    fi

    # 数値でない場合はデフォルト値
    if ! [[ "$quality_score" =~ ^[0-9]+$ ]]; then
        quality_score=50
    fi

    echo "$quality_score"
}

# 要約品質の評価
evaluate_summary_quality() {
    local summary="$1"
    local summary_type="$2"
    local quality_score=50

    # 基本的な要件チェック
    local char_count=${#summary}
    local expected_length

    case "$summary_type" in
        "brief")
            expected_length=100
            ;;
        "detailed")
            expected_length=300
            ;;
        "technical")
            expected_length=250
            ;;
        *)
            expected_length=200
            ;;
    esac

    # 長さの適切性
    local length_ratio=$((char_count * 100 / expected_length))
    if [[ $length_ratio -ge 50 ]] && [[ $length_ratio -le 150 ]]; then
        quality_score=$((quality_score + 20))
    elif [[ $length_ratio -lt 25 ]] || [[ $length_ratio -gt 200 ]]; then
        quality_score=$((quality_score - 20))
    fi

    # 日本語の自然性チェック
    if echo "$summary" | grep -q "です\|ます\|である"; then
        quality_score=$((quality_score + 10))
    fi

    # 技術的内容の適切性
    if [[ "$summary_type" == "technical" ]]; then
        if echo "$summary" | grep -qi "コマンド\|エラー\|実行\|処理"; then
            quality_score=$((quality_score + 15))
        fi
    fi

    # 情報の具体性
    if echo "$summary" | grep -q "[0-9]\+件\|[0-9]\+個\|[0-9]\+行"; then
        quality_score=$((quality_score + 10))
    fi

    # 冗長性のチェック
    local word_count=$(echo "$summary" | wc -w)
    local unique_words=$(echo "$summary" | tr ' ' '\n' | sort | uniq | wc -l)
    if [[ $word_count -gt 0 ]]; then
        local diversity_ratio=$((unique_words * 100 / word_count))
        if [[ $diversity_ratio -gt 70 ]]; then
            quality_score=$((quality_score + 10))
        elif [[ $diversity_ratio -lt 50 ]]; then
            quality_score=$((quality_score - 10))
        fi
    fi

    # 0-100の範囲に正規化（改行文字を除去）
    quality_score=$(echo "$quality_score" | tr -d '\n\r')
    if [[ $quality_score -lt 0 ]] 2>/dev/null; then
        quality_score=0
    elif [[ $quality_score -gt 100 ]] 2>/dev/null; then
        quality_score=100
    fi

    # 数値でない場合はデフォルト値
    if ! [[ "$quality_score" =~ ^[0-9]+$ ]]; then
        quality_score=50
    fi

    echo "$quality_score"
}

# 強化された簡易要約
generate_enhanced_simple_summary() {
    local input_text="$1"
    local summary_type="$2"
    local context="$3"

    log "DEBUG" "Generating enhanced simple summary"

    # 高度なキーワード分析
    local error_patterns=("error" "エラー" "failed" "failure" "exception" "abort" "crash")
    local success_patterns=("success" "成功" "completed" "done" "finished" "ok" "passed")
    local warning_patterns=("warning" "警告" "warn" "caution" "注意")
    local progress_patterns=("running" "実行中" "processing" "処理中" "building" "ビルド中")

    local error_count=0
    local success_count=0
    local warning_count=0
    local progress_count=0

    # パターンマッチング
    for pattern in "${error_patterns[@]}"; do
        error_count=$((error_count + $(echo "$input_text" | grep -ci "$pattern")))
    done

    for pattern in "${success_patterns[@]}"; do
        success_count=$((success_count + $(echo "$input_text" | grep -ci "$pattern")))
    done

    for pattern in "${warning_patterns[@]}"; do
        warning_count=$((warning_count + $(echo "$input_text" | grep -ci "$pattern")))
    done

    for pattern in "${progress_patterns[@]}"; do
        progress_count=$((progress_count + $(echo "$input_text" | grep -ci "$pattern")))
    done

    # コンテキストベースの要約構築
    local summary=""

    # コンテキスト情報の追加
    if [[ -n "$context" ]]; then
        local context_desc=$(format_context_description "$context")
        summary+="$context_desc "
    fi

    # 状態の判定と説明
    if [[ $error_count -gt 0 ]]; then
        summary+="${error_count}件のエラーが発生しています。"
        if [[ $error_count -gt 3 ]]; then
            summary+="重大な問題の可能性があります。"
        fi
    elif [[ $warning_count -gt 0 ]]; then
        summary+="${warning_count}件の警告があります。"
    elif [[ $success_count -gt 0 ]]; then
        summary+="${success_count}件の操作が成功しました。"
    elif [[ $progress_count -gt 0 ]]; then
        summary+="現在${progress_count}件の処理が実行中です。"
    else
        summary+="システムは正常に動作しています。"
    fi

    # 詳細レベルに応じた追加情報
    if [[ "$summary_type" == "detailed" ]] || [[ "$summary_type" == "technical" ]]; then
        # 最近のコマンドの抽出
        local recent_commands=$(echo "$input_text" | grep -E "⏺ Bash|⏺ Update|⏺ Write" | tail -3)
        if [[ -n "$recent_commands" ]]; then
            local command_count=$(echo "$recent_commands" | wc -l)
            summary+=" 最近${command_count}件のコマンドが実行されました。"
        fi

        # 技術的詳細の追加
        if [[ "$summary_type" == "technical" ]]; then
            local tech_info=$(extract_technical_information "$input_text")
            if [[ -n "$tech_info" ]]; then
                summary+=" $tech_info"
            fi
        fi
    fi

    echo "$summary"
}

# コンテキスト説明のフォーマット
format_context_description() {
    local context="$1"
    local parts=(${context//,/ })
    local description=""

    for part in "${parts[@]}"; do
        local key=$(echo "$part" | cut -d':' -f1)
        local value=$(echo "$part" | cut -d':' -f2-)

        case "$key" in
            "dir")
                description+="${value}ディレクトリで"
                ;;
            "git")
                description+="${value}ブランチにて"
                ;;
            "type")
                description+="${value}プロジェクトの"
                ;;
            "session")
                description+="${value}セッションで"
                ;;
        esac
    done

    echo "$description"
}

# 技術的情報の抽出
extract_technical_information() {
    local input_text="$1"
    local tech_info=""

    # ビルド/テスト関連
    if echo "$input_text" | grep -qi "build\|compile\|test\|npm\|yarn"; then
        tech_info+="ビルド・テスト関連の作業が検出されました。"
    fi

    # Git関連
    if echo "$input_text" | grep -qi "git \|commit\|push\|pull\|merge"; then
        tech_info+="Git操作が実行されました。"
    fi

    # 開発サーバー関連
    if echo "$input_text" | grep -qi "server\|localhost\|port\|running"; then
        tech_info+="開発サーバーが動作中です。"
    fi

    echo "$tech_info"
}

# 統計記録
record_summary_stats() {
    local summary_type="$1"
    local model="$2"
    local method="$3"
    local duration="$4"
    local input_quality="$5"
    local output_quality="$6"

    local timestamp=$(get_timestamp)
    local stats_entry=$(
        cat <<EOF
{
    "timestamp": $timestamp,
    "operation": "summary_generation",
    "summary_type": "$summary_type",
    "model": "$model",
    "method": "$method",
    "duration": $duration,
    "input_quality": $input_quality,
    "output_quality": $output_quality,
    "success": true
}
EOF
    )

    # 統計ファイルに記録
    mkdir -p "$CLAUDE_VOICE_HOME/logs"
    echo "$stats_entry" >>"$CLAUDE_VOICE_HOME/logs/summary_stats.jsonl"
}

# このモジュールのテスト関数
test_summary_engine() {
    echo "Testing summary engine module..."

    # テスト用のサンプルテキスト
    local test_text="⏺ Bash(npm test) ✅ 15 tests passed ❌ 2 tests failed Error: Module not found"

    # 各タイプの要約生成テスト
    echo "Testing brief summary:"
    local brief_summary=$(generate_summary "$test_text" "brief" "simple")
    echo "Brief: $brief_summary"

    echo "Testing detailed summary:"
    local detailed_summary=$(generate_summary "$test_text" "detailed" "simple")
    echo "Detailed: $detailed_summary"

    echo "Testing technical summary:"
    local technical_summary=$(generate_summary "$test_text" "technical" "simple")
    echo "Technical: $technical_summary"

    # 品質評価のテスト
    local input_quality=$(evaluate_input_quality "$test_text")
    echo "Input quality: $input_quality"

    local output_quality=$(evaluate_summary_quality "$brief_summary" "brief")
    echo "Output quality: $output_quality"

    echo "Summary engine test completed"
}

# このスクリプトが直接実行された場合のテスト
if [[ "${BASH_SOURCE[0]}" == "${0}" ]]; then
    # 基本モジュールの読み込み
    SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
    source "$SCRIPT_DIR/base.sh"
    source "$SCRIPT_DIR/llm_manager.sh"

    claude_voice_init true
    test_summary_engine
fi
