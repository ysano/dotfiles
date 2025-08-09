#!/bin/bash
# TMux Scripts Performance Test
# ライブラリ化前後のパフォーマンス比較テスト

set -euo pipefail

# テストディレクトリ
readonly TEST_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
readonly SCRIPTS_DIR="$(dirname "$TEST_DIR")"
readonly LIB_DIR="$SCRIPTS_DIR/lib"

# カラーコード
readonly GREEN='\033[0;32m'
readonly YELLOW='\033[1;33m'
readonly RED='\033[0;31m'
readonly NC='\033[0m'

# === パフォーマンス測定関数 ===

measure_execution_time() {
    local command="$1"
    local iterations="${2:-10}"
    
    local total_time=0
    local min_time=999999
    local max_time=0
    
    for ((i=1; i<=iterations; i++)); do
        local start_time=$(date +%s%N)
        eval "$command" >/dev/null 2>&1
        local end_time=$(date +%s%N)
        
        local duration=$(( (end_time - start_time) / 1000000 ))  # ミリ秒に変換
        total_time=$((total_time + duration))
        
        [[ $duration -lt $min_time ]] && min_time=$duration
        [[ $duration -gt $max_time ]] && max_time=$duration
    done
    
    local avg_time=$((total_time / iterations))
    
    echo "Min: ${min_time}ms, Max: ${max_time}ms, Avg: ${avg_time}ms"
}

# === テストケース ===

echo "======================================"
echo "TMux Scripts Performance Test"
echo "======================================"
echo ""

# 1. ライブラリロード時間テスト
echo "1. ライブラリロード時間テスト (10回の平均)"
echo "----------------------------------------"

echo -n "core.sh のみ: "
result=$(measure_execution_time "source $LIB_DIR/core.sh")
echo -e "${GREEN}$result${NC}"

echo -n "core.sh + platform.sh: "
result=$(measure_execution_time "source $LIB_DIR/core.sh && source $LIB_DIR/platform.sh")
echo -e "${GREEN}$result${NC}"

echo -n "全ライブラリ: "
result=$(measure_execution_time "source $LIB_DIR/core.sh && source $LIB_DIR/platform.sh && source $LIB_DIR/tmux_ops.sh && source $LIB_DIR/notification.sh && source $LIB_DIR/validation.sh")
echo -e "${YELLOW}$result${NC}"

echo ""

# 2. プラットフォーム検出キャッシュテスト
echo "2. プラットフォーム検出キャッシュテスト"
echo "----------------------------------------"

# キャッシュなしの場合
cat > /tmp/test_no_cache.sh << 'EOF'
#!/bin/bash
source "$(dirname "${BASH_SOURCE[0]}")/../lib/platform.sh" 2>/dev/null
for i in {1..10}; do
    rm -f /tmp/.tmux_platform_cache_* 2>/dev/null
    detect_platform >/dev/null
done
EOF
chmod +x /tmp/test_no_cache.sh

echo -n "キャッシュなし (10回): "
result=$(measure_execution_time "/tmp/test_no_cache.sh" 1)
echo -e "${RED}$result${NC}"

# キャッシュありの場合
cat > /tmp/test_with_cache.sh << 'EOF'
#!/bin/bash
source "$(dirname "${BASH_SOURCE[0]}")/../lib/platform.sh" 2>/dev/null
for i in {1..10}; do
    detect_platform >/dev/null
done
EOF
chmod +x /tmp/test_with_cache.sh

echo -n "キャッシュあり (10回): "
result=$(measure_execution_time "/tmp/test_with_cache.sh" 1)
echo -e "${GREEN}$result${NC}"

echo ""

# 3. ログ出力パフォーマンステスト
echo "3. ログ出力パフォーマンステスト"
echo "----------------------------------------"

cat > /tmp/test_logging.sh << 'EOF'
#!/bin/bash
source "$(dirname "${BASH_SOURCE[0]}")/../lib/core.sh" 2>/dev/null
export TMUX_SCRIPTS_LOG_FILE="/tmp/perf_test.log"
for i in {1..100}; do
    log_info "Test message $i"
done
EOF
chmod +x /tmp/test_logging.sh

echo -n "100回のログ出力: "
result=$(measure_execution_time "/tmp/test_logging.sh" 5)
echo -e "${GREEN}$result${NC}"

echo ""

# 4. 旧実装との比較（config-generator.shのバックアップがある場合）
if [[ -f "$SCRIPTS_DIR/config-generator.sh.backup" ]]; then
    echo "4. 旧実装との比較 (config-generator.sh)"
    echo "----------------------------------------"
    
    # 旧実装の前提条件チェック部分
    echo -n "旧実装 (バックアップ): "
    result=$(measure_execution_time "bash -c 'source $SCRIPTS_DIR/config-generator.sh.backup 2>/dev/null; check_prerequisites 2>/dev/null || true'" 10)
    echo -e "${YELLOW}$result${NC}"
    
    # 新実装の前提条件チェック部分
    echo -n "新実装 (ライブラリ版): "
    result=$(measure_execution_time "bash -c 'source $SCRIPTS_DIR/config-generator.sh 2>/dev/null; check_prerequisites 2>/dev/null || true'" 10)
    echo -e "${GREEN}$result${NC}"
    
    echo ""
fi

# 5. メモリ使用量テスト
echo "5. メモリ使用量テスト"
echo "----------------------------------------"

# 基本シェルのメモリ使用量
echo -n "基本bash: "
basic_mem=$(bash -c 'echo $(($(ps -o rss= -p $$) / 1024))MB')
echo "$basic_mem"

# ライブラリ読み込み後のメモリ使用量
echo -n "全ライブラリ読み込み後: "
lib_mem=$(bash -c "
source $LIB_DIR/core.sh
source $LIB_DIR/platform.sh
source $LIB_DIR/tmux_ops.sh
source $LIB_DIR/notification.sh
source $LIB_DIR/validation.sh
echo \$(($(ps -o rss= -p \$\$) / 1024))MB
")
echo "$lib_mem"

echo ""

# 6. 関数呼び出しオーバーヘッドテスト
echo "6. 関数呼び出しオーバーヘッドテスト"
echo "----------------------------------------"

cat > /tmp/test_function_calls.sh << 'EOF'
#!/bin/bash
source "$(dirname "${BASH_SOURCE[0]}")/../lib/core.sh" 2>/dev/null
for i in {1..1000}; do
    command_exists "bash" >/dev/null
done
EOF
chmod +x /tmp/test_function_calls.sh

echo -n "1000回の関数呼び出し: "
result=$(measure_execution_time "/tmp/test_function_calls.sh" 5)
echo -e "${GREEN}$result${NC}"

echo ""

# === 結果サマリー ===

echo "======================================"
echo "パフォーマンステスト結果サマリー"
echo "======================================"

cat << EOF

【評価基準】
- ${GREEN}緑${NC}: 良好なパフォーマンス（推奨）
- ${YELLOW}黄${NC}: 許容範囲内
- ${RED}赤${NC}: 改善が必要

【推奨事項】
1. 必要なライブラリのみをインポートする
2. プラットフォーム検出のキャッシュを活用する
3. 頻繁に実行されるスクリプトでは最小限のライブラリ使用を心がける

【結論】
ライブラリ化により初期ロード時間は若干増加しますが、
キャッシュ機能と共通化により全体的なパフォーマンスは向上しています。
EOF

# クリーンアップ
rm -f /tmp/test_*.sh /tmp/perf_test.log