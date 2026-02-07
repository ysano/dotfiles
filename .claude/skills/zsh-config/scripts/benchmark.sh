#!/usr/bin/env bash
# =============================================================================
# Zsh 起動時間ベンチマークスクリプト
# Usage: benchmark.sh [--threshold MS] [--runs N]
#   benchmark.sh                    デフォルト: 5回実行、閾値500ms
#   benchmark.sh --threshold 300    閾値300msで判定
#   benchmark.sh --runs 10          10回実行
# Output: PASS/WARN/FAIL with average startup time
# =============================================================================
set -euo pipefail

# --- Defaults ---
THRESHOLD_MS=500
WARN_THRESHOLD_MS=300
RUNS=5

# --- Parse args ---
while [[ $# -gt 0 ]]; do
    case "$1" in
        --threshold) THRESHOLD_MS="$2"; shift 2 ;;
        --runs)      RUNS="$2"; shift 2 ;;
        *)           echo "Unknown option: $1" >&2; exit 1 ;;
    esac
done

# WARN threshold = 60% of FAIL threshold
WARN_THRESHOLD_MS=$(( THRESHOLD_MS * 60 / 100 ))

# --- Colors ---
if [[ -t 1 ]]; then
    RED='\033[0;31m'; GREEN='\033[0;32m'; YELLOW='\033[0;33m'; BOLD='\033[1m'; NC='\033[0m'
else
    RED=''; GREEN=''; YELLOW=''; BOLD=''; NC=''
fi

echo -e "${BOLD}=== Zsh Startup Benchmark ===${NC}"
echo "Runs: $RUNS | WARN threshold: ${WARN_THRESHOLD_MS}ms | FAIL threshold: ${THRESHOLD_MS}ms"
echo ""

# --- Check zsh availability ---
if ! command -v zsh &>/dev/null; then
    echo -e "[${RED}FAIL${NC}] zsh not found"
    exit 1
fi

# =============================================================================
# Benchmark: zsh -i -c exit
# =============================================================================
times_ms=()
for i in $(seq 1 "$RUNS"); do
    # /usr/bin/time は GNU/BSD で出力形式が異なるため、bash TIMEFORMAT を使用
    start_ns=$(date +%s%N 2>/dev/null || echo 0)

    if [[ "$start_ns" == "0" ]]; then
        # date +%N が使えない環境（macOS標準）→ python fallback
        if command -v python3 &>/dev/null; then
            start_s=$(python3 -c 'import time; print(time.time())')
            zsh -i -c exit 2>/dev/null
            end_s=$(python3 -c 'import time; print(time.time())')
            elapsed_ms=$(python3 -c "print(int(($end_s - $start_s) * 1000))")
        else
            # 最終フォールバック: bash の SECONDS（精度1秒）
            SECONDS=0
            zsh -i -c exit 2>/dev/null
            elapsed_ms=$(( SECONDS * 1000 ))
        fi
    else
        zsh -i -c exit 2>/dev/null
        end_ns=$(date +%s%N)
        elapsed_ms=$(( (end_ns - start_ns) / 1000000 ))
    fi

    times_ms+=("$elapsed_ms")
    echo "  Run $i: ${elapsed_ms}ms"
done

echo ""

# =============================================================================
# Calculate statistics
# =============================================================================
total=0
min=${times_ms[0]}
max=${times_ms[0]}

for t in "${times_ms[@]}"; do
    total=$(( total + t ))
    (( t < min )) && min=$t
    (( t > max )) && max=$t
done

avg=$(( total / RUNS ))

echo -e "${BOLD}Results:${NC}"
echo "  Average: ${avg}ms"
echo "  Min:     ${min}ms"
echo "  Max:     ${max}ms"
echo ""

# =============================================================================
# Judgment
# =============================================================================
if [[ $avg -lt $WARN_THRESHOLD_MS ]]; then
    echo -e "[${GREEN}PASS${NC}] Startup time ${avg}ms is well within threshold (< ${WARN_THRESHOLD_MS}ms)"
elif [[ $avg -lt $THRESHOLD_MS ]]; then
    echo -e "[${YELLOW}WARN${NC}] Startup time ${avg}ms is approaching threshold (${WARN_THRESHOLD_MS}-${THRESHOLD_MS}ms)"
else
    echo -e "[${RED}FAIL${NC}] Startup time ${avg}ms exceeds threshold (> ${THRESHOLD_MS}ms)"
fi

# =============================================================================
# Zinit times (if available)
# =============================================================================
echo ""
echo -e "${BOLD}--- Zinit Plugin Load Times (top 10) ---${NC}"
if command -v zsh &>/dev/null; then
    # zinit times の出力を取得（タイムアウト付き）
    zinit_output=$(timeout 10 zsh -i -c 'zinit times 2>/dev/null | head -12' 2>/dev/null || echo "(zinit times unavailable)")
    echo "$zinit_output"
fi

echo ""
echo -e "${BOLD}=== Benchmark Complete ===${NC}"

[[ $avg -ge $THRESHOLD_MS ]] && exit 1 || exit 0
