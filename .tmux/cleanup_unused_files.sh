#!/bin/bash
# tmux未使用ファイル削除スクリプト（マルチOS対応版）
# 生成日: 2025-08-09
# 注意: OS固有ファイルとプラットフォーム固有ファイルは保持されます

set -e

# カラー定義
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# ディレクトリ設定
TMUX_DIR="$(cd "$(dirname "$0")" && pwd)"
BACKUP_DIR="${TMUX_DIR}/backups"
TIMESTAMP=$(date +%Y%m%d_%H%M%S)
BACKUP_FILE="${BACKUP_DIR}/tmux_backup_${TIMESTAMP}.tar.gz"

echo -e "${BLUE}=== tmux未使用ファイル削除スクリプト（マルチOS対応版） ===${NC}"
echo ""

# バックアップディレクトリ作成
mkdir -p "$BACKUP_DIR"

# ステップ1: バックアップ作成
echo -e "${YELLOW}ステップ1: バックアップを作成中...${NC}"
# statusディレクトリとキャッシュを除外してバックアップ作成
tar -czf "$BACKUP_FILE" -C "$(dirname "$TMUX_DIR")" \
    --exclude='.tmux/status' \
    --exclude='.tmux/cache' \
    --exclude='.tmux/backups' \
    --exclude='.tmux/*.log' \
    ".tmux" 2>/dev/null
echo -e "${GREEN}✓ バックアップを作成しました: $BACKUP_FILE${NC}"
echo ""

# ステップ2: 削除対象ファイルの確認
echo -e "${YELLOW}ステップ2: 削除対象ファイルを確認中...${NC}"

# 削除対象ファイルリスト（マルチOS対応: OS固有ファイルは除外）
declare -a FILES_TO_DELETE=(
    # テストファイル（開発環境以外では不要）
    "tests/minimal-test.sh"
    "tests/phase2-test-suite.sh"
    "tests/test_tmux_config_v2.sh"
    "tests/test-3state-detection.sh"
    "tests/test-runner-enhanced.sh"
    "tests/unit/debug_test.sh"
    "tests/unit/test-dependency-injection.sh"
    "tests/unit/test-enhanced-detection.sh"
    "tests/wsl-integration-test.sh"
    
    # Claude関連テスト
    "claude/tests/test_config_manager.sh"
    "claude/tests/test_debug.sh"
    "claude/tests/test_edge_cases_config.sh"
    "claude/tests/test_end_to_end.sh"
    "claude/tests/test_execution_engine.sh"
    "claude/tests/test_fast.sh"
    "claude/tests/test_frame_detection.sh"
    "claude/tests/test_health_diagnostics.sh"
    "claude/tests/test_minimal.sh"
    "claude/tests/test_module_integration.sh"
    "claude/tests/test_panning.sh"
    "claude/tests/test_runner.sh"
    "claude/tests/test_simple.sh"
    "claude/tests/test_stats_monitor.sh"
    "claude/tests/test_user_interface.sh"
    "claude/core/tests/test_modular_voice_engine.sh"
    
    # 未使用スクリプト（参照されていない）
    "scripts/claude-status-debug.sh"
    "scripts/claude-status-display.sh"
    "scripts/claude-status-enhanced.sh"
    "scripts/claude-status-precision.sh"
    "scripts/claude-status-smart.sh"
    "scripts/claude-status-unified.sh"
    "scripts/claude-voice-wrapper.sh"
    "scripts/platform-isolation-validator.sh"
    "scripts/test-status-bar-performance.sh"
    "scripts/tests/performance_test.sh"
    "scripts/tests/test_lib.sh"
    "scripts/config-validator.sh"
    
    # Claude関連未使用
    "claude/scripts/check_naming_conventions.sh"
    "claude/scripts/test_wsl_integration.sh"
    
    # 古いコアスクリプト（古いパスを参照）
    "core/audio-fallback.sh"
    "core/audio-mode-switcher.sh"
    "core/init-minimal.sh"
    "core/ollama-cross.sh"
    "core/voice-unified.sh"
    "core/shared/config-manager.sh"
    "core/shared/logging.sh"
    "core/shared/platform-utils.sh"
    "core/tmux-keybindings.conf"
    
    # Claude未使用モジュール（どこからも参照されていない）
    "claude/adapters/tmux_adapter.sh"
    "claude/core/common_functions.sh"
    "claude/core/error_handler.sh"
    "claude/core/execution_engine.sh"
    "claude/core/health_diagnostics.sh"
    "claude/core/integration.sh"
    "claude/core/platform_utils.sh"
    "claude/core/summary_engine.sh"
    "claude/core/user_interface.sh"
    
    # その他（参照されていない）
    "ci.sh"
    "claude/services/detection_service.sh"
    
    # 注意: 以下のファイルはマルチOS対応のため削除しません:
    # - claude/os/linux.sh (Linux対応)
    # - claude/platforms/windows/* (Windows対応)
    # - claude/platforms/wsl/* (WSL対応)
    # - generated/tmux-wsl.conf (WSL設定)
    # - scripts/wsl-setup.sh (WSLセットアップ)
)

# 削除対象ファイル数をカウント
TOTAL_FILES=0
DELETED_FILES=0

for file in "${FILES_TO_DELETE[@]}"; do
    FULL_PATH="${TMUX_DIR}/${file}"
    if [ -f "$FULL_PATH" ]; then
        ((TOTAL_FILES++))
    fi
done

echo -e "削除対象: ${YELLOW}${TOTAL_FILES}${NC} ファイル"
echo ""

# ユーザー確認
echo -e "${RED}警告: これらのファイルを削除しますか？${NC}"
echo -e "バックアップは作成済みです: $BACKUP_FILE"
read -p "続行しますか？ (y/N): " -n 1 -r
echo ""

if [[ ! $REPLY =~ ^[Yy]$ ]]; then
    echo -e "${YELLOW}キャンセルしました${NC}"
    exit 0
fi

# ステップ3: ファイル削除
echo ""
echo -e "${YELLOW}ステップ3: ファイルを削除中...${NC}"

for file in "${FILES_TO_DELETE[@]}"; do
    FULL_PATH="${TMUX_DIR}/${file}"
    if [ -f "$FULL_PATH" ]; then
        rm -f "$FULL_PATH"
        ((DELETED_FILES++))
        echo -e "  ${RED}削除:${NC} $file"
    fi
done

# 空のディレクトリを削除
echo ""
echo -e "${YELLOW}ステップ4: 空のディレクトリを削除中...${NC}"
find "$TMUX_DIR" -type d -empty -delete 2>/dev/null || true

# 結果表示
echo ""
echo -e "${GREEN}=== 削除完了 ===${NC}"
echo -e "削除したファイル数: ${GREEN}${DELETED_FILES}${NC}"
echo -e "バックアップ: ${BLUE}${BACKUP_FILE}${NC}"
echo ""
echo -e "${YELLOW}注意事項:${NC}"
echo "- tmux設定を再読み込みしてください: tmux source-file ~/.tmux.conf"
echo "- 問題が発生した場合は、バックアップから復元できます:"
echo "  tar -xzf $BACKUP_FILE -C $(dirname "$TMUX_DIR")"
echo ""

# ステップ5: 削除後の状態確認
echo -e "${YELLOW}ステップ5: 削除後の状態を確認中...${NC}"
REMAINING_FILES=$(find "$TMUX_DIR" -type f \( -name "*.sh" -o -name "*.conf" \) | wc -l)
echo -e "残存ファイル数: ${GREEN}${REMAINING_FILES}${NC}"

# オプション: 削除結果をログに保存
LOG_FILE="${BACKUP_DIR}/cleanup_log_${TIMESTAMP}.txt"
{
    echo "削除実行日時: $(date)"
    echo "削除ファイル数: ${DELETED_FILES}"
    echo "バックアップファイル: ${BACKUP_FILE}"
    echo "マルチOS対応: はい（OS固有ファイルは保持）"
    echo ""
    echo "削除したファイル:"
    for file in "${FILES_TO_DELETE[@]}"; do
        FULL_PATH="${TMUX_DIR}/${file}"
        if [ ! -f "$FULL_PATH" ]; then
            echo "  - $file"
        fi
    done
    echo ""
    echo "保持したOS固有ファイル:"
    echo "  - os/wsl.conf, os/linux.conf, os/darwin.conf, os/freebsd.conf"
    echo "  - claude/os/darwin.sh, claude/os/windows.sh, claude/os/linux.sh"
    echo "  - claude/platforms/windows/* (Windows対応)"
    echo "  - claude/platforms/wsl/* (WSL対応)"
} > "$LOG_FILE"

echo -e "削除ログ: ${BLUE}${LOG_FILE}${NC}"
echo ""
echo -e "${GREEN}✓ クリーンアップが完了しました！${NC}"
echo -e "${BLUE}注: マルチOS対応のため、OS固有ファイルはすべて保持されています。${NC}"