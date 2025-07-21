#!/bin/bash
# Claude Commands出典確認スクリプト
# 使用方法: ./scripts/check_command_sources.sh [source_type]

set -e

COMMANDS_DIR=".claude/commands"

# 引数チェック
if [ "$#" -gt 1 ]; then
    echo "使用方法: $0 [source_type]"
    echo "source_type: Claude-Command-Suite, Original, Documentation (省略時は全て表示)"
    exit 1
fi

SOURCE_FILTER="$1"

echo "=== Claude Commands 出典確認 ==="
echo "検索ディレクトリ: $COMMANDS_DIR"
echo "フィルター: ${SOURCE_FILTER:-全て}"
echo "=========================="

# 出典別統計
echo -e "\n📊 出典別統計:"
declare -A source_counts

# 全ファイルの出典を確認
find "$COMMANDS_DIR" -name "*.md" | while read file; do
    source=$(git check-attr source "$file" | cut -d: -f3 | xargs)
    echo "$source"
done | sort | uniq -c | while read count source; do
    printf "  %-20s: %d ファイル\n" "$source" "$count"
done

echo -e "\n📋 詳細リスト:"

# 詳細リスト表示
find "$COMMANDS_DIR" -name "*.md" | sort | while read file; do
    source=$(git check-attr source "$file" | cut -d: -f3 | xargs)
    author=$(git check-attr author "$file" | cut -d: -f3 | xargs)
    
    # フィルター適用
    if [ -n "$SOURCE_FILTER" ] && [ "$source" != "$SOURCE_FILTER" ]; then
        continue
    fi
    
    # ファイル名を短縮
    short_file=$(echo "$file" | sed 's|.claude/commands/||')
    
    printf "  %-35s | %-20s | %s\n" "$short_file" "$source" "$author"
done

echo -e "\n🔍 使用例:"
echo "  全ファイル:                ./scripts/check_command_sources.sh"
echo "  Claude-Command-Suite のみ: ./scripts/check_command_sources.sh Claude-Command-Suite"
echo "  オリジナルのみ:            ./scripts/check_command_sources.sh Original"

echo -e "\n💡 個別確認:"
echo "  git check-attr source author .claude/commands/dev/git-status.md"