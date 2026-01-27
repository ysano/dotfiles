#!/bin/bash
# カテゴリー別統合スクリプト
# 使用方法: ./integrate_category.sh <category> [--no-backup] [--no-translate]

set -euo pipefail

CATEGORY="${1:-}"
NO_BACKUP=false
NO_TRANSLATE=false

# 引数解析
while [[ $# -gt 0 ]]; do
    case "$1" in
        --no-backup)
            NO_BACKUP=true
            shift
            ;;
        --no-translate)
            NO_TRANSLATE=true
            shift
            ;;
        *)
            if [[ -z "$CATEGORY" ]]; then
                CATEGORY="$1"
            fi
            shift
            ;;
    esac
done

if [[ -z "$CATEGORY" ]]; then
    echo "使用方法: $0 <category> [--no-backup] [--no-translate]"
    echo ""
    echo "カテゴリー: boundary, context, deploy, dev, docs, memory, orchestration,"
    echo "           performance, project, reasoning, rust, security, semantic,"
    echo "           setup, simulation, skills, spec-workflow, svelte, sync,"
    echo "           team, test, wfgy"
    exit 1
fi

SOURCE_DIR="/tmp/Claude-Command-Suite/.claude/commands/$CATEGORY"
TARGET_DIR=".claude/commands/$CATEGORY"
SCRIPT_DIR="$(dirname "$0")"

# ソースディレクトリ確認
if [[ ! -d "$SOURCE_DIR" ]]; then
    echo "❌ エラー: ソースディレクトリが見つかりません: $SOURCE_DIR"
    exit 1
fi

# ファイル数確認
SOURCE_COUNT=$(find "$SOURCE_DIR" -name "*.md" -type f | wc -l | tr -d ' ')
if [[ "$SOURCE_COUNT" -eq 0 ]]; then
    echo "❌ エラー: ソースディレクトリにファイルがありません: $SOURCE_DIR"
    exit 1
fi

echo "=== $CATEGORY カテゴリー統合開始 ==="
echo "ソース: $SOURCE_DIR ($SOURCE_COUNT ファイル)"
echo "ターゲット: $TARGET_DIR"

# 1. バックアップ作成
if [[ "$NO_BACKUP" == false ]] && [[ -d "$TARGET_DIR" ]]; then
    BACKUP_DIR="${TARGET_DIR}.backup.$(date +%Y%m%d_%H%M%S)"
    echo "📦 バックアップ作成中: $BACKUP_DIR"
    cp -r "$TARGET_DIR" "$BACKUP_DIR"
    echo "✓ バックアップ完了"
fi

# 2. ディレクトリ作成
mkdir -p "$TARGET_DIR"

# 3. ファイルコピー
echo "📋 ファイルコピー中..."
cp "$SOURCE_DIR"/*.md "$TARGET_DIR/" 2>/dev/null || true
COPIED_COUNT=$(find "$TARGET_DIR" -name "*.md" -type f | wc -l | tr -d ' ')
echo "✓ コピー完了: $COPIED_COUNT ファイル"

# 4. セクションヘッダー翻訳
if [[ "$NO_TRANSLATE" == false ]]; then
    echo "🌏 セクションヘッダー翻訳中..."
    TRANSLATED=0
    while IFS= read -r file; do
        "$SCRIPT_DIR/translate_command.sh" "$file"
        ((TRANSLATED++))
    done < <(find "$TARGET_DIR" -name "*.md" -type f)
    echo "✓ 翻訳完了: $TRANSLATED ファイル"
fi

# 5. .gitattributes更新
echo "📝 .gitattributes更新中..."
ATTR_LINE=".claude/commands/$CATEGORY/*.md source=Claude-Command-Suite author=qdhenry"

# 既存のエントリを削除
if [[ -f .gitattributes ]]; then
    grep -v "^.claude/commands/$CATEGORY/" .gitattributes > .gitattributes.tmp || true
    mv .gitattributes.tmp .gitattributes
fi

# 新しいエントリを追加
echo "$ATTR_LINE" >> .gitattributes
echo "✓ .gitattributes更新完了"

# 6. 検証
echo "🔍 検証中..."
"$SCRIPT_DIR/validate_integration.sh" "$CATEGORY"

echo ""
echo "🎉 $CATEGORY カテゴリー統合完了!"
echo "   ファイル数: $COPIED_COUNT"
echo "   バックアップ: ${BACKUP_DIR:-なし}"
