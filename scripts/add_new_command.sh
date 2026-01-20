#!/bin/bash
# 新規コマンド追加支援スクリプト
# 使用方法: ./scripts/add_new_command.sh <category> <command_name> <source_type> [author]

set -e

if [ "$#" -lt 3 ]; then
    echo "使用方法: $0 <category> <command_name> <source_type> [author]"
    echo ""
    echo "引数:"
    echo "  category:     deploy, dev, docs, performance, project, security, setup, team, test"
    echo "  command_name: ファイル名（.md拡張子なし）"
    echo "  source_type:  Claude-Command-Suite, Original, External"
    echo "  author:       作者名（省略時は自動設定）"
    echo ""
    echo "例:"
    echo "  $0 dev new-debug-tool Original"
    echo "  $0 team slack-integration External \"SlackTeam\""
    exit 1
fi

CATEGORY="$1"
COMMAND_NAME="$2"
SOURCE_TYPE="$3"
AUTHOR="${4:-Local}"

# カテゴリ検証
VALID_CATEGORIES="boundary context deploy dev docs memory orchestration performance project reasoning rust security semantic setup simulation skills spec-workflow svelte sync team test wfgy"
if ! echo "$VALID_CATEGORIES" | grep -w "$CATEGORY" > /dev/null; then
    echo "❌ 無効なカテゴリ: $CATEGORY"
    echo "有効なカテゴリ: $VALID_CATEGORIES"
    exit 1
fi

# ファイルパス
FILE_PATH=".claude/commands/$CATEGORY/$COMMAND_NAME.md"

# ファイル存在チェック
if [ -f "$FILE_PATH" ]; then
    echo "❌ ファイルが既に存在します: $FILE_PATH"
    exit 1
fi

# ディレクトリ作成
mkdir -p ".claude/commands/$CATEGORY"

# テンプレートファイル作成
cat > "$FILE_PATH" << EOF
# $COMMAND_NAME

[コマンドの説明をここに記載]

## 実行手順

1. **[ステップ1]**
   - [詳細説明]

2. **[ステップ2]**
   - [詳細説明]

## 注意点

- [注意事項があれば記載]

## 関連コマンド

- [\`/category:related-command\`] - [関連コマンドの説明]
EOF

echo "✅ ファイル作成完了: $FILE_PATH"

# .gitattributes更新
GITATTR_LINE="$FILE_PATH source=$SOURCE_TYPE author=$AUTHOR"

# Claude-Command-Suite由来の場合は特別セクションに追加
if [ "$SOURCE_TYPE" = "Claude-Command-Suite" ]; then
    # 特別セクションに追加
    if ! grep -q "^$GITATTR_LINE$" .gitattributes; then
        sed -i "/# Claude-Command-Suite由来ファイル（上書き）/a\\$GITATTR_LINE" .gitattributes
        echo "✅ .gitattributes更新: Claude-Command-Suite セクションに追加"
    fi
else
    # デフォルト設定で対応されるのでメッセージのみ
    echo "✅ 出典設定: $SOURCE_TYPE (デフォルト設定により自動適用)"
fi

# 設定確認
echo ""
echo "📋 設定確認:"
git check-attr -a "$FILE_PATH"

echo ""
echo "🎉 新規コマンド追加完了!"
echo "   ファイル: $FILE_PATH"
echo "   カテゴリ: $CATEGORY"
echo "   出典: $SOURCE_TYPE"
echo "   作者: $AUTHOR"
echo ""
echo "📝 次のステップ:"
echo "   1. ファイル内容を編集: $FILE_PATH"
echo "   2. Git追加: git add $FILE_PATH"
echo "   3. コミット: git commit -m \"Add $COMMAND_NAME command\""