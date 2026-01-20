#!/bin/bash
# Claude-Command-Suite英語コマンドを日本語化するスクリプト
# 使用方法:
#   ./translate_command.sh <input_file> [--full-translate]
#
# オプション:
#   --full-translate    Ollamaを使用して本文も翻訳（デフォルト: セクションヘッダーのみ）

set -euo pipefail

INPUT_FILE="${1:-}"
FULL_TRANSLATE=false
DICT_FILE="$(dirname "$0")/dictionary.json"

# 引数チェック
if [[ -z "$INPUT_FILE" ]]; then
    echo "使用方法: $0 <input_file> [--full-translate]"
    exit 1
fi

if [[ ! -f "$INPUT_FILE" ]]; then
    echo "エラー: ファイルが見つかりません: $INPUT_FILE"
    exit 1
fi

if [[ ! -f "$DICT_FILE" ]]; then
    echo "エラー: 辞書ファイルが見つかりません: $DICT_FILE"
    exit 1
fi

# オプション解析
if [[ "${2:-}" == "--full-translate" ]]; then
    FULL_TRANSLATE=true
fi

# 一時ファイル作成
TEMP_FILE=$(mktemp)
trap "rm -f $TEMP_FILE" EXIT

# セクションヘッダー翻訳（辞書ベース）
translate_headers() {
    local file="$1"
    local output="$2"

    cp "$file" "$output"

    # JSONから翻訳ペアを取得して置換
    while IFS= read -r line; do
        if [[ "$line" =~ ^[[:space:]]*\"(.+)\":[[:space:]]*\"(.+)\" ]]; then
            local en="${BASH_REMATCH[1]}"
            local ja="${BASH_REMATCH[2]}"
            # セクションヘッダーのみ置換（行頭の##で始まる行）
            sed -i.bak "s|^${en}$|${ja}|g" "$output"
            rm -f "${output}.bak"
        fi
    done < <(grep -o '"[^"]*": "[^"]*"' "$DICT_FILE")
}

# Ollamaを使った完全翻訳
translate_full() {
    local file="$1"
    local output="$2"

    if ! command -v ollama &> /dev/null; then
        echo "警告: Ollamaが見つかりません。セクションヘッダーのみ翻訳します。"
        translate_headers "$file" "$output"
        return
    fi

    echo "Ollamaで完全翻訳中... (数分かかる場合があります)"

    # Ollamaを使って翻訳
    local prompt="以下の技術文書を日本語に翻訳してください。以下のルールに従ってください：
1. YAMLフロントマター（---で囲まれた部分）は翻訳しないでください
2. コードブロック（\`\`\`で囲まれた部分）は翻訳しないでください
3. URL、ファイルパス、コマンド名は翻訳しないでください
4. 技術用語は適切に日本語化してください（例: Architecture→アーキテクチャ）
5. マークダウンの構造を保持してください

翻訳する文書:
$(cat "$file")"

    ollama run gemma3:4b "$prompt" > "$output" 2>/dev/null || {
        echo "警告: Ollama翻訳に失敗しました。セクションヘッダーのみ翻訳します。"
        translate_headers "$file" "$output"
    }
}

# 翻訳実行
if [[ "$FULL_TRANSLATE" == true ]]; then
    translate_full "$INPUT_FILE" "$TEMP_FILE"
else
    translate_headers "$INPUT_FILE" "$TEMP_FILE"
fi

# 元のファイルを置き換え
mv "$TEMP_FILE" "$INPUT_FILE"

echo "✓ 翻訳完了: $INPUT_FILE"
