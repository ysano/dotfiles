#!/bin/bash
# ファイル名: hooks/setup-hooks.sh
# 説明: ~/.claude/settings.json に Claude Voice hooks 設定をマージする
# 用途: 初回セットアップ時に実行。既存設定を保持しつつ hooks を追加する。

set -euo pipefail

SETTINGS_FILE="${HOME}/.claude/settings.json"
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
STATUS_UPDATE_SCRIPT="${HOME}/.tmux/claude/hooks/status-update.sh"

echo "=== Claude Voice hooks セットアップ ==="

# jq の存在確認
if ! command -v jq >/dev/null 2>&1; then
    echo "エラー: jq がインストールされていません"
    echo "  sudo apt install jq  # Ubuntu/Debian"
    echo "  brew install jq       # macOS"
    exit 1
fi

# status-update.sh の存在確認
if [[ ! -x "$SCRIPT_DIR/status-update.sh" ]]; then
    echo "エラー: status-update.sh が見つからないか実行権限がありません"
    echo "  パス: $SCRIPT_DIR/status-update.sh"
    exit 1
fi

# settings.json ディレクトリの存在確認
mkdir -p "$(dirname "$SETTINGS_FILE")"

# 既存の settings.json を読み込み（なければ空オブジェクト）
if [[ -f "$SETTINGS_FILE" ]]; then
    EXISTING=$(cat "$SETTINGS_FILE")
    # JSON として有効か検証
    if ! echo "$EXISTING" | jq . >/dev/null 2>&1; then
        echo "エラー: 既存の settings.json が不正な JSON です"
        exit 1
    fi
else
    EXISTING='{}'
fi

# バックアップ作成
if [[ -f "$SETTINGS_FILE" ]]; then
    BACKUP="${SETTINGS_FILE}.backup.$(date +%Y%m%d_%H%M%S)"
    cp "$SETTINGS_FILE" "$BACKUP"
    echo "バックアップ作成: $BACKUP"
fi

# hooks 設定を定義
# $HOME を展開した実際のパスを使用
HOOKS_CONFIG=$(cat <<ENDJSON
{
  "hooks": {
    "UserPromptSubmit": [
      {
        "hooks": [
          {
            "type": "command",
            "command": "${STATUS_UPDATE_SCRIPT}",
            "timeout": 5
          }
        ]
      }
    ],
    "PreToolUse": [
      {
        "hooks": [
          {
            "type": "command",
            "command": "${STATUS_UPDATE_SCRIPT}",
            "timeout": 5
          }
        ]
      }
    ],
    "Notification": [
      {
        "matcher": "idle_prompt|permission_prompt",
        "hooks": [
          {
            "type": "command",
            "command": "${STATUS_UPDATE_SCRIPT}",
            "timeout": 5
          }
        ]
      }
    ],
    "Stop": [
      {
        "hooks": [
          {
            "type": "command",
            "command": "${STATUS_UPDATE_SCRIPT}",
            "timeout": 5
          }
        ]
      }
    ],
    "SessionStart": [
      {
        "hooks": [
          {
            "type": "command",
            "command": "${STATUS_UPDATE_SCRIPT}",
            "timeout": 5
          }
        ]
      }
    ],
    "SessionEnd": [
      {
        "hooks": [
          {
            "type": "command",
            "command": "${STATUS_UPDATE_SCRIPT}",
            "timeout": 5
          }
        ]
      }
    ]
  }
}
ENDJSON
)

# 既存設定と hooks 設定をマージ
# hooks キーが既存の場合は各イベントごとにマージ（既存の hooks エントリを保持）
MERGED=$(echo "$EXISTING" | jq --argjson hooks_config "$HOOKS_CONFIG" '
  # 既存の hooks がある場合はイベントごとにマージ
  if .hooks then
    .hooks as $existing_hooks |
    $hooks_config.hooks as $new_hooks |
    reduce ($new_hooks | keys[]) as $event (
      .;
      if .hooks[$event] then
        # 既存のイベントに status-update.sh のエントリがなければ追加
        .hooks[$event] as $existing_entries |
        $new_hooks[$event] as $new_entries |
        if ($existing_entries | map(select(.hooks[]?.command | test("status-update\\.sh$"))) | length) > 0 then
          .  # 既に存在する場合はスキップ
        else
          .hooks[$event] = $existing_entries + $new_entries
        end
      else
        .hooks[$event] = $new_hooks[$event]
      end
    )
  else
    . * $hooks_config
  end
')

# 結果を検証
if ! echo "$MERGED" | jq . >/dev/null 2>&1; then
    echo "エラー: マージ結果が不正な JSON です"
    exit 1
fi

# settings.json に書き込み
echo "$MERGED" | jq . > "$SETTINGS_FILE"

echo ""
echo "hooks 設定をマージしました: $SETTINGS_FILE"
echo ""
echo "設定されたイベント:"
echo "  UserPromptSubmit  → ⚡ Busy"
echo "  PreToolUse        → ⚡ Busy（タイムスタンプ更新）"
echo "  Notification      → ⌛ Waiting（idle_prompt|permission_prompt）"
echo "  Stop              → ✅ Idle"
echo "  SessionStart      → ✅ Idle"
echo "  SessionEnd        → （クリア）"
echo ""
echo "Claude Code を再起動すると hooks が有効になります。"
