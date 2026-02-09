## Hooks リファレンス

**配置**: `settings.json` の `hooks` セクション、または `.claude/hooks/` にスクリプト
**トリガー**: Claude Code ライフサイクルイベントに自動応答

### イベント種別

| イベント | タイミング | 用途 |
|----------|-----------|------|
| `PreToolUse` | ツール実行前 | 入力検証、危険操作ブロック |
| `PostToolUse` | ツール実行後 | フォーマット、リント、テスト |
| `SessionStart` | セッション開始時 | 環境チェック、コンテキスト設定 |
| `Stop` | エージェント停止時 | 完了度チェック、クリーンアップ |

### settings.json フォーマット

```json
{
  "hooks": {
    "PostToolUse": [
      {
        "matcher": "Write|Edit|MultiEdit",
        "hooks": [
          {
            "type": "command",
            "command": ".claude/hooks/scripts/format-and-lint.sh",
            "timeout": 10
          }
        ]
      }
    ],
    "PreToolUse": [
      {
        "matcher": "Bash",
        "hooks": [
          {
            "type": "command",
            "command": ".claude/hooks/scripts/validate-command.sh",
            "timeout": 5
          }
        ]
      }
    ]
  }
}
```

### matcher パターン

| パターン | 意味 |
|----------|------|
| `"Write"` | Write ツールのみ |
| `"Write\|Edit"` | Write または Edit |
| `"Write\|Edit\|MultiEdit"` | 複数ツールを OR 結合 |

### Hook オブジェクト

| フィールド | 型 | 説明 |
|-----------|------|------|
| `type` | `"command"` | 現在は `command` のみ |
| `command` | string | 実行するシェルコマンド |
| `timeout` | number | タイムアウト秒数 |

### 環境変数

Hook スクリプトには以下の環境変数が渡される:

| 変数 | 説明 |
|------|------|
| `$CLAUDE_PROJECT_DIR` | プロジェクトルートパス |
| `$TOOL_NAME` | 実行中のツール名 |
| `$TOOL_INPUT` | ツールの入力（JSON） |

### 設計ガイドライン

- **高速に保つ**: 200ms 以下が目標。重い処理は `PostToolUse` で非ブロッキング実行
- **失敗を安全に**: `|| true` で非致命的エラーを吸収
- **スクリプト分離**: インラインコマンドより `.claude/hooks/scripts/` にスクリプトを配置
- **ファイル型フィルタ**: `[[ "$FILE_PATH" =~ \.ts$ ]]` で対象を限定
