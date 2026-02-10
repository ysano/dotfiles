## Hooks リファレンス

**配置**: `settings.json` の `hooks` セクション、Skill/Agent フロントマター、Plugin `hooks/hooks.json`
**トリガー**: Claude Code ライフサイクルイベントに自動応答

### イベント一覧（14種）

| イベント | タイミング | ブロック可能 |
|----------|-----------|:---:|
| `SessionStart` | セッション開始・再開 | No |
| `UserPromptSubmit` | ユーザープロンプト送信後、処理前 | Yes |
| `PreToolUse` | ツール実行前 | Yes |
| `PermissionRequest` | 権限ダイアログ表示時 | Yes |
| `PostToolUse` | ツール実行成功後 | No |
| `PostToolUseFailure` | ツール実行失敗後 | No |
| `Notification` | 通知送信時 | No |
| `SubagentStart` | サブエージェント起動時 | No |
| `SubagentStop` | サブエージェント完了時 | Yes |
| `Stop` | メインエージェント応答完了時 | Yes |
| `TeammateIdle` | チームメイトがアイドルになる直前 | Yes |
| `TaskCompleted` | タスク完了マーク時 | Yes |
| `PreCompact` | コンテキスト圧縮前 | No |
| `SessionEnd` | セッション終了時 | No |

### ハンドラー型（3種）

| type | 説明 | 固有フィールド |
|------|------|---------------|
| `command` | シェルコマンド実行 | `command`, `async` |
| `prompt` | LLM 単一ターン評価 | `prompt`, `model` |
| `agent` | ツール使用可能なサブエージェント | `prompt`, `model` |

共通フィールド: `type`(必須), `timeout`, `statusMessage`, `once`(Skill限定)

### settings.json フォーマット

```json
{
  "hooks": {
    "PostToolUse": [
      {
        "matcher": "Write|Edit",
        "hooks": [
          {
            "type": "command",
            "command": "\"$CLAUDE_PROJECT_DIR\"/.claude/hooks/lint.sh",
            "timeout": 10
          }
        ]
      }
    ]
  }
}
```

### Skill/Agent フロントマターでの定義

```yaml
---
name: secure-ops
hooks:
  PreToolUse:
    - matcher: "Bash"
      hooks:
        - type: command
          command: "./scripts/validate.sh"
---
```

Agent の `Stop` フックは自動的に `SubagentStop` に変換される。

### matcher パターン

| イベント | matcher 対象 | 例 |
|----------|-------------|-----|
| `PreToolUse`, `PostToolUse`, `PostToolUseFailure`, `PermissionRequest` | ツール名 | `Bash`, `Edit\|Write`, `mcp__memory__.*` |
| `SessionStart` | 開始方法 | `startup`, `resume`, `clear`, `compact` |
| `SessionEnd` | 終了理由 | `clear`, `logout`, `prompt_input_exit` |
| `Notification` | 通知タイプ | `permission_prompt`, `idle_prompt` |
| `SubagentStart`, `SubagentStop` | エージェント型 | `Explore`, `Plan`, カスタム名 |
| `PreCompact` | トリガー | `manual`, `auto` |
| `UserPromptSubmit`, `Stop`, `TeammateIdle`, `TaskCompleted` | matcher 非対応 | 常に発火 |

### 入出力プロトコル

**入力**: JSON を stdin で受信。共通フィールド:

| フィールド | 説明 |
|-----------|------|
| `session_id` | セッションID |
| `transcript_path` | 会話ログのパス |
| `cwd` | カレントディレクトリ |
| `permission_mode` | 権限モード |
| `hook_event_name` | イベント名 |

ツールイベントは追加で `tool_name`, `tool_input` 等を含む。

**出力（exit code）**:

| Exit Code | 動作 |
|-----------|------|
| `0` | 成功。stdout の JSON を処理 |
| `2` | ブロッキングエラー。stderr をフィードバック |
| その他 | 非ブロッキングエラー。実行継続 |

**出力（JSON stdout / exit 0 時のみ）**:

| フィールド | 説明 |
|-----------|------|
| `continue` | `false` で Claude 全体を停止 |
| `stopReason` | `continue: false` 時のユーザー向けメッセージ |
| `decision` | `"block"` でアクション阻止（PostToolUse, Stop 等） |
| `reason` | decision の説明 |
| `hookSpecificOutput` | イベント固有の制御（`hookEventName` 必須） |

**PreToolUse の決定制御**（hookSpecificOutput 内）:

| フィールド | 説明 |
|-----------|------|
| `permissionDecision` | `"allow"` / `"deny"` / `"ask"` |
| `permissionDecisionReason` | 理由 |
| `updatedInput` | ツール入力の書き換え |

### 設計ガイドライン

- **高速に保つ**: command は 200ms 以下が目標
- **スクリプト分離**: `.claude/hooks/scripts/` に配置、`$CLAUDE_PROJECT_DIR` で参照
- **prompt/agent 型**: 単純な判定 → `prompt`、ファイル検査が必要 → `agent`
- **async**: 長い処理は `"async": true` でバックグラウンド実行（ブロック不可）
