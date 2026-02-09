---
description: "Convert natural language project rules into Claude Code hook configurations"
---

## Instructions

Convert rules into Claude Code hooks: **$ARGUMENTS**

引数がない場合は `./CLAUDE.md`, `./CLAUDE.local.md`, `~/.claude/CLAUDE.md` からルールを読み取る。

### Hook Events

| Event | Timing | Use Case |
|-------|--------|----------|
| **PreToolUse** | ツール実行前 | 検証、ブロック、セキュリティチェック |
| **PostToolUse** | ツール実行後 | フォーマット、Lint、テスト実行 |
| **Stop** | セッション終了時 | ステータス確認、クリーンアップ |

### Matcher

- Exact: `"Edit"` / Multiple: `"Edit|MultiEdit|Write"` / Regex: `".*Edit"`
- 省略時: 全ツールに適用

### Conversion Process

各ルールについて:
1. キーワードから適切な Event を判定（"before"→Pre, "after"→Post, "when done"→Stop）
2. 対象ツールの matcher を決定
3. 実行コマンドを生成

### Output Format

```json
{
  "hooks": {
    "PostToolUse": [{
      "matcher": "Edit|MultiEdit|Write",
      "hooks": [{"type": "command", "command": "your-command-here"}]
    }]
  }
}
```

### Examples

**"Format Python files after editing"** → PostToolUse + `"Edit|MultiEdit|Write"` + `black . --quiet 2>/dev/null || true`

**"Check for secrets before saving"** → PreToolUse + `"Write|Edit|MultiEdit"` + `git secrets --scan 2>/dev/null || echo 'OK'`

### Output

- 既存の `~/.claude/hooks.json` とマージ（上書きしない）
- 生成した設定のサマリーを報告
