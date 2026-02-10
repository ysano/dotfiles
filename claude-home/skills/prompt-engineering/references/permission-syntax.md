## Permission Syntax リファレンス

`settings.json` および `settings.local.json` の `permissions` セクションで使用する構文。

### 基本構造

```json
{
  "permissions": {
    "allow": [
      "ToolName(pattern)",
      "ToolName"
    ],
    "deny": [
      "ToolName(pattern)"
    ]
  }
}
```

### 構文パターン

| 構文 | 意味 | 例 |
|------|------|-----|
| `"ToolName"` | ツール全体を許可 | `"Read"` |
| `"ToolName(*)"` | 全引数を許可 | `"Bash(*)"` |
| `"ToolName(prefix:*)"` | プレフィックス一致 | `"Bash(git commit:*)"` |
| `"ToolName(exact)"` | 完全一致 | `"Bash(npm test)"` |
| `"WebFetch(domain:host)"` | ドメイン制限 | `"WebFetch(domain:github.com)"` |

### Bash パーミッション例

```json
{
  "permissions": {
    "allow": [
      "Bash(git commit:*)",
      "Bash(git add:*)",
      "Bash(npm test)",
      "Bash(ls:*)",
      "Bash(find:*)",
      "Bash(rm:*)",
      "Bash(chmod:*)"
    ]
  }
}
```

- `"Bash(git commit:*)"` → `git commit -m "..."` など `git commit` で始まるコマンドすべて
- `"Bash(npm test)"` → `npm test` のみ（引数なし）
- `"Bash(rm:*)"` → `rm` で始まるコマンドすべて（`rm -rf` 含む、注意）

### WebFetch パーミッション

```json
{
  "allow": [
    "WebFetch(domain:github.com)",
    "WebFetch(domain:docs.anthropic.com)"
  ]
}
```

### 設定ファイルの優先順序

```
~/.claude/settings.json          ← ユーザーグローバル
.claude/settings.json            ← プロジェクト共有（Git管理）
.claude/settings.local.json      ← プロジェクトローカル（.gitignore）
```

後からロードされた設定が優先。`deny` は `allow` より優先。

### MCP ツールパーミッション

MCP ツールは `mcp__<server>__<tool>` パターンで命名される。

```json
{
  "allow": [
    "mcp__memory__create_entities",
    "mcp__memory__*"
  ],
  "deny": [
    "mcp__filesystem__write_file"
  ]
}
```

### Skill パーミッション

```json
{
  "allow": [
    "Skill(commit)",
    "Skill(review-pr *)"
  ],
  "deny": [
    "Skill(deploy *)"
  ]
}
```

- `Skill(name)` → 完全一致
- `Skill(name *)` → プレフィックス一致（引数付き）

### Task（サブエージェント）パーミッション

```json
{
  "deny": [
    "Task(Explore)",
    "Task(my-custom-agent)"
  ]
}
```

### ベストプラクティス

- **最小権限**: 必要なコマンドのみ許可
- **プレフィックス活用**: `"Bash(git:*)"` で Git 操作を一括許可
- **deny で安全弁**: `"Bash(rm -rf /)"` を明示的に deny
- **local.json**: 個人の開発環境固有の許可は `settings.local.json` に
