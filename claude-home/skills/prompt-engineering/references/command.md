## Command プロンプト リファレンス

> **注意**: Commands は Skills に統合された。既存の `.claude/commands/` は動作するが、新規作成は `.claude/skills/<name>/SKILL.md` を推奨。Skill リファレンスの `references/skill.md` を参照。

**配置**: `.claude/commands/<category>/<name>.md` または `.claude/commands/<name>.md`
**呼び出し**: `/category:name` または `/name`

### フォーマット

```markdown
---
description: "Perform comprehensive code quality review"
allowed-tools: Read, Grep, Glob, Bash
---

## Instructions

Analyze the codebase for quality, security, and maintainability issues:

1. **Code Quality**: Check naming, structure, complexity
2. **Security**: Identify OWASP Top 10 vulnerabilities

Output a structured report with severity levels and specific file:line references.
```

### フロントマター

| フィールド | 説明 |
|-----------|------|
| `description` | 必須。動詞で始める簡潔な説明（1行）。`/` メニューに表示 |
| `allowed-tools` | 許可なしで使えるツール。カンマ区切り文字列 |
| `model` | 使用するモデル |

### $ARGUMENTS 置換

ユーザーが `/command-name <引数>` で渡した引数を受け取る。

| 変数 | 説明 |
|------|------|
| `$ARGUMENTS` | 全引数。未使用時は末尾に `ARGUMENTS: <値>` を追加 |
| `$ARGUMENTS[N]` | 0始まりインデックスで引数アクセス |
| `$N` | `$ARGUMENTS[N]` の短縮形（`$0`, `$1`） |

```markdown
---
description: "Fix a GitHub issue by number"
---

Fix GitHub issue $ARGUMENTS following our coding standards.
```

### H1 禁止

`#` で始めない。`description` フロントマターがタイトルの役割を果たす。

### 避けるべきパターン

- `# Title` の H1 ヘッダー → description で代替
- `Remember to...` で終わるアドバイス → 指示として統合するか削除
- `## Best Practices` セクション → 指示に統合するか削除
- 300行超 → 単一責任に分割
