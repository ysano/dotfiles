## Claude Code Hooks

イベント駆動型の自動化スクリプト集。`settings.json` の `hooks` セクションで有効化。

### 汎用スクリプト

| スクリプト | 用途 |
|-----------|------|
| `bash-validator.py` | Bash コマンド実行前のセキュリティ検証 |
| `typescript-check.py` | TypeScript 型チェック（インクリメンタル） |
| `bundle-size-check.py` | バンドルサイズ監視 |
| `format-and-lint.sh` | Prettier + ESLint 自動フォーマット |
| `test-runner.sh` | 変更ファイルの自動テスト実行 |
| `prompt-enhancer.py` | プロンプトコンテキスト付与 |

### 設定プリセット

| ファイル | 用途 |
|----------|------|
| `settings-minimal.json` | 最小構成 |
| `settings-comprehensive.json` | フル機能 |
| `settings-performance.json` | パフォーマンス重視 |
| `settings-team.json` | チーム協業向け |
| `settings-ai-dlc.json` | AI-DLC ワークフロー向け |

### AI-DLC 固有

| スクリプト | 用途 |
|-----------|------|
| `check-spec-existence.py` | コード変更前に Spec/Plan の存在確認（PreToolUse） |
| `auto-update-ticket.sh` | git push 後にチケットにコミット自動記録（PostToolUse） |

### Svelte 固有

`svelte/` サブディレクトリに Svelte/SvelteKit 専用の hooks、ドキュメント、設定を格納。

### 設定例

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
    ]
  }
}
```
