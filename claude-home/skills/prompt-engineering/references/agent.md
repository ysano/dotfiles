## Agent（Subagent）プロンプト リファレンス

**配置**: `.claude/agents/<name>.md` または `~/.claude/agents/<name>.md`
**呼び出し**: Task ツール（`subagent_type: "<name>"`）

### フォーマット

```markdown
---
name: agent-name
description: Reviews code for quality and best practices. Use PROACTIVELY after code changes.
tools: Read, Glob, Grep, Bash
model: sonnet
---

You are a code reviewer. When invoked, analyze the code and provide
specific, actionable feedback on quality, security, and best practices.
```

フロントマターが設定、本文がシステムプロンプトになる。Agent は Claude Code のフルシステムプロンプトを受信しない。

### フロントマター

`name` と `description` のみ必須。

| フィールド | 必須 | 説明 |
|-----------|:---:|------|
| `name` | Yes | ファイル名と一致。小文字・ハイフン |
| `description` | Yes | Claude が委譲判断に使用。"Use PROACTIVELY for..." で起動条件を明示 |
| `tools` | No | 使用可能ツール。省略時は全ツール継承 |
| `disallowedTools` | No | 拒否ツール。継承/指定リストから除外 |
| `model` | No | `sonnet` / `opus` / `haiku` / `inherit`。デフォルト: `inherit` |
| `permissionMode` | No | 権限モード（後述） |
| `maxTurns` | No | 最大エージェントターン数 |
| `skills` | No | 起動時にコンテキストに注入する Skill 名のリスト |
| `mcpServers` | No | 使用可能な MCP サーバー |
| `hooks` | No | Agent ライフサイクルにスコープされたフック |
| `memory` | No | 永続メモリスコープ（後述） |

### permissionMode

| モード | 動作 |
|--------|------|
| `default` | 標準の権限チェック（プロンプト表示） |
| `acceptEdits` | ファイル編集を自動承認 |
| `dontAsk` | 権限プロンプトを自動拒否（明示許可ツールは動作） |
| `delegate` | Agent Teams リード用の調整専用モード |
| `bypassPermissions` | 全権限チェックをスキップ（注意） |
| `plan` | 読み取り専用（Plan モード） |

### memory（永続メモリ）

| スコープ | パス | 用途 |
|---------|------|------|
| `user` | `~/.claude/agent-memory/<name>/` | 全プロジェクト共通の学習 |
| `project` | `.claude/agent-memory/<name>/` | プロジェクト固有（Git管理可） |
| `local` | `.claude/agent-memory-local/<name>/` | プロジェクト固有（Git管理外） |

有効化すると `MEMORY.md`（先頭200行）がコンテキストに注入される。

### tools の制御

```yaml
# 許可リスト
tools: Read, Glob, Grep, Bash

# 拒否リスト（継承から除外）
disallowedTools: Write, Edit

# サブエージェント起動制限（--agent 使用時のみ有効）
tools: Task(worker, researcher), Read, Bash
```

### H1 禁止

`#` で始めない。フロントマターの `name` と `description` がその役割を果たす。

### 単一責任

- 1エージェント = 1つの明確な専門領域
- 50-150行が適正範囲
- 500行超は分割を検討

### model 選択基準

| model | 用途 |
|-------|------|
| `haiku` | 定型的な検索・分類・フォーマット変換 |
| `sonnet` | 大半のタスク（コスト・速度バランス） |
| `opus` | 複雑な推論・アーキテクチャ設計 |
| `inherit` | メイン会話と同じモデル（デフォルト） |

> 詳細な選択戦略（コスト分析、NLCC、ハイブリッドルーティング）は `model-selection` Skill を参照。

### Agent 価値判断基準

Agent を作成・レビューする際、以下の基準で本当に必要かを判断する。

**価値のある Agent（作成すべき）**:
- ドメイン固有の決定木やワークフローがある（例: インシデント対応の Severity 判定）
- Claude が学習データだけでは知り得ない組織固有の規約・パターンを持つ
- 特定ツールの組み合わせや実行順序に専門性がある（例: Svelte 5 runes + Storybook CSF3）
- 削除して vanilla Claude に依頼した場合、品質が明確に落ちる

**無価値な Agent（作成すべきでない）**:
- 本文が汎用的な「ベストプラクティス」の羅列（Claude が既知の内容）
- description を Task プロンプトにコピーすれば同等の結果が得られる
- 複数 Agent 間で同一の定型文ブロック（"Core Development Philosophy" 等）がコピーされている
- 「〇〇の専門家」と名乗るだけで固有知識がない

**判定テスト**: "この Agent を削除し、description の内容だけを Task プロンプトに渡して同じタスクを実行した場合、品質は落ちるか？" — No なら不要。

### MCP 設定戦略

Agent の `tools:` に MCP ツール（`mcp__*`）を指定する場合の設計原則。

**原則: 公式 Marketplace に委譲し、固有のものだけバンドルする**

1. **公式 Marketplace 確認**: `context7`, `playwright`, `atlassian`, `linear` 等の汎用 MCP は公式プラグインとして提供されている。Plugin の `.mcp.json` にバンドルしない
2. **重複回避**: 同じ MCP サーバーを複数プラグインにバンドルすると、全プラグインインストール時に重複プロセスが起動する。同名でもプラグインスコープが別のため dedup されない可能性がある
3. **固有 MCP のみバンドル**: 公式 Marketplace に存在しない MCP サーバーのみ `.mcp.json` に含める
4. **認証依存の MCP**: OAuth/API Token が必要な MCP（Azure, Atlassian 等）は README でセットアップ手順を案内。プラグインに埋め込んでも認証フローが別途必要

```
# .mcp.json に含めるべき例（公式になし）
{
  "magic": {
    "command": "npx",
    "args": ["-y", "@21st-dev/magic"],
    "env": { "API_KEY": "${TWENTYFIRST_API_KEY}" }
  }
}

# README で案内すべき例（公式プラグインあり）
# /plugin install context7@claude-plugins-official
# /plugin install playwright@claude-plugins-official
```

### 避けるべきパターン

- description にいつ使うかが書かれていない
- 独自コマンド体系（`*help` など）→ 標準の tool use を使う
- 過度なペルソナ設定（名前、アイコン、挨拶）→ 能力と制約に集中
- 呼び出し元への依存（「オーケストレーターに報告」等）
