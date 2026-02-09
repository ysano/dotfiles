---
description: "Generate production-ready Cloudflare Workers code with best practices"
---

## Instructions

Generate Cloudflare Workers code based on: **$ARGUMENTS**

### Code Standards

- **TypeScript** by default（ES modules 形式のみ、Service Worker 形式は不可）
- 全 import を明示、型定義を含める
- 単一ファイルに保持（特に指定がない限り）
- 公式 SDK がある場合はそれを使用、FFI/native バインディングのあるライブラリは不可
- シークレットはハードコードしない

### Output Format

以下を個別の Markdown コードブロックで出力:
1. Main worker code (`index.ts`)
2. Configuration (`wrangler.jsonc`)
3. Example usage / curl commands

### wrangler.jsonc 必須設定

- `compatibility_date`: `"2025-03-07"`
- `compatibility_flags`: `["nodejs_compat"]`
- `observability`: `{ "enabled": true }`
- 使用する bindings のみ含める（dependencies は含めない）

### Cloudflare サービス統合

データストレージが必要な場合、適切なサービスを選択:
- **KV**: Key-Value ストレージ
- **Durable Objects**: 強整合性の状態管理、WebSocket（Hibernation API 必須）
- **D1**: リレーショナルデータ
- **R2**: オブジェクトストレージ
- **Queues**: 非同期処理

### WebSocket (Durable Objects 内)

- Hibernation API を使用（`this.ctx.acceptWebSocket(server)`）
- `async webSocketMessage()` / `async webSocketClose()` ハンドラを定義
- `addEventListener` パターンは使用しない

### Agents

- `agents` ライブラリを優先使用
- `Agent<Env, State>` で型パラメータを指定
- `wrangler.jsonc` に `migrations[].new_sqlite_classes` を設定
- クライアント側は `useAgent` React hook を使用
