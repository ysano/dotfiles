---
name: Cloudflare Manager
description: Comprehensive Cloudflare account management for deploying Workers, KV Storage, R2, Pages, DNS, and Routes. Use when deploying cloudflare services, managing worker containers, configuring KV/R2 storage, or setting up DNS/routing. Requires CLOUDFLARE_API_KEY in .env and Bun runtime with dependencies installed.
---

# Cloudflare Manager

Comprehensive Cloudflare service management skill for Workers, KV Storage, R2 buckets, Pages, DNS records, and routing. Validates API credentials, extracts deployment URLs, and provides actionable error messages.

## Initial Setup

1. **Install Dependencies**
   ```bash
   cd ~/.claude/skills/cloudflare-manager && bun install
   ```

2. **Configure API Key** - Create `.env` in project root:
   ```bash
   CLOUDFLARE_API_KEY=your_api_token_here
   CLOUDFLARE_ACCOUNT_ID=your_account_id  # Optional, auto-detected
   ```
   Get token at https://dash.cloudflare.com/profile/api-tokens. Required permissions: Workers Scripts/KV/R2 Edit, Pages Edit, Zone DNS Edit.

3. **Validate Credentials**
   ```bash
   cd ~/.claude/skills/cloudflare-manager && bun scripts/validate-api-key.ts
   ```

## Quick Start Guide

### Deploy a Worker
```bash
bun scripts/workers.ts deploy worker-name ./worker-script.js
# Returns URL: https://worker-name.username.workers.dev
```

### KV Storage
```bash
bun scripts/kv-storage.ts create-namespace user-sessions    # Create namespace
bun scripts/kv-storage.ts write <ns-id> "key" '{"data":1}' # Write
bun scripts/kv-storage.ts read <ns-id> "key"                # Read
bun scripts/kv-storage.ts list-keys <ns-id>                 # List keys
bun scripts/kv-storage.ts delete <ns-id> "key"              # Delete
```
**Note**: KV uses eventual consistency; writes may take up to 60 seconds to propagate globally.

### R2 Storage
```bash
bun scripts/r2-storage.ts create-bucket media-assets
bun scripts/r2-storage.ts upload media-assets ./logo.png logo.png
bun scripts/r2-storage.ts list-objects media-assets
bun scripts/r2-storage.ts download media-assets logo.png ./out.png
```

### Pages
```bash
bun scripts/pages.ts deploy my-app ./dist  # Returns: https://my-app.pages.dev
bun scripts/pages.ts set-env my-app API_URL https://api.example.com
```
For file uploads, use Wrangler: `npx wrangler pages deploy ./dist --project-name=my-app`

### DNS & Routes
```bash
bun scripts/dns-routes.ts create-dns example.com A api 192.168.1.1
bun scripts/dns-routes.ts create-route example.com "*.example.com/api/*" api-handler
```

## Common Workflows

### Multi-Service Setup
```bash
bun scripts/kv-storage.ts create-namespace app-cache
bun scripts/r2-storage.ts create-bucket app-media
bun scripts/workers.ts deploy app-worker ./worker.js --kv-binding app-cache --r2-binding app-media
bun scripts/dns-routes.ts create-route example.com "example.com/*" app-worker
```

### Update Worker
```bash
bun scripts/workers.ts update worker-name ./new-script.js
bun scripts/workers.ts get worker-name
bun scripts/workers.ts list
```

## Error Handling

| Error | Cause | Solution |
|-------|-------|---------|
| `CLOUDFLARE_API_KEY not found` | Missing `.env` | Create `.env` in project root with API key |
| `Insufficient permissions` | Token lacks scope | Update at dash.cloudflare.com/profile/api-tokens |
| `Rate limit exceeded (429)` | Too many requests | Auto-retries with backoff; wait 1-2 min |
| Network error | API unreachable | Check internet connection |

## Best Practices

- **Security**: Never commit `.env`; use token auth; rotate every 90 days; least-privilege
- **Performance**: KV for reads (not frequent writes); R2 for large files (KV 25MB limit); workers under 1MB
- **Workflow**: Test locally with `wrangler dev`; validate after token updates; use staging
- **Naming**: Descriptive names (`user-auth-worker` not `worker1`); lowercase-hyphens for R2

## Script Reference

All in `~/.claude/skills/cloudflare-manager/scripts/`:

| Script | Purpose |
|--------|---------|
| validate-api-key.ts | Validate credentials and permissions |
| workers.ts | Deploy, update, manage Workers |
| kv-storage.ts | Manage KV namespaces and data |
| r2-storage.ts | Manage R2 buckets and objects |
| pages.ts | Deploy and configure Pages |
| dns-routes.ts | Configure DNS and worker routes |
| utils.ts | Shared API utilities |

Templates in `templates/`: worker-template.js, wrangler.toml.template

For advanced scenarios and comprehensive examples, see [examples.md](examples.md).
