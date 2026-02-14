# Claude Home Module Catalog

claude-home/ 配下の全モジュール棚卸し・AI-DLC 分類・整理方針。

## 概要

| 種別 | 数 | 配置 |
|---|---|---|
| Commands | 207 (22 categories + 4 top-level) | `commands/` |
| Agents | 67 | `agents/` |
| Skills | 18 | `skills/` |
| Hooks | 14 (general 11 + svelte 3) | `hooks/` |
| Hook Settings | 6 presets | `hooks/settings-*.json` |
| Maintenance Scripts | 5 | `scripts/` |
| **Total** | **309** | |

---

## Agent Naming Convention

6 種の統一サフィックス。詳細: `docs/agent-naming-convention.md`

| Suffix | 意味 | Count |
|---|---|---|
| `-architect` | 戦略設計・アーキテクチャ判断 | 8 |
| `-pro` | 技術/ドメイン専門・実装 | 26 |
| `-reviewer` | 品質ゲート・レビュー・監査 | 7 |
| `-operator` | CI/CD・デプロイ・運用 | 7 |
| `-agent` | メタ/オーケストレーション | 12 |
| (固有名) | WFGY 認知システム | 6 |

---

## AI-DLC Phase Mapping

各モジュールを AI-DLC ライフサイクルフェーズにマッピング。

### Upstream (要件定義・設計)

| 種別 | モジュール | 説明 |
|---|---|---|
| Command | `/ai-dlc:create-prd` | インタビュー駆動 PRD 作成 |
| Command | `/ai-dlc:create-architecture` | ADR 駆動アーキテクチャ設計 |
| Command | `/ai-dlc:create-stories` | Epic/Story 分解 → Atomic Spec スタブ |
| Command | `/ai-dlc:quick-spec` | 軽量単一文書 Spec (5 Story 以下) |
| Command | `/ai-dlc:translate-upstream` | BMAD 等外部文書 → AI-DLC 変換 |
| Command | `/spec-elicitation` | インタビュー駆動 Spec 開発 |
| Command | `/adr` | Architecture Decision Record 生成 |
| Skill | ai-dlc-upstream | 上流パイプライン契約・成果物フォーマット |
| Skill | ai-dlc-estimate | 見積もり技法 (AEF, Value-Based) |
| Skill | ai-dlc-sier | SIer 構造変革 (Sales/Branding/PM/Consulting) |
| Skill | simulation | シナリオ探索・デジタルツイン・意思決定木・市場モデリング |
| Agent | product-architect | プロダクトビジョン・戦略・ロードマップ |
| Agent | backend-architect | バックエンドシステム設計 |
| Agent | project-architect | プロジェクト初期化・構造設計 |
| Agent | strategy-architect | ビジネス・技術シナリオモデリング |

### Planning (スプリント計画)

```
/ai-dlc:refine → /ai-dlc:plan → /ai-dlc:digest (daily) → /ai-dlc:status (随時)
     │                │                │                         │
     └─ Backlog 精査   └─ Sprint 計画    └─ 日次ダイジェスト        └─ 健全性確認
```

| 種別 | モジュール | 説明 |
|---|---|---|
| Command | `/ai-dlc:status` | ライフサイクル健全性ダッシュボード |
| Command | `/ai-dlc:refine` | バックログリファインメント (Atomic Spec スコアリング) |
| Command | `/ai-dlc:plan` | 3 フェーズスプリント計画 |
| Command | `/ai-dlc:digest` | 非同期 AI ダイジェスト + 人間判断アジェンダ |
| Command | `/orchestration:start` | タスクオーケストレーション開始 |
| Skill | ticket-management | Atomic Spec・Agent Loop・AI Janitor |
| Skill | ai-dlc-ceremonies | セレモニーパターン・役割変化 |
| Agent | orchestration-agent | 複合タスク調整・依存管理・分解 |
| Agent | team-agent | マルチエージェントチーム編成 |

### Development (Agent Loop: 実装)

| 種別 | モジュール | 説明 |
|---|---|---|
| Command | `/dev:code-review` | コード品質レビュー |
| Command | `/dev:refactor-code` | リファクタリング |
| Command | `/dev:debug-error` | エラー診断・修正 |
| Command | `/dev:fix-issue` | GitHub Issue 修正 |
| Command | `/dev:incremental-feature-build` | 逐次的機能構築 |
| Command | `/dev:parallel-feature-build` | 並列エージェント機能構築 |
| Command | `/dev:pull-request` | PR 作成 |
| Skill | setup | 開発環境・Linting・Formatting・DB・API設計・モノレポ |
| Skill | docs | アーキテクチャ文書・API文書・オンボーディング・マイグレーション |
| Agent | frontend-pro | React コンポーネント構築 |
| Agent | react-pro | React 専門 |
| Agent | nextjs-pro | Next.js 専門 |
| Agent | svelte-pro | Svelte 5+ / SvelteKit |
| Agent | typescript-pro | TypeScript 型安全設計 |
| Agent | python-pro | Python 専門 |
| Agent | golang-pro | Go 専門 |
| Agent | fullstack-pro | フルスタック |
| Agent | mobile-pro | React Native / Flutter |
| Agent | electron-pro | Electron デスクトップ |
| Agent | swift-macos-pro | macOS / SwiftUI |
| Agent | graphql-architect | GraphQL API 設計 |
| Agent | postgresql-pglite-pro | DB アーキテクチャ |
| Agent | debug-pro | デバッグ専門 |
| Agent | ai-pro | LLM / RAG アプリ |
| Agent | data-pipeline-pro | ETL / データ基盤 |
| Agent | data-pro | SQL / BigQuery |
| Agent | ml-pro | ML ライフサイクル |

### Verification (検証)

| 種別 | モジュール | 説明 |
|---|---|---|
| Command | `/ai-dlc:review` | AI-DLC 品質ゲート PR レビュー |
| Command | `/ai-dlc:verify` | スプリント成果検証 |
| Command | `/test:write-tests` | テスト作成 |
| Command | `/test:generate-test-cases` | テストケース自動生成 |
| Command | `/security:security-audit` | セキュリティ評価 |
| Command | `/performance:performance-audit` | パフォーマンス監査 |
| Skill | security | セキュリティ監査・堅牢化・依存関係・認証手順 |
| Skill | performance | パフォーマンス監査・ビルド・バンドル・DB・CDN・キャッシュ・監視手順 |
| Skill | test | テスト戦略・単体/結合/E2E・カバレッジ・負荷テスト手順 |
| Hook | check-spec-existence.py | Write/Edit 前に Spec 存在確認 |
| Hook | typescript-check.py | TS 型チェック |
| Hook | test-runner.sh | テスト自動実行 |
| Hook | bash-validator.py | Bash コマンド安全性検証 |
| Agent | code-reviewer | コードレビュー (教育的) |
| Agent | architecture-reviewer | アーキテクチャ整合性 |
| Agent | security-reviewer | セキュリティ脆弱性 |
| Agent | performance-reviewer | パフォーマンス最適化 |
| Agent | performance-architect | パフォーマンス戦略 |
| Agent | qa-reviewer | QA 戦略 |
| Agent | test-operator | テスト自動化 CI/CD |
| Agent | dependency-reviewer | 依存関係分析 |

### Operations (運用・デプロイ)

| 種別 | モジュール | 説明 |
|---|---|---|
| Command | `/deploy:prepare-release` | リリース準備・検証 |
| Command | `/deploy:hotfix-deploy` | 緊急ホットフィックス |
| Command | `/deploy:rollback-deploy` | デプロイロールバック |
| Command | `/deploy:ci-setup` | CI パイプライン設定 |
| Command | `/deploy:containerize-application` | コンテナ化 |
| Command | `/deploy:setup-kubernetes-deployment` | K8s デプロイ |
| Hook | auto-update-ticket.sh | push 時に Issue 自動コメント |
| Hook | format-and-lint.sh | Prettier + ESLint 自動修正 |
| Skill | deploy | CI/CD・コンテナ・K8s・リリース・ロールバック手順 |
| Agent | deploy-operator | CI/CD パイプライン |
| Agent | cloud-architect | クラウドインフラ設計 |
| Agent | azure-devops-operator | Azure DevOps |
| Agent | incident-operator | インシデント対応 |
| Agent | devops-incident-operator | DevOps インシデント |
| Agent | release-operator | リリース管理 |
| Agent | database-pro | DB パフォーマンスチューニング |
| Skill | cloudflare-manager | Workers/KV/R2 管理 |

### Improvement (振り返り・改善)

| 種別 | モジュール | 説明 |
|---|---|---|
| Command | `/ai-dlc:diagnose` | データ駆動スプリント診断 |
| Command | `/ai-dlc:calibrate` | エージェント設定・性能分析 |
| Hook | session-learning-capture.sh | セッション終了時学習記録 |
| Agent | legacy-pro | レガシーシステム近代化 |
| Agent | dx-pro | 開発者体験最適化 |

---

## AI-DLC Role Mapping

AI-DLC の役割定義と対応モジュール。

### Value Orchestrator (旧 PO)

価値仮説の設計・検証、AI コンテキスト提供、Outcome Done 定義。

- `/ai-dlc:create-prd`, `/ai-dlc:verify`
- `product-architect` agent
- `ai-dlc-upstream` skill (PRD artifact)

### Agent Orchestration Coach (旧 SM)

AI-人間協調パターン、エージェントガバナンス、フロー最適化。

- `/ai-dlc:status`, `/ai-dlc:refine`, `/ai-dlc:plan`, `/ai-dlc:diagnose`, `/ai-dlc:calibrate`
- `ai-dlc-ceremonies` skill
- `team-agent`, `orchestration-agent` agents

### AI Strategy Architect (旧 PM)

AI ケイパビリティ思考、プロトタイプ駆動検証、戦略設計。

- `/ai-dlc:create-architecture`, `/ai-dlc:quick-spec`
- `strategy-architect`, `project-architect` agents
- `ai-dlc-estimate`, `ai-dlc-sier` skills

### Delivery System Operator (旧 PjM)

AI システム運用監視、デリバリーパイプライン最適化。

- `/orchestration:*` commands (10 commands)
- `/deploy:*` commands
- `deploy-operator`, `release-operator` agents
- `deploy`, `cloudflare-manager` skills

### Senior Architect

システム設計、DB スキーマ、CLAUDE.md メンテナンス。

- `/dev:code-review`, `/dev:refactor-code`
- `backend-architect`, `graphql-architect`, `fullstack-pro` agents
- Technology-specific agents (typescript-pro, python-pro, etc.)

### ARE (AI Reliability Engineer)

AI 生成コードの論理欠陥・セキュリティホール・Spec 不整合検出。

- `/ai-dlc:review`, `/test:write-tests`, `/security:security-audit`
- `code-reviewer`, `security-reviewer`, `qa-reviewer` agents
- Verification hooks (typescript-check, test-runner, bash-validator)

### Prompt Architect

CLAUDE.md / SKILL 設計、AI ガバナンスリード。

- `/ai-dlc:calibrate`
- `prompt-agent`, `prompt-reviewer` agents
- `prompt-engineering`, `prompt-management` skills
- Skill lifecycle agents (elicitation, generator, validator, docs)

---

## Reusability Tiers

複数プロジェクトでの再利用性による分類。

### Tier 1: Core (プロジェクト非依存) — ~59%

どのプロジェクトでもそのまま使えるモジュール。

**Commands**: dev/*, test/*, security/*, docs/*, deploy/*, setup/*, performance/*, project/*, team/*
**Agents**: 全開発系 (-pro), 全品質系 (-reviewer), 全インフラ系 (-operator), docs-pro, debug-pro
**Skills**: prompt-engineering, prompt-management, security, performance, test, deploy, setup, docs
**Hooks**: bash-validator, format-and-lint, session-learning-capture

### Tier 2: Framework (技術スタック依存) — ~10%

特定フレームワーク使用時に有効。

**Commands**: svelte/*, rust/*
**Agents**: svelte-pro, svelte-storybook-pro, svelte-testing-pro, nextjs-pro, react-pro, swift-macos-pro, electron-pro
**Hooks**: svelte-validator, component-analyzer, story-sync-check, typescript-check
**Settings**: settings-storybook.json

### Tier 3: Domain (方法論依存) — ~7%

AI-DLC 採用プロジェクトで有効。

**Commands**: ai-dlc/*
**Skills**: ticket-management, ai-dlc-ceremonies, ai-dlc-upstream, ai-dlc-estimate, ai-dlc-sier, github-projects-v2, linear, jira
**Hooks**: check-spec-existence, auto-update-ticket
**Settings**: settings-ai-dlc.json
**Agents**: github-board-agent, github-ticket-agent, linear-ticket-agent, ticket-sync-agent

### Tier 4: Meta (Claude Code 拡張) — ~15%

Claude Code 自体の拡張・推論強化。

**Commands**: wfgy/*, reasoning/*, boundary/*, semantic/*, memory/*, orchestration/*, skills/*
**Agents**: WFGY 系 7 agents, skill lifecycle 4 agents
**Scripts**: 5 maintenance scripts

### Tier 5: Experimental (実験的) — ~9%

**Commands**: simulation/* (8 commands)
**Skills**: simulation
**Agents**: prompt-reviewer (multi-model review)

---

## Commands Inventory (207)

### Top-Level (4)

| Command | Description |
|---|---|
| `/handoff` | コンテキストウィンドウ限界時のハンドオフ文書作成 |
| `/handoff-continue` | ハンドオフ + 新セッション自動起動 |
| `/spec-elicitation` | インタビュー駆動 Spec 開発 |
| `/adr` | Architecture Decision Record 生成 |

### ai-dlc (13)

calibrate, create-architecture, create-prd, create-stories, diagnose, digest, plan, quick-spec, refine, review, status, translate-upstream, verify

### boundary (4)

boundary-detect, boundary-heatmap, boundary-risk-assess, boundary-safe-bridge

### context (1)

optimize-prompt

### deploy (8)

add-changelog, ci-setup, containerize-application, hotfix-deploy, prepare-release, rollback-deploy, setup-automated-releases, setup-kubernetes-deployment

### dev (20)

all-tools, architecture-scenario-explorer, clean-branches, cloudflare-worker, code-permutation-tester, code-review, code-to-task, debug-error, directory-deep-dive, explain-code, fix-issue, generate-linear-worklog, git-status, incremental-feature-build, parallel-feature-build, prime, pull-request, refactor-code, rule2hook, ultra-think

### docs (5)

create-architecture-documentation, create-onboarding-guide, generate-api-documentation, migration-guide, troubleshooting-guide

### memory (5)

memory-checkpoint, memory-compress, memory-merge, memory-prune, memory-recall

### orchestration (10)

commit, find, log, move, remove, report, resume, start, status, sync

### performance (8)

add-performance-monitoring, implement-caching-strategy, optimize-build, optimize-bundle-size, optimize-database-performance, performance-audit, setup-cdn-optimization, system-behavior-simulator

### project (14)

add-package, create-feature, init-project, milestone-tracker, pac-configure, pac-create-epic, pac-create-ticket, pac-update-status, pac-validate, project-health-check, project-timeline-simulator, project-to-linear, todo-branch, todo-worktree

### reasoning (4)

reasoning-chain-validate, reasoning-logic-vector, reasoning-resonance, reasoning-tension-calc

### rust (6)

audit-clean-arch, audit-dependencies, audit-layer-boundaries, audit-ports-adapters, setup-tauri-mcp, suggest-refactor

### security (4)

add-authentication-system, dependency-audit, security-audit, security-hardening

### semantic (6)

semantic-node-build, semantic-tree-export, semantic-tree-import, semantic-tree-init, semantic-tree-switch, semantic-tree-view

### setup (12)

create-database-migrations, design-database-schema, design-rest-api, implement-graphql-api, migrate-to-typescript, modernize-deps, setup-development-environment, setup-formatting, setup-linting, setup-monitoring-observability, setup-monorepo, setup-rate-limiting

### simulation (8)

business-scenario-explorer, constraint-modeler, decision-tree-explorer, digital-twin-creator, future-scenario-generator, market-response-modeler, simulation-calibrator, timeline-compressor

### skills (2)

build-skill, package-skill

### spec-workflow (3)

parallel-tasks, parallel-tasks-help, spec-workflow-setup

### svelte (13)

svelte:a11y, svelte:component, svelte:debug, svelte:migrate, svelte:optimize, svelte:scaffold, svelte:storybook-setup, svelte:storybook-story, svelte:storybook-migrate, svelte:test, svelte:test-setup, svelte:test-fix, svelte:test-coverage

### team (11)

architecture-review, decision-quality-analyzer, dependency-mapper, estimate-assistant, github-issue-automation, github-project-manager, github-workflow-automation, issue-triage, memory-spring-cleaning, migration-assistant, team-workload-balancer

### test (9)

add-mutation-testing, add-property-based-testing, e2e-setup, generate-test-cases, setup-comprehensive-testing, setup-load-testing, setup-visual-testing, test-coverage, write-tests

### wfgy (6)

wfgy-bbam, wfgy-bbcr, wfgy-bbmc, wfgy-bbpf, wfgy-formula-all, wfgy-init

---

## Agents Inventory (67)

### `-architect` — 戦略設計 (8)

| Agent | Description | Model |
|---|---|---|
| backend-architect | バックエンドシステム設計 | sonnet |
| cloud-architect | AWS/Azure/GCP / Terraform | sonnet |
| graphql-architect | GraphQL API 設計 | sonnet |
| project-architect | プロジェクト初期化・構造 | default |
| strategy-architect | シナリオモデリング・戦略判断 | default |
| performance-architect | パフォーマンス戦略設計 | sonnet |
| product-architect | プロダクトビジョン・戦略 | sonnet |
| semantic-architect | セマンティックツリー構築 (WFGY) | sonnet |

### `-pro` — 技術専門・実装 (26)

| Agent | Description | Model |
|---|---|---|
| react-pro | React 専門 (Hooks, Context, State) | sonnet |
| nextjs-pro | Next.js (SSR/SSG/App Router) | sonnet |
| python-pro | Python 専門 | sonnet |
| golang-pro | Go 専門 | sonnet |
| typescript-pro | TypeScript 型安全設計 | sonnet |
| electron-pro | Electron デスクトップ | sonnet |
| postgresql-pglite-pro | PostgreSQL / PgLite | sonnet |
| swift-macos-pro | macOS / SwiftUI | sonnet |
| frontend-pro | React コンポーネント構築 | sonnet |
| fullstack-pro | フルスタック E2E | sonnet |
| mobile-pro | React Native / Flutter | sonnet |
| svelte-pro | Svelte 5+ / SvelteKit | sonnet |
| svelte-storybook-pro | Storybook for SvelteKit | sonnet |
| svelte-testing-pro | Svelte テスト (Vitest/Playwright) | sonnet |
| ai-pro | LLM / RAG / Prompt Pipeline | sonnet |
| data-pipeline-pro | ETL / データ基盤 / Streaming | sonnet |
| data-pro | SQL / BigQuery / 分析 | sonnet |
| ml-pro | ML ライフサイクル / MLOps | sonnet |
| debug-pro | デバッグ専門 | sonnet |
| legacy-pro | レガシーシステム近代化 | sonnet |
| database-pro | DB パフォーマンスチューニング | sonnet |
| ui-pro | UI デザイン・デザインシステム | sonnet |
| ux-pro | UX リサーチ・ユーザビリティ | sonnet |
| dx-pro | 開発者体験最適化 | sonnet |
| docs-pro | ソフトウェアドキュメンテーション | sonnet |
| api-docs-pro | API ドキュメンテーション (OpenAPI) | sonnet |

### `-reviewer` — 品質ゲート (7)

| Agent | Description | Model |
|---|---|---|
| code-reviewer | コードレビュー (教育的) | haiku |
| architecture-reviewer | アーキテクチャ整合性 | haiku |
| security-reviewer | セキュリティ脆弱性 | sonnet |
| performance-reviewer | パフォーマンス最適化 | default |
| qa-reviewer | QA 戦略・プロセス | sonnet |
| dependency-reviewer | 依存関係分析 | default |
| prompt-reviewer | プロンプトレビュー・改善提案 | default |

### `-operator` — 運用・デプロイ (7)

| Agent | Description | Model |
|---|---|---|
| deploy-operator | CI/CD / コンテナオーケストレーション | sonnet |
| release-operator | リリース管理 | default |
| incident-operator | インシデント対応 | sonnet |
| devops-incident-operator | DevOps インシデント対応 | sonnet |
| azure-devops-operator | Azure DevOps / Azure CLI | sonnet |
| test-operator | テスト自動化 CI/CD | haiku |
| commit-operator | タスク完了・Git ワークフロー | default |

### `-agent` — メタ/オーケストレーション (13)

| Agent | Description | Model |
|---|---|---|
| team-agent | マルチエージェント編成 | haiku |
| orchestration-agent | 複合タスク調整・分解 | default |
| prompt-agent | プロンプト設計・最適化 | sonnet |
| task-status-agent | タスクステータス管理 | default |
| skill-elicitation-agent | 要件抽出 → Skill 仕様 | default |
| skill-generator-agent | 仕様 → コード生成 | default |
| skill-validator-agent | Skill テスト・検証 | default |
| skill-docs-agent | Skill ドキュメンテーション | default |
| github-board-agent | GH Project ボード管理 | default |
| github-ticket-agent | GH Issue CRUD・トリアージ | default |
| linear-ticket-agent | Linear Issue 操作・分析 | default |
| ticket-sync-agent | GH ↔ Linear 同期 | default |
| skill-migration-agent | Command → Skill マイグレーション | sonnet |

### WFGY 固有名 — 認知システム (6)

| Agent | Description |
|---|---|
| memory-curator | メモリ圧縮・プルーニング・チェックポイント |
| boundary-guardian | ハルシネーション防止 |
| reasoning-validator | 推論整合性検証 |
| cognitive-debugger | 推論障害診断・修復 |
| logic-synthesizer | 多経路推論・解合成 |
| decision-navigator | 戦略的意思決定支援 |

---

## Skills Inventory (18)

### Theory / Knowledge Base

| Skill | Type | Purpose |
|---|---|---|
| ticket-management | Theory | Atomic Spec, Agent Loop, AI Janitor, AI-Native DoD |
| ai-dlc-ceremonies | Theory | セレモニーパターン, 役割変化, Adoption Playbook |
| ai-dlc-upstream | Theory | 上流パイプライン契約 (PRD/Architecture/Stories) |
| ai-dlc-estimate | Theory | 見積もり技法 (AEF, Value-Based, Hybrid) |
| ai-dlc-sier | Theory | SIer 構造変革 (5 ビジネスドメイン) |
| prompt-engineering | Theory | Coding Agent プロンプト設計ガイドライン |
| security | Knowledge Base | セキュリティ監査・堅牢化・依存関係・認証 (OWASP/NIST/CIS) |
| performance | Knowledge Base | パフォーマンス監査・ビルド・バンドル・DB・CDN・キャッシュ・監視 |
| test | Knowledge Base | テスト戦略・単体/結合/E2E・カバレッジ・負荷・ミューテーション |
| deploy | Knowledge Base | CI/CD・コンテナ・K8s・リリース・ロールバック・ホットフィックス |
| setup | Knowledge Base | 開発環境・Linting・Formatting・DB・API設計・モノレポ |
| docs | Knowledge Base | アーキテクチャ文書・API文書・オンボーディング・マイグレーション |
| simulation | Knowledge Base | シナリオ探索・デジタルツイン・意思決定木・市場モデリング |

### API Integration

| Skill | Type | Env Vars |
|---|---|---|
| github-projects-v2 | Knowledge Base | GitHub PAT (`gh auth`) |
| linear | API Integration | `LINEAR_API_TOKEN` |
| jira | API Integration | OAuth (Atlassian Plugin) |
| cloudflare-manager | API Integration | `CLOUDFLARE_API_KEY` |

### Task / Management

| Skill | Type | Purpose |
|---|---|---|
| prompt-management | Task | プロンプト CRUD + レビュー |

---

## Hooks Inventory

### General Hooks (11)

| Hook | Event | Matcher | Purpose | Timeout |
|---|---|---|---|---|
| check-spec-existence.py | PreToolUse | Write/Edit/MultiEdit | Spec 存在確認 | 5s |
| bash-validator.py | PreToolUse | Bash | コマンド安全性検証 | 5s |
| auto-update-ticket.sh | PostToolUse | Bash (git push) | Issue 自動コメント | 10s |
| typescript-check.py | PostToolUse | Write/Edit/MultiEdit | TS 型チェック | 30s |
| bundle-size-check.py | Pre/PostToolUse | Write/Edit/MultiEdit | バンドルサイズ監視 | 30s/120s |
| format-and-lint.sh | PostToolUse | Write/Edit/MultiEdit | Prettier + ESLint | 10s |
| prompt-enhancer.py | UserPromptSubmit | (all) | Svelte コンテキスト注入 | 5s |
| session-learning-capture.sh | Stop | (all) | 学習記録リマインド | 10s |
| test-runner.sh | Pre/PostToolUse | Write/Edit/MultiEdit | テスト自動実行 | 30s/60s |
| svelte-validator.py | PostToolUse | Write/Edit/MultiEdit | Svelte 構文検証 | 10s |
| component-analyzer.py | PostToolUse | Write/Edit/MultiEdit | コンポーネント複雑度分析 | 10s |

### Svelte Hooks (3)

| Hook | Event | Purpose | Timeout |
|---|---|---|---|
| svelte/svelte-validator.py | PostToolUse | .svelte ファイル検証 | 30s |
| svelte/component-analyzer.py | PostToolUse | コンポーネント複雑度 | 10s |
| svelte/story-sync-check.sh | PostToolUse | Storybook 同期リマインド | 5s |

### Settings Presets (6)

| Preset | Use Case | Active Hooks |
|---|---|---|
| settings-minimal.json | ソロ開発 | format-and-lint のみ |
| settings-ai-dlc.json | AI-DLC ワークフロー | check-spec, bash-validator, auto-update-ticket, session-learning |
| settings-comprehensive.json | フル機能 | 全 general hooks + Svelte |
| settings-performance.json | パフォーマンス重視 | bundle-size, component-analyzer, performance-analyzer |
| settings-team.json | チーム協業 | naming-convention, doc-enforcer, todo-tracker, security-check |
| settings-storybook.json | Storybook 同期 | story-sync-check, storybook-validator |

---

## Consolidation Recommendations

### Tier 1: Immediate ✅ DONE

3 commands 削除済み: `changelog-demo-command`(デモスタブ), `test-changelog-automation`(テストスタブ), `doc-api`(重複)。

### Tier 2: Agent Merges ✅ DONE

4 agents 削除済み: `code-auditor`(→code-reviewer), `architecture-auditor`(→architecture-reviewer), `test-engineer`(→qa-reviewer+test-operator), `task-decomposer`(→orchestration-agent)。

### Tier 3: WFGY Consolidation ✅ DONE (部分)

Commands: 明確な重複2件を削除済み。
- `/reasoning:multi-path` → `/wfgy:bbpf` に統一（90%重複）
- `/boundary:bbcr-fallback` → `/wfgy:bbcr` に統一（重複）
- 残りの各コマンドは独自機能を持つため維持

Agents: knowledge-mapper → semantic-architect に統合済み。
- cognitive-debugger / reasoning-validator: ユースケース異なる（reactive修復 vs proactive検証）ため個別維持
- logic-synthesizer / decision-navigator: ユースケース異なる（技術的合成 vs 戦略的意思決定）ため個別維持
- 結果: 7 → 6 agents

### Tier 4: Svelte Storybook ~~(6 commands → 3)~~ ✅ DONE

6 → 3 に統合済み。削除: `storybook`(汎用), `storybook-mock`(→story に統合), `storybook-troubleshoot`(→setup に統合)。残存: `storybook-setup`, `storybook-story`, `storybook-migrate`。

### Tier 5: Cross-references ✅ DONE

5ペア（10ファイル）に "See also" 相互参照を追加済み:
- `security-audit` ↔ `security-hardening`
- `performance-audit` ↔ `add-performance-monitoring`
- `dev/code-review` ↔ `ai-dlc/review`
- `incremental-feature-build` ↔ `parallel-feature-build`
- `setup-comprehensive-testing` ↔ `e2e-setup`

### Tier 6: Naming Convention ✅ DONE

66 agents を 6 種サフィックスに統一。詳細: `docs/agent-naming-convention.md`
- 44 agents 改名、22 agents 維持
- 21 種のサフィックス → 6 種に統一

---

## Maintenance Scripts (5)

| Script | Purpose | In-Place |
|---|---|---|
| compress_commands.py | 500 行超のコマンド圧縮 | Yes |
| fix_agent_h1.py | Agent ファイルの H1 ヘッダー除去 | Yes |
| fix_all_format.py | FM 生成 + H1 除去 + ノイズ除去 | Yes |
| fix_command_format.py | Command ファイルの FM + H1 修正 | Yes |
| remove_noise.py | Best Practices/Tips/Remember 除去 | Yes |

**実行順序**: remove_noise → fix_command_format → fix_agent_h1 → fix_all_format → compress_commands
