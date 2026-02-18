## claude-home/ - Claude Code 汎用拡張モジュール

プロジェクト非依存の再利用可能な Agents / Hooks / Skills。
Commands は claude-plugins に移行済み。dotfiles固有モジュールは `.claude/` を参照。

## ディレクトリ構造

```
claude-home/
├── agents/      (66ファイル / 6サフィックス)  - Task ツールで起動
├── hooks/       (14汎用 + svelte/3)       - イベント駆動型自動化
├── docs/                                  - ドキュメント
├── skills/      (20スキル)                 - 知識ベース・API統合
├── scripts/     (5スクリプト)              - メンテナンスユーティリティ
└── settings.local.json                     - ローカル権限設定
```

## Commands

全コマンドは claude-plugins リポジトリに移行済み。`/plugin install <name>@ysano-plugins` で利用。

## Agents 起動方法

```bash
Task: backend-architect    # 開発系
Task: code-reviewer    # 品質系
Task: semantic-architect   # WFGY推論系
```

領域: 開発 / データ・AI / インフラ / 品質 / ドキュメント / チーム / 推論・メモリ / チケット管理

## Skills

| スキル | 種別 | 用途 |
|--------|------|------|
| ticket-management | 理論 | AI-DLC チケット管理フレームワーク |
| github-projects-v2 | 知識ベース | GH Projects V2 API/CLI |
| linear | API統合 | Linear GraphQL/MCP (`LINEAR_API_TOKEN`) |
| jira | API統合 | Jira Cloud MCP (Atlassian Plugin) |
| cloudflare-manager | API統合 | CF Workers/KV/R2 (`CLOUDFLARE_API_KEY`) |
| ai-dlc-ceremonies | 理論 | AI-DLC セレモニー運営パターン・役割変化 |
| ai-dlc-upstream | 理論 | AI-DLC 上流ワークフロー契約（PRD/Architecture/Stories） |
| ai-dlc-estimate | 理論 | AI-DLC 見積もり技法 (AEF/Value-Based) |
| ai-dlc-sier | 理論 | 次世代 SIer 構造変革 (5ビジネスドメイン) |
| prompt-engineering | 理論 | Coding Agent プロンプト設計 |
| model-selection | 知識ベース | モデル選択戦略・コスト最適化・NLCC |
| prompt-management | タスク | プロンプト作成・更新・レビュー・削除 |
| security | 知識ベース | セキュリティ監査・堅牢化・依存関係・認証 |
| performance | 知識ベース | パフォーマンス監査・ビルド・バンドル・DB・CDN・キャッシュ・監視 |
| test | 知識ベース | テスト戦略・単体/結合/E2E・カバレッジ・負荷・ミューテーション |
| deploy | 知識ベース | CI/CD・コンテナ・K8s・リリース・ロールバック・ホットフィックス |
| setup | 知識ベース | 開発環境・Linting・Formatting・DB・API設計・モノレポ |
| docs | 知識ベース | アーキテクチャ文書・API文書・オンボーディング・マイグレーション |
| simulation | 知識ベース | シナリオ探索・デジタルツイン・意思決定木・市場モデリング |
| ai-dlc-observability | 知識ベース | DORA Four Keys・AI-Confidence・Sprint Health |

## 設計原則

- **チーム展開前提**: Solo 環境で開発・検証するが、Pod/Squad/Enterprise へのデプロイを想定して設計する。Solo 最適化のために汎用性を犠牲にしない
- **ドメイン分離**: `claude-home/`=汎用、`.claude/skills/`=dotfiles専用
- **レイヤード**: Commands(UI) → Agents(ロジック) → Skills(知識) → Hooks(イベント)
- **出典追跡**: `.gitattributes` で Claude-Command-Suite / Original を管理

## 関連ドキュメント

| パス | 内容 |
|------|------|
| `docs/catalog.md` | 全モジュール棚卸し・AI-DLC分類・整理方針 |
| `docs/README.md` | WFGY推論エージェント詳細 |
| `docs/WORKFLOW_EXAMPLES.md` | エージェントチェーンパターン |
| `skills/ticket-management/` | AI-DLC 理論・Atomic Spec・Agent Loop |
| `skills/ai-dlc-ceremonies/` | AI-DLC セレモニー運営・役割変化 |
| `skills/ai-dlc-upstream/` | AI-DLC 上流ワークフロー契約・成果物フォーマット |
| `skills/github-projects-v2/references/` | GH Projects V2 CLI/GraphQL |
| `skills/linear/references/` | Linear API/MCP |
| `skills/jira/references/` | Jira MCP ツール・アーキテクチャ |
