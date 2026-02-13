---
description: "List AI-DLC commands for upstream workflow and sprint ceremonies"
---

AI-DLC (AI Development Lifecycle) コマンド群。
AI-人間協働チーム向けのセレモニー運営を支援する。

知識ベースは `ai-dlc-ceremonies` Skill、チケットライフサイクルは `ticket-management` Skill、上流ワークフロー契約は `ai-dlc-upstream` Skill を参照。
既存の `/team:*` コマンドは汎用的なチーム運営向け。AI-DLC コマンドは AI-Native なセレモニーパターンに特化。

## 上流コマンド（Upstream）

要件定義から Story 分解までの成果物パイプライン。

| コマンド | 用途 | 出力 |
|---|---|---|
| `/ai-dlc:quick-spec` | 軽量単一文書スペック（1-5 Story 向け） | `docs/spec-*.md` |
| `/ai-dlc:create-prd` | インタビュー駆動 PRD 作成（FR-NNN 番号付き） | `docs/prd-*.md` |
| `/ai-dlc:create-architecture` | ADR 駆動アーキテクチャ設計 | `docs/architecture-*.md` |
| `/ai-dlc:create-stories` | PRD + Architecture → Atomic Spec スタブ群 | `docs/stories-*.md` |
| `/ai-dlc:review` | AI-DLC 品質ゲート付きコードレビュー | レビューレポート |
| `/ai-dlc:translate-upstream` | BMAD / 外部文書 → AI-DLC フォーマット変換 | 変換済み文書 + ギャップレポート |

### Full Pipeline

```
/ai-dlc:create-prd → /ai-dlc:create-architecture → /ai-dlc:create-stories → /ai-dlc:plan
     │                        │                            │
     └── PRD 文書              └── Architecture 文書         └── Atomic Spec スタブ群
```

軽量プロジェクト（1-5 Story）は `/ai-dlc:quick-spec` で単一文書に集約可能。

### BMAD 対応

| AI-DLC コマンド | BMAD 相当 |
|---|---|
| `/ai-dlc:create-prd` | `/create-prd` |
| `/ai-dlc:create-architecture` | `/create-architecture` |
| `/ai-dlc:create-stories` | `/create-epics-and-stories` |
| `/ai-dlc:translate-upstream` | (変換コマンド) |

`_bmad/` 検出時は情報バナーを表示。自動切替はしない。

## 下流コマンド（Downstream）

スプリント運営・セレモニーの実行。

| コマンド | 用途 | 対応セレモニー |
|---|---|---|
| `/ai-dlc:plan` | 3 フェーズスプリント計画（AI 事前分析 + 人間判断 + 配信） | 3-Phase Planning |
| `/ai-dlc:digest` | 2 層デイリーダイジェスト（AI ダイジェスト + 判断アジェンダ） | Async Digest |
| `/ai-dlc:verify` | スプリント成果検証（トレーサビリティ + Outcome Done ギャップ） | Verification Session |
| `/ai-dlc:diagnose` | データ駆動型レトロ診断（Before/During/After） | Diagnostic Session |
| `/ai-dlc:calibrate` | エージェント設定・パフォーマンス分析 | Agent Calibration |

## 使い分け: `/ai-dlc:*` vs `/team:*`

| 観点 | `/ai-dlc:*` | `/team:*` |
|---|---|---|
| 設計思想 | AI-人間協働チーム向け | 汎用チーム運営向け |
| Spec 品質チェック | Atomic Spec 5 要素を検証 | なし |
| AI 固有メトリクス | Churn, AI-Confidence, 4h ルール | なし |
| Output/Outcome Done | ギャップ分析あり | なし |
| Agent ガバナンス | CLAUDE.md/SKILL/Hooks 分析 | なし |

## 典型的なワークフロー（エンドツーエンド）

```
Upstream:      /ai-dlc:quick-spec     → 軽量スペック（小規模）
               /ai-dlc:create-prd     → PRD 作成
               /ai-dlc:create-architecture → アーキテクチャ設計
               /ai-dlc:create-stories → Story 分解

Sprint Start:  /ai-dlc:plan           → 3フェーズ計画
Daily:         /ai-dlc:digest         → 非同期ダイジェスト
PR Review:     /ai-dlc:review         → 品質ゲートレビュー
Sprint End:    /ai-dlc:verify         → 成果検証
               /ai-dlc:diagnose       → レトロ用診断
Bi-weekly:     /ai-dlc:calibrate      → エージェント調整
```
