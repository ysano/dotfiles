---
description: "AI-DLC Commands"
---

AI-DLC (AI Development Lifecycle) コマンド群。
AI-人間協働チーム向けのセレモニー運営を支援する。

知識ベースは `ai-dlc-ceremonies` Skill、チケットライフサイクルは `ticket-management` Skill を参照。
既存の `/team:*` コマンドは汎用的なチーム運営向け。AI-DLC コマンドは AI-Native なセレモニーパターンに特化。

## コマンド一覧

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

## 典型的なワークフロー

```
Sprint Start:  /ai-dlc:plan          → 3フェーズ計画
Daily:         /ai-dlc:digest        → 非同期ダイジェスト
Sprint End:    /ai-dlc:verify        → 成果検証
               /ai-dlc:diagnose      → レトロ用診断
Bi-weekly:     /ai-dlc:calibrate     → エージェント調整
```
