---
name: ai-dlc-ceremonies
description: >
  AI-DLC ceremony patterns and role context.
  Use when facilitating AI-native sprint planning, generating async digests,
  running diagnostic sessions, verifying sprint outcomes,
  or conducting agent calibration.
  ai-dlc commands reference this for ceremony-specific knowledge.
user-invocable: false
---

AI-DLC (AI Development Lifecycle) セレモニー知識ベース。
従来のスクラムイベントを AI-人間協働向けに再定義したパターン集。
チケットライフサイクル（Atomic Spec, Agent Loop, DoD）は `ticket-management` Skill を参照。

## セレモニー一覧

| セレモニー | 目的 | 参照 | 対応コマンド |
|---|---|---|---|
| **3-Phase Planning** | AI 事前分析 + 人間の戦略判断 + 配信準備 | `references/planning-phases.md` | `/ai-dlc:plan` |
| **Async Digest** | 2層デイリー: AI ダイジェスト + 判断アジェンダ | `references/async-digest.md` | `/ai-dlc:digest` |
| **Diagnostic Session** | データ駆動型レトロ: Before/During/After | `references/diagnostic-session.md` | `/ai-dlc:diagnose` |
| **Verification Session** | AI 出力検証 + Outcome Done ギャップ分析 | `references/verification-session.md` | `/ai-dlc:verify` |
| **Agent Calibration** | CLAUDE.md/SKILL/Hooks の定期検査・調整 | `references/agent-calibration.md` | `/ai-dlc:calibrate` |
| **Role Transformation** | 4役割の変革詳細（PO/SM/PM/PjM） | `references/role-transformation.md` | - |
| **Adoption Playbook** | AI-DLC 導入 5 ステップ | `references/adoption-playbook.md` | - |

## スケール別頻度

| チームスケール | スプリント長 | Planning | Digest | Diagnostic | Verification | Calibration |
|---|---|---|---|---|---|---|
| **Pod (1-10名)** | 1-3日 | スプリント毎 | 毎日 | スプリント毎 | 日次軽量デモ + 週次正式 | 2週に1回 |
| **Squad (10-30名)** | 3-5日 | スプリント毎 | 毎日 | スプリント毎 | 週次正式 | 2週に1回 |
| **Enterprise (30名以上)** | 1週間 | スプリント毎 | 毎日 | スプリント毎 | 週次 + 月次戦略 | 2週に1回 |

## 5 設計原則

1. **検証シフト**: 全イベントの重心を「作業の計画・報告」から「AI 出力の検証・承認」へ
2. **非同期ファースト**: AI が自動化できるデータ収集・分析は非同期化。同期時間は人間にしかできない判断に集中
3. **データ駆動**: 全イベントで AI が事前にデータを準備。「意見ではなくデータで始める」
4. **アウトカム志向**: 「どれだけ作ったか」ではなく「どれだけ価値を届けたか」を評価軸とする
5. **エージェントガバナンス**: AI エージェントの動作品質を検査・改善する仕組みを正式なイベントとして組み込む

## 役割変化

| 従来の役割 | AI-DLC での新たな焦点 | 新呼称 |
|---|---|---|
| **Product Owner** | 価値仮説の設計・検証、AI へのコンテキスト提供、Outcome Done 定義 | Value Orchestrator |
| **Scrum Master** | AI-人間協働パターン設計、エージェントガバナンス、フロー最適化 | Agent Orchestration Coach |
| **Product Manager** | AI-Capability 思考による戦略設計、プロトタイプ駆動の検証 | AI Strategy Architect |
| **Project Manager** | AI システムのオペレーション監視、デリバリーパイプライン最適化 | Delivery System Operator |

## AI-DLC Hooks

| Hook | イベント | 目的 |
|---|---|---|
| `check-spec-existence.py` | PreToolUse (Write/Edit/MultiEdit) | コード変更前に Spec/Plan の存在確認 |
| `auto-update-ticket.sh` | PostToolUse (Bash) | git push 後にチケットにコミット自動記録 |

設定プリセット: `hooks/settings-ai-dlc.json`

## ticket-management とのクロスリファレンス

本スキルはセレモニー運営パターンに特化。以下は `ticket-management` Skill を参照:

| トピック | 参照先 |
|---|---|
| Atomic Spec 5要素テンプレート | `ticket-management` > `references/atomic-spec.md` |
| Agent Loop 7段階ライフサイクル | `ticket-management` > `references/agent-loop.md` |
| AI-Native DoD / カスタムフィールド | `ticket-management` > `references/ai-native-practices.md` |
| 組織トポロジー / ツール選択 | `ticket-management` > `references/organization-topology.md` |

## 上流パイプラインとの接続

セレモニー実行の前段階として、上流ワークフローが成果物を準備する。
上流パイプライン（PRD → Architecture → Stories）の詳細は `ai-dlc-upstream` Skill を参照。
上流コマンド: `/ai-dlc:create-prd`, `/ai-dlc:create-architecture`, `/ai-dlc:create-stories`, `/ai-dlc:quick-spec`
