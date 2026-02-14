---
name: ai-dlc-observability
description: >
  AI-DLC observability metrics and DORA Four Keys integration.
  Use when computing sprint health, AI effectiveness, or project metrics.
  Referenced by /ai-dlc:status, /ai-dlc:diagnose, /ai-dlc:digest.
user-invocable: false
---

AI-DLC Observability フレームワーク。DORA Four Keys と AI-DLC 固有メトリクスを統合し、スプリント健全性を定量的に評価する知識ベース。

## メトリクス一覧

### DORA Four Keys (AI-DLC 再定義)

| 名前 | 種別 | 算出式概要 | データソース | 参照先 |
|---|---|---|---|---|
| VDF (Value Delivery Frequency) | Throughput | Spec 適合 PR 数 / sprint_days | GitHub CLI (`gh pr list`) | `references/dora-four-keys.md` |
| SVLT (Spec-to-Value Lead Time) | Speed | PR.mergedAt - Issue.createdAt | GitHub CLI | `references/dora-four-keys.md` |
| Rework Rate | Stability | high_churn_files / total_modified_files | churn cache (G3) | `references/dora-four-keys.md` |
| TTC (Time to Correct) | Recovery | fix_PR.mergedAt - bug_issue.createdAt | GitHub CLI | `references/dora-four-keys.md` |

### AI-DLC 固有メトリクス

| 名前 | 種別 | 算出式概要 | データソース | 参照先 |
|---|---|---|---|---|
| AI-Confidence | 品質 | SQ*0.30 + CI*0.25 + TPR*0.25 + SE*0.20 | spec cache (G4) + churn cache (G3) + sessions.jsonl (G5) | `references/ai-dlc-metrics.md` |
| MTTV Macro | 速度 | PR merge cycle 中央値 | GitHub CLI | `references/ai-dlc-metrics.md` |
| MTTV Micro | 速度 | active_time / turns | sessions.jsonl (G5) | `references/ai-dlc-metrics.md` |
| Token Efficiency | 経済性 | Tokens/Commit, Cost/PR | OTel Events | `references/ai-dlc-metrics.md` |
| Sprint Health Score | 総合 | 6要素均等加重 (DORA 4 + AI-Confidence + Spec Coverage) | 複合 | `references/ai-dlc-metrics.md` |

## 4層レイヤリング

| レイヤー | 責務 | ストレージ | 消費者 |
|---|---|---|---|
| L1 Real-time | API リクエスト毎のテレメトリ | OTel Collector / Prometheus | Grafana ダッシュボード |
| L2 Session | セッション毎の集計 | `~/.claude/metrics/sessions.jsonl` | L3 集計スクリプト |
| L3 Sprint | スプリント粒度の複合メトリクス | `~/.claude/metrics/sprints.jsonl` | `/ai-dlc:status`, `/ai-dlc:diagnose` |
| L4 Trend | 長期トレンド・回帰分析 | sprints.jsonl 履歴 | `/ai-dlc:calibrate` |

詳細: `references/metrics-layering.md`

## データソース

| ソース | レイヤー | 収集方法 | データ |
|---|---|---|---|
| OTel Events | L1 | `CLAUDE_CODE_ENABLE_TELEMETRY=1` | cost_usd, input/output_tokens |
| sessions.jsonl | L2 | `metrics-collector.py` (G5 Stop Hook) | turns, tools, modified_files |
| churn cache | L2 | `churn-counter.py` (G3 PostToolUse Hook) | ファイル別修正回数 |
| spec cache | L2 | `check-spec-existence.py` (G4 PreToolUse Hook) | Spec 品質 0-5 スコア |
| GitHub CLI | L3 | `aggregate-sprint.py` 実行時 | PR/Issue メタデータ |

## 集計スクリプト

Sprint 粒度のメトリクス集計には決定的スクリプトを使用:

```bash
# 基本（直近7日）
python3 .claude/skills/ai-dlc-observability/scripts/aggregate-sprint.py

# 期間指定
python3 .claude/skills/ai-dlc-observability/scripts/aggregate-sprint.py \
  --since 2026-02-10 --until 2026-02-14

# プロジェクト指定
python3 ... --project-dir /home/user/dotfiles
```

## クロスリファレンス

| トピック | 参照先 |
|---|---|
| チケットライフサイクル (Atomic Spec, Agent Loop, DoD) | `ticket-management` Skill |
| セレモニーパターン・役割変化 | `ai-dlc-ceremonies` Skill |
| 見積もり技法 (AEF, Value-Based) | `ai-dlc-estimate` Skill |
| 非開発者向けアクセス | `references/non-developer-access.md` |

<constraints>
- OTel 未設定環境では Economics セクションをスキップ（Solo デフォルト）
- GitHub CLI (`gh`) 未認証環境では DORA セクションを null で続行
- sessions.jsonl は 30 日超のエントリをローテーション対象とする
- Sprint Health Score の閾値は Solo/Pod/Squad で異なる
- コスト情報は Session JSONL に含まれない（OTel 専用）
</constraints>
