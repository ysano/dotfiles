# メトリクスレイヤリングアーキテクチャ

AI-DLC Observability の 4 層構造。各レイヤーの責務・ストレージ・消費者を定義する。

## アーキテクチャ概要

```
┌─────────────────────────────────────────────────┐
│ L4 Trend        sprints.jsonl 履歴               │
│                 /ai-dlc:calibrate                │
│                 月次・四半期トレンド分析           │
├─────────────────────────────────────────────────┤
│ L3 Sprint       sprints.jsonl (集計結果)          │
│                 aggregate-sprint.py              │
│                 status / diagnose / verify       │
├─────────────────────────────────────────────────┤
│ L2 Session      sessions.jsonl (G5)              │
│                 churn cache (G3)                  │
│                 spec cache (G4)                   │
│                 Hook 自動収集 / digest 直接参照    │
├─────────────────────────────────────────────────┤
│ L1 Real-time    OTel Collector                   │
│                 claude_code.api_request events    │
│                 Prometheus / Grafana              │
└─────────────────────────────────────────────────┘
```

## L1: Real-time (OTel)

| 項目 | 詳細 |
|---|---|
| 責務 | API リクエスト毎のテレメトリ収集 |
| ストレージ | OTel Collector → Prometheus |
| 消費者 | Grafana ダッシュボード |
| 頻度 | リアルタイム |
| 有効化 | `CLAUDE_CODE_ENABLE_TELEMETRY=1` |

収集データ: `cost_usd`, `input_tokens`, `output_tokens`, `model`, `duration_ms`

Solo 開発では通常 OTel 未設定。L2-L3 で十分な可視性を確保する設計。

## L2: Session (Hook 自動収集)

| 項目 | 詳細 |
|---|---|
| 責務 | セッション毎のメトリクス自動収集 |
| ストレージ | ローカルファイル (下表) |
| 消費者 | L3 集計スクリプト, `/ai-dlc:digest` (直接読み取り) |
| 頻度 | セッション終了時 / ファイル編集時 |

| ファイル | Hook | 内容 |
|---|---|---|
| `~/.claude/metrics/sessions.jsonl` | metrics-collector.py (G5) | turns, tools, modified_files, timestamps |
| `/tmp/claude-churn-cache/{project}_churn.json` | churn-counter.py (G3) | ファイル別修正回数 |
| `/tmp/claude-spec-quality-cache/{hash}_scores.json` | check-spec-existence.py (G4) | Spec 品質 0-5 スコア |

## L3: Sprint (CLI 集計)

| 項目 | 詳細 |
|---|---|
| 責務 | Sprint 粒度の複合メトリクス算出 |
| ストレージ | `~/.claude/metrics/sprints.jsonl` |
| 消費者 | `/ai-dlc:status`, `/ai-dlc:diagnose`, `/ai-dlc:verify` |
| 頻度 | コマンド実行時 (on-demand) |

集計スクリプト: `scripts/aggregate-sprint.py`

### sprints.jsonl フォーマット

1 行 = 1 Sprint 集計結果 (JSON)。主要フィールド:

```json
{
  "sprint_id": "2026-02-10_2026-02-14",
  "since": "2026-02-10",
  "until": "2026-02-14",
  "project_dir": "/home/user/project",
  "computed_at": "2026-02-14T12:00:00Z",
  "dora": { "vdf": {}, "svlt": {}, "rework_rate": {}, "ttc": {} },
  "ai_dlc": { "ai_confidence": {}, "mttv_macro_hours": null, "sprint_health": {} },
  "activity": { "session_count": 0, "total_turns": 0 },
  "economics": { "available": false },
  "alerts": []
}
```

## L4: Trend (長期分析)

| 項目 | 詳細 |
|---|---|
| 責務 | 長期トレンド分析・回帰検出 |
| ストレージ | sprints.jsonl の履歴（全エントリ） |
| 消費者 | `/ai-dlc:calibrate` |
| 頻度 | 月次 / 四半期 |

L4 は sprints.jsonl を時系列で分析し、改善・悪化トレンドを検出する。
Phase 1 では L4 の自動化は対象外（手動の `/ai-dlc:calibrate` で対応）。

## データフロー

```
[Claude Code Session]
    │
    ├── G3 churn-counter.py ──→ /tmp/claude-churn-cache/
    │                                    │
    │                              /ai-dlc:digest (直接)
    │                                    │
    ├── G4 check-spec-existence.py ──→ /tmp/claude-spec-quality-cache/
    │                                    │
    └── G5 metrics-collector.py ──→ sessions.jsonl
                                         │
                                   /ai-dlc:digest (直接)
                                         │
                                         ▼
                               aggregate-sprint.py + gh CLI
                                         │
                                         ▼
                                   sprints.jsonl
                                    │         │
                              L3 消費者    L4 消費者
                              status      calibrate
                              diagnose    (履歴比較)
                              verify
```

## sessions.jsonl ローテーション方針

- 30 日超のエントリは集計スクリプトのフィルタ対象外
- 自動削除は行わない（手動またはスケジュールタスクで対応）
- ローテーション時は `sprints.jsonl` に集計結果が残るため L3 以上のデータは保全
