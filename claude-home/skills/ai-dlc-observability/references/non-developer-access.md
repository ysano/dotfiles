# 非開発者向けメトリクスアクセス

AI-DLC メトリクスを非開発者（Value Orchestrator, AI Strategy Architect, ステークホルダー）に提供するチャネル設計。

## ペルソナ × 必要メトリクス

| ペルソナ | 旧称 | 主要関心 | 必要メトリクス |
|---|---|---|---|
| Value Orchestrator | PO | 価値デリバリー頻度 | VDF, Sprint Health, Spec Coverage |
| Agent Orchestration Coach | SM | フロー効率 | SVLT, Rework Rate, AI-Confidence |
| AI Strategy Architect | PM | 投資対効果 | Token Efficiency, Cost/PR, MTTV |
| Delivery System Operator | PjM | デリバリー安定性 | TTC, Sprint Health, Alerts |
| ステークホルダー | Exec | ROI サマリ | Sprint Health, VDF, Economics |

## 3 チャネル

### 1. GitHub Projects V2

**対象**: Value Orchestrator, Coach

GitHub Projects のカスタムフィールドに Sprint Health を反映:

```bash
# Sprint Health をプロジェクトフィールドに設定
gh project field-create <PROJECT_NUMBER> \
  --owner <OWNER> \
  --name "Sprint Health" \
  --data-type NUMBER

# Iteration フィールドと組み合わせてスプリント毎の推移を可視化
```

利点: 既存の GitHub ワークフローに統合。追加ツール不要。
制約: リアルタイム性は低い（コマンド実行時に更新）。

### 2. Markdown レポート

**対象**: 全ペルソナ

`/ai-dlc:digest` が生成する Markdown レポートにメトリクスセクションを追加:

```markdown
## Sprint Metrics Summary
| Metric | Value | Level | Trend |
|---|---|---|---|
| VDF | 1.5/day | HIGH | +0.3 |
| Sprint Health | 0.72 | ATTENTION | -0.05 |
| AI-Confidence | 0.78 | HIGH | stable |
```

配信方法:
- GitHub Issue にコメント投稿
- Slack Webhook で通知
- メール送信（CI/CD パイプライン経由）

### 3. Grafana ダッシュボード

**対象**: Delivery System Operator, ステークホルダー

OTel 有効環境で Prometheus → Grafana のダッシュボードを構築:

```
Prometheus データソース:
- claude_code_api_request_cost_usd (Counter)
- claude_code_api_request_tokens (Histogram)
- claude_code_session_duration_seconds (Histogram)

Grafana パネル:
- Daily Cost Trend (折れ線)
- Token Usage by Model (積み上げ棒)
- Sprint Health Timeline (ゲージ + 折れ線)
```

設定手順概要:
1. `CLAUDE_CODE_ENABLE_TELEMETRY=1` を環境変数に設定
2. OTel Collector を構成 (`otel-collector-config.yaml`)
3. Prometheus をスクレイピング先に追加
4. Grafana でダッシュボード作成

### チャネル選択ガイド

| 条件 | 推奨チャネル |
|---|---|
| Solo 開発 | Markdown レポート (最小構成) |
| Pod (2-10名) | GitHub Projects V2 + Markdown |
| Squad (10-30名) | GitHub Projects V2 + Grafana |
| Enterprise | 全チャネル |

## Anthropic ROI Guide との関係

Anthropic の公式 ROI ガイドでは以下のメトリクスが推奨されている:

- **Productivity**: Lines of Code, PR Throughput → VDF で代替
- **Quality**: Bug Rate, Code Review Time → Rework Rate + TTC で代替
- **Speed**: Time to First Commit, Cycle Time → SVLT + MTTV で代替

AI-DLC Observability は Anthropic ROI Guide の推奨メトリクスを包含しつつ、
Spec 駆動開発に特化した AI-Confidence と Sprint Health を追加する。
