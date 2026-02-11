Verification Session: AI-DLC のスプリントレビュー・検証モデル。

## Why

従来のスプリントレビューは「デモ会」だった。AI-DLC では成果物の量が爆発的に増え、「本当に正しいか」の検証が最大の課題になる。
レビューの中心テーマは「何を作ったか」から「**AI が作ったものは正しいか**」にシフトする。
Output Done（動くソフトウェア）だけでなく Outcome Done（ビジネスインパクト）の検査が最重要アジェンダ。

## What: 3 つの検証焦点

### 1. AI 生成物の品質検証

| 検証項目 | 方法 | 判定基準 |
|---|---|---|
| PR → Story トレーサビリティ | マージ済み PR と Issue の紐付け | 全 PR が Issue に紐付いている |
| テストカバレッジ差分 | スプリント前後のカバレッジ比較 | カバレッジが維持 or 向上 |
| コードレビュー統計 | reject 率、修正ラウンド数 | reject 率が前スプリント以下 |
| AI-Confidence 分布 | スコア帯別の集計（利用可能な場合） | 80 未満の比率が前スプリント以下 |

### 2. Output Done vs Outcome Done ギャップ分析

| レベル | 定義 | 検査方法 |
|---|---|---|
| **Output Done** | 動くソフトウェアの品質基準（テスト通過、セキュリティスキャン、レビュー完了） | CI/CD + レビュー記録 |
| **Outcome Done** | ビジネスインパクトの基準（ユーザー行動・体験・結果の改善証拠） | ユーザーデータ、A/B テスト結果 |

ギャップ = Output Done は達成しているが Outcome Done が未確認の成果物。
AI が生産量を増大させる環境では、このギャップの可視化が最重要。

### 3. レビュー頻度

| スケール | 頻度 | 形式 |
|---|---|---|
| Pod (1-3日) | 日次軽量デモ（15 分） + 週次正式レビュー | AI 生成デモ資料 |
| Squad (3-5日) | 週次正式レビュー | ステークホルダー参加 |
| Enterprise (1週間) | 週次 + 月次ストラテジックレビュー | プロダクト戦略レベル |

## How (for Claude Code)

### `/ai-dlc:verify` が実行するステップ

**データ収集**:

```bash
# マージ済み PR（スプリント期間）
gh pr list --state merged --json number,title,body,mergedAt,reviews,additions,deletions --limit 100

# 完了 Issue
gh issue list --state closed --json number,title,closedAt,labels,body --limit 100

# PR → Issue トレーサビリティ（PR body から Issue 番号を抽出）
gh pr list --state merged --json number,title,body --limit 100

# レビュー統計
gh pr list --state merged --json number,reviews --limit 100
```

**検証レポートテンプレート**:

```markdown
## Sprint Verification Report

### Deliverables Summary
- PRs merged: [N] (+[additions] / -[deletions] lines)
- Issues completed: [N]
- Sprint goal achievement: [assessment]

### Traceability
| PR | Linked Issue | Status |
|---|---|---|
| #[N] | #[N] | [linked / orphan] |

Orphan PRs (no linked issue): [count]

### Quality Assessment
- Avg review rounds: [N]
- PRs with rejection: [N]/[total] ([%])
- Test coverage delta: [+/-N%]

### Output Done vs Outcome Done Gap
| Issue | Output Done | Outcome Done | Gap |
|---|---|---|---|
| #[N] | [yes/no] | [verified/unverified/N/A] | [description] |

Items with Output Done but unverified Outcome: [count]

### Recommendations
- [specific recommendation based on data]
```

## Cross-ref

- DoD レベル（最小/標準/厳格） → `ticket-management` > `references/ai-native-practices.md`
- AI-Confidence / MTTV → `ticket-management` > `references/ai-native-practices.md`
- 品質検証の詳細計画 → `/ai-dlc:plan` Phase 2（検証時間の確保）
