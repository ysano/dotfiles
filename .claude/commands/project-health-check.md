# プロジェクトヘルスチェック

ベロシティ、品質、チームパフォーマンスのメトリクスでプロジェクトの健全性を評価し、リスクと改善機会を特定します。

## 実行手順

1. **ヘルスチェックの初期化**
   - ツール接続の検証（GitHub）
   - 評価期間の定義（デフォルト：過去30日）
   - ヘルスチェック基準と闾値の設定
   - 評価すべき主要メトリクスの特定

2. **多元的分析**

#### コードヘルスメトリクス
```bash
# Code churn analysis
git log --format=format: --name-only --since="30 days ago" | sort | uniq -c | sort -rg

# Contributor activity
git shortlog -sn --since="30 days ago"

# Branch health
git for-each-ref --format='%(refname:short) %(committerdate:relative)' refs/heads/ | grep -E "(months|years) ago"

# File complexity (if cloc available)
cloc . --json --exclude-dir=node_modules,dist,build

# Test coverage trends
npm test -- --coverage --json
```

#### 依存関係ヘルス
```bash
# 古い依存関係のチェック
npm outdated --json

# セキュリティ脆弱性
npm audit --json

# ライセンスコンプライアンス
npx license-checker --json
```

#### GitHub Projects/タスク管理ヘルス
```
1. スプリントベロシティトレンド
2. サイクルタイム分析
3. ブロックされたタスクの期間
4. バックログの成長率
5. バグ対機能の比率
6. タスク完了の予測可能性
```

#### チームヘルス指標
```
1. PRレビューのターンアラウンドタイム
2. コミット頻度の分布
3. 作業分担のバランス
4. オンコールインシデント頻度
5. ドキュメント更新
```

3. **ヘルスレポート生成**

```markdown
# プロジェクトヘルスレポート - [プロジェクト名]
生成日: [日付]

## エグゼクティブサマリー
総合ヘルススコア: [スコア]/100 [🟢 健全 | 🟡 要注意 | 🔴 危険]

### 主要な発見事項
- ✅ 強み: [上位3つのポジティブ指標]
- ⚠️ 懸念事項: [注意が必要な上位3つの領域]
- 🚨 重要な問題: [即座に対処すべき項目]

## 詳細ヘルスメトリクス

1. **デリバリーヘルス** (スコア: [X]/100)
| メトリクス | 現在値 | 目標値 | ステータス |
|--------|---------|--------|--------|
| スプリントベロシティ | [X] pts | [Y] pts | 🟢 |
| 期限内デリバリー | [X]% | 90% | 🟡 |
| サイクルタイム | [X] 日 | [Y] 日 | 🟢 |
| 欠陥率 | [X]% | <5% | 🔴 |

2. **コード品質** (スコア: [X]/100)
| メトリクス | 現在値 | 目標値 | ステータス |
|--------|---------|--------|--------|
| テストカバレッジ | [X]% | 80% | 🟡 |
| コード重複 | [X]% | <3% | 🟢 |
| 複雑度スコア | [X] | <10 | 🟡 |
| セキュリティ問題 | [X] | 0 | 🔴 |

3. **技術的負債** (スコア: [X]/100)
- 📊 総負債項目: [カウント]
- 📈 負債成長率: [+/-X% per スプリント]
- ⏱️ 推定負債作業: [X日]
- 💰 負債影響: [説明]

4. **チームヘルス** (スコア: [X]/100)
| メトリクス | 現在値 | 目標値 | ステータス |
|--------|---------|--------|--------|
| PRレビュー時間 | [X] 時間 | <4 時間 | 🟢 |
| 知識のサイロ化 | [X] | 0 | 🟡 |
| 作業バランス | [スコア] | >0.8 | 🟢 |
| バーンアウトリスク | [レベル] | 低 | 🟡 |

5. **依存関係ヘルス** (スコア: [X]/100)
- 🔄 古い依存関係: [X]/[総計]
- 🛡️ セキュリティ脆弱性: [重要: X, 高: Y]
- 📜 ライセンス問題: [カウント]
- 🔗 外部サービスヘルス: [ステータス]

## トレンド分析

### ベロシティトレンド（過去6スプリント）
```
スプリント1: ████████████ 40 pts
スプリント2: ██████████████ 45 pts
スプリント3: ████████████████ 50 pts
スプリント4: ██████████████ 45 pts
スプリント5: ████████████ 38 pts
スプリント6: ██████████ 35 pts ⚠️ 減少
```

### バグ発見率
```
週1: ██ 2 バグ
週2: ████ 4 バグ
週3: ██████ 6 バグ ⚠️ 増加
週4: ████████ 8 バグ 🚨 対策必要
```

## リスク評価

### 高優先度リスク
1. **ベロシティの低下** 
   - 影響: 高
   - 可能性: 確認済み
   - 軽減策: スプリント計画プロセスの見直し

2. **セキュリティ脆弱性**
   - 影響: 重要
   - カウント: [X] 高, [Y] 中
   - 対策: 即座のパッチ適用が必要

3. **知識の集中**
   - 影響: 中
   - バスファクター: 2
   - 対策: ペアリング/ドキュメント化の実装

## 実行可能な推奨事項

### 即座の対策（今週）
1. 🛡️ **セキュリティ**: 重要な脆弱性を修正するため[パッケージ]を更新
2. 🐛 **品質**: バグが多発する上位3モジュールに対処
3. 👥 **チーム**: [重要コンポーネント]の知識移転をスケジュール

### 短期改善（今スプリント）
1. 📈 **ベロシティ**: 持続可能なレベルまでスコープを縮小
2. 🧪 **テスト**: [モジュール]のカバレッジを80%まで向上
3. 📚 **ドキュメント**: [機能]の古いドキュメントを更新

### 長期イニシアチブ（今四半期）
1. 🏗️ **アーキテクチャ**: 複雑度を下げるため[コンポーネント]をリファクタ
2. 🔄 **プロセス**: 自動依存関係更新を実装
3. 📊 **メトリクス**: 継続的ヘルス監視を設定

## 前回ヘルスチェックとの比較

| カテゴリ | 前回チェック | 現在 | トレンド |
|----------|------------|---------|-------|
| 総合スコア | 72/100 | 68/100 | ↓ -4 |
| デリバリー | 80/100 | 75/100 | ↓ -5 |
| コード品質 | 70/100 | 72/100 | ↑ +2 |
| 技術的負債 | 65/100 | 60/100 | ↓ -5 |
| チームヘルス | 75/100 | 70/100 | ↓ -5 |
```

4. **インタラクティブ詳細分析**

焦点を絞った分析オプションを提供:

```
"ヘルスチェックに基づいて、以下のどれを実行しますか:
1. ベロシティ低下トレンドの詳細分析
2. セキュリティ脆弱性修正計画の生成
3. 技術的負債ホットスポットの分析
4. チーム作業負荷リバランス計画の作成
5. 自動ヘルス監視の設定"
```

## Error Handling

### Missing GitHub Projects Access
```
"GitHub Projects access not available. Health check will be limited to:
- Git repository metrics only
- Basic GitHub Issues data
- Manual input required for project data

To enable full health analysis:
1. Ensure GitHub Projects V2 access
2. Configure proper API permissions
3. Re-run health check"
```

### Incomplete Data
```
"Some metrics could not be calculated:
- [List missing metrics]
- [Explain impact on analysis]

Would you like to:
1. Proceed with available data
2. Manually provide missing information
3. Skip incomplete sections"
```

## Customization Options

### Threshold Configuration
```yaml
# health-check-config.yml
thresholds:
  velocity_variance: 20  # Acceptable % variance
  test_coverage: 80      # Minimum coverage %
  pr_review_time: 4      # Maximum hours
  bug_rate: 5           # Maximum % of work
  dependency_age: 90    # Days before "outdated"
```

### Custom Health Metrics
Allow users to define additional metrics:
```
"Add custom health metric:
- Name: Customer Satisfaction
- Data Source: [API/Manual/File]
- Target Value: [>4.5/5]
- Weight: [Impact on overall score]"
```

## Export Options

1. **Executive Summary** (PDF/Markdown)
2. **Detailed Report** (HTML with charts)
3. **Raw Metrics** (JSON/CSV)
4. **Action Items** (GitHub issues)
5. **Monitoring Dashboard** (Grafana/Datadog format)

## Automation Suggestions

```
"Would you like me to:
1. Schedule weekly health checks
2. Set up alerts for critical metrics
3. Create GitHub issues for action items
4. Generate PR templates with health criteria
5. Configure CI/CD health gates"
```

## Best Practices

1. **Regular Cadence**: Run health checks weekly/bi-weekly
2. **Track Trends**: Compare with historical data
3. **Action-Oriented**: Focus on fixable issues
4. **Team Involvement**: Share results transparently
5. **Continuous Improvement**: Refine metrics based on outcomes