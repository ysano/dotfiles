# プロジェクトヘルスチェック

ベロシティ、品質、チームパフォーマンスのメトリクスでプロジェクトの健全性を評価し、リスクと改善機会を特定します。

## 実行手順

1. **ヘルスチェックの初期化**
   - ツール接続の検証（Linear、GitHub）
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

#### Linear/タスク管理ヘルス
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

3. **Health Report Generation**

```markdown
# Project Health Report - [Project Name]
Generated: [Date]

## Executive Summary
Overall Health Score: [Score]/100 [🟢 Healthy | 🟡 Needs Attention | 🔴 Critical]

### Key Findings
- ✅ Strengths: [Top 3 positive indicators]
- ⚠️ Concerns: [Top 3 areas needing attention]
- 🚨 Critical Issues: [Immediate action items]

## Detailed Health Metrics

1. **Delivery Health** (Score: [X]/100)
| Metric | Current | Target | Status |
|--------|---------|--------|--------|
| Sprint Velocity | [X] pts | [Y] pts | 🟢 |
| On-time Delivery | [X]% | 90% | 🟡 |
| Cycle Time | [X] days | [Y] days | 🟢 |
| Defect Rate | [X]% | <5% | 🔴 |

2. **Code Quality** (Score: [X]/100)
| Metric | Current | Target | Status |
|--------|---------|--------|--------|
| Test Coverage | [X]% | 80% | 🟡 |
| Code Duplication | [X]% | <3% | 🟢 |
| Complexity Score | [X] | <10 | 🟡 |
| Security Issues | [X] | 0 | 🔴 |

3. **Technical Debt** (Score: [X]/100)
- 📊 Total Debt Items: [Count]
- 📈 Debt Growth Rate: [+/-X% per sprint]
- ⏱️ Estimated Debt Work: [X days]
- 💰 Debt Impact: [Description]

4. **Team Health** (Score: [X]/100)
| Metric | Current | Target | Status |
|--------|---------|--------|--------|
| PR Review Time | [X] hrs | <4 hrs | 🟢 |
| Knowledge Silos | [X] | 0 | 🟡 |
| Work Balance | [Score] | >0.8 | 🟢 |
| Burnout Risk | [Level] | Low | 🟡 |

5. **Dependency Health** (Score: [X]/100)
- 🔄 Outdated Dependencies: [X]/[Total]
- 🛡️ Security Vulnerabilities: [Critical: X, High: Y]
- 📜 License Issues: [Count]
- 🔗 External Service Health: [Status]

## Trend Analysis

### Velocity Trend (Last 6 Sprints)
```
Sprint 1: ████████████ 40 pts
Sprint 2: ██████████████ 45 pts
Sprint 3: ████████████████ 50 pts
Sprint 4: ██████████████ 45 pts
Sprint 5: ████████████ 38 pts
Sprint 6: ██████████ 35 pts ⚠️ Declining
```

### Bug Discovery Rate
```
Week 1: ██ 2 bugs
Week 2: ████ 4 bugs
Week 3: ██████ 6 bugs ⚠️ Increasing
Week 4: ████████ 8 bugs 🚨 Action needed
```

## Risk Assessment

### High Priority Risks
1. **Declining Velocity** 
   - Impact: High
   - Likelihood: Confirmed
   - Mitigation: Review sprint planning process

2. **Security Vulnerabilities**
   - Impact: Critical
   - Count: [X] high, [Y] medium
   - Action: Immediate patching required

3. **Knowledge Concentration**
   - Impact: Medium
   - Bus Factor: 2
   - Action: Implement pairing/documentation

## Actionable Recommendations

### Immediate Actions (This Week)
1. 🛡️ **Security**: Update [package] to fix critical vulnerability
2. 🐛 **Quality**: Address top 3 bug-prone modules
3. 👥 **Team**: Schedule knowledge transfer for [critical component]

### Short-term Improvements (This Sprint)
1. 📈 **Velocity**: Reduce scope to sustainable level
2. 🧪 **Testing**: Increase coverage in [module] to 80%
3. 📚 **Documentation**: Update outdated docs for [feature]

### Long-term Initiatives (This Quarter)
1. 🏗️ **Architecture**: Refactor [component] to reduce complexity
2. 🔄 **Process**: Implement automated dependency updates
3. 📊 **Metrics**: Set up continuous health monitoring

## Comparison with Previous Health Check

| Category | Last Check | Current | Trend |
|----------|------------|---------|-------|
| Overall Score | 72/100 | 68/100 | ↓ -4 |
| Delivery | 80/100 | 75/100 | ↓ -5 |
| Code Quality | 70/100 | 72/100 | ↑ +2 |
| Technical Debt | 65/100 | 60/100 | ↓ -5 |
| Team Health | 75/100 | 70/100 | ↓ -5 |
```

4. **Interactive Deep Dives**

Offer focused analysis options:

```
"Based on the health check, would you like to:
1. Deep dive into declining velocity trends
2. Generate security vulnerability fix plan
3. Analyze technical debt hotspots
4. Create team workload rebalancing plan
5. Set up automated health monitoring"
```

## Error Handling

### Missing Linear Connection
```
"Linear MCP not connected. Health check will be limited to:
- Git/GitHub metrics only
- No sprint velocity or task metrics
- Manual input required for team data

To enable full health analysis:
1. Install Linear MCP server
2. Configure with API credentials
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
4. **Action Items** (Linear tasks/GitHub issues)
5. **Monitoring Dashboard** (Grafana/Datadog format)

## Automation Suggestions

```
"Would you like me to:
1. Schedule weekly health checks
2. Set up alerts for critical metrics
3. Create Linear tasks for action items
4. Generate PR templates with health criteria
5. Configure CI/CD health gates"
```

## Best Practices

1. **Regular Cadence**: Run health checks weekly/bi-weekly
2. **Track Trends**: Compare with historical data
3. **Action-Oriented**: Focus on fixable issues
4. **Team Involvement**: Share results transparently
5. **Continuous Improvement**: Refine metrics based on outcomes