# レトロスペクティブアナライザー

GitHub Projects V2、Issues、PR、Actionsのメトリクスを分析し、改善のためのパターンを特定することで、データ駆動の洞察とアクションアイテムでスプリントレトロスペクティブを促進します。

## 実行手順

1. **レトロスペクティブの設定**
   - 分析するスプリント/マイルストーンの特定（デフォルト：最新）
   - GitHub Projects V2、APIアクセスの確認
   - レトロスペクティブ形式の設定
   - 分析時間範囲の設定

2. **スプリントデータ収集**

#### 定量的メトリクス
```
GitHub Projects V2/Issuesから：
- 計画対完了Issues数
- スプリントベロシティとキャパシティ
- Issueサイクルタイムとリードタイム
- バグラベルのIssues数
- 計画外Issueの割合

Git/GitHubから：
- コミット頻度と分布
- PRマージ時間統計
- コードレビューのターンアラウンド
- GitHub Actions成功率
- デプロイ頻度と成功率

GitHub Actions/Workflowsから：
- CI/CDパイプライン実行時間
- テストカバレッジトレンド
- ワークフロー失敗率
- デプロイメント頻度
```

#### 定性データソース
```
1. PRレビューコメントの感情
2. コミットメッセージパターン
3. GitHub Discussions/Issuesコメント
4. 前回のレトロスペクティブアクションアイテム
5. GitHub Issuesのバグレポートトレンド
6. GitHub Actionsログエラーパターン
7. Slack会話（利用可能な場合）
```

3. **自動分析**

#### スプリントパフォーマンス分析
```markdown
# スプリント[名前] レトロスペクティブ分析

## スプリント概要
- 期間: [開始] から [終了]
- チームサイズ: [人数] 名
- スプリント目標: [説明]
- 目標達成度: [達成/部分達成/未達成]

## 主要メトリクス要約

### デリバリーメトリクス
| メトリクス | 目標 | 実績 | 差異 |
|----------|------|------|------|
| ベロシティ | [X] pts | [Y] pts | [+/-Z]% |
| 完了率 | 90% | [X]% | [+/-Y]% |
| 欠陥率 | <5% | [X]% | [+/-Y]% |
| 計画外作業 | <20% | [X]% | [+/-Y]% |

### プロセスメトリクス
| メトリクス | 今回スプリント | 前回 | トレンド |
|----------|--------------|------|--------|
| 平均PRレビュー時間 | [X] 時間 | [Y] 時間 | [↑/↓] |
| 平均サイクルタイム | [X] 日 | [Y] 日 | [↑/↓] |
| CI/CD成功率 | [X]% | [Y]% | [↑/↓] |
| チーム幸福度 | [X]/5 | [Y]/5 | [↑/↓] |
```

#### パターン認識
```markdown
## 特定されたパターン

### ポジティブなパターン 🟢
1. **コードレビュー速度の改善**
   - 平均レビュー時間が30%短縮
   - 新しいレビューガイドラインとの相関
   - 推奨事項: プロセスを文書化し維持

2. **一貫した日々の進捗**
   - スプリント期間を通して均等なコミット分布
   - 最後の駆け込みなし
   - 良好なスプリント計画を示している

### 懸念すべきパターン 🔴
1. **月曜日のデプロイ失敗**
   - 失敗したデプロイの60%が月曜日
   - 可能性のある原因: 週末の変更がテストされていない
   - アクション: 月曜日朝のチェックを実装

2. **スコープクリープの増加**
   - 計画外作業が35%（20%から増加）
   - 原因: 緊急顧客要求
   - アクション: スプリントコミットプロセスを見直し
```

4. **インタラクティブレトロスペクティブ促進**

#### レトロスペクティブ前レポート
```markdown
# レトロスペクティブ前の洞察

## データ駆動ディスカッショントピック

### 1. うまくいったこと 
データに基づくと、これらの領域で改善が見られました:
- ✅ コードレビュー効率 (+30%)
- ✅ テストカバレッジ増加 (+5%)
- ✅ 本番環境での重要なバグゼロ
- ✅ 全チームメンバーが均等に貢献

**推奨ディスカッション質問:**
- レビューを速くした具体的な変更は何ですか?
- 重要なバグゼロを維持するにはどうすればよいですか?
- 作業分散を成功させた要因は何ですか?

### 2. うまくいかなかったこと
データはこれらの領域で課題を示しています:
- ❌ スプリントベロシティの未達成 (-15%)
- ❌ 高い計画外作業 (35%)
- ❌ 3回のロールバックが必要
- ❌ チーム残業時間の増加

**推奨ディスカッション質問:**
- ベロシティ未達成の原因は何でしたか?
- 計画外作業をより良く処理するにはどうすればよいですか?
- ロールバックの原因は何でしたか?

### 3. データからのアクションアイテム
パターンに基づく推奨改善:
1. より安全なデプロイのためのフィーチャーフラグを実装
2. スプリント計画で計画外作業予算を作成
3. [問題領域]の統合テストを追加
4. スプリント中間チェックインをスケジュール
```

#### ライブレトロスペクティブサポート
```
レトロスペクティブ中に、以下でサポートできます:

1. **事実確認**: 
   「実際のベロシティは50ポイントではなく45ポイントでした」

2. **パターンコンテキスト**:
   「月曜日のデプロイ問題があるのは3回目のスプリントです」

3. **履歴比較**:
   「前回似たような問題があった時はXを試しました」

4. **アクションアイテム追跡**:
   「前回のレトロから6個中4個のアクションアイテムを完了しました」
```

5. **レトロスペクティブ出力フォーマット**

#### 標準レトロスペクティブ要約
```markdown
# スプリント[X] レトロスペクティブ要約

## 参加者
[参加者リスト]

## うまくいったこと
- [投票数付きカテゴリ化リスト]
- サポートデータ: [メトリクス]

## うまくいかなかったこと  
- [投票数付きカテゴリ化リスト]
- 根本原因分析: [詳細]

## アクションアイテム
| アクション | 担当者 | 期限 | 成功基準 |
|----------|-------|------|----------|
| [アクション1] | [名前] | [日付] | [測定可能な成果] |
| [アクション2] | [名前] | [日付] | [測定可能な成果] |

## 次スプリントの実験
1. [実験説明]
   - 仮説: [期待すること]
   - 測定: [どう知るか]
   - レビュー日: [いつ評価するか]

## チームヘルスパルス
- エネルギーレベル: [評価]/5
- 明確さ: [評価]/5
- 自信: [評価]/5
- キーコート: "[注目すべきチーム感情]"
```

#### トレンド分析レポート
```markdown
# レトロスペクティブトレンド分析

## 繰り返しテーマ（過去5スプリント）

### 継続的な課題
1. **デプロイメント問題** (5スプリント中4回)
   - 根本原因が未解決
   - エスカレーション推奨

2. **見積もり精度** (5スプリント中5回)
   - 一貫して20%の超過
   - 体系的なアプローチが必要

### 改善中の領域
1. **コミュニケーション** (3スプリント連続改善)
2. **コード品質** (安定した改善)

### 成功パターン
1. **ペアプログラミング** (5回中5回でポジティブに言及)
2. **デイリースタンドアップ** (効果的なフォーマットを発見)
```

6. **アクションアイテム生成**

#### スマートアクションアイテム
```
レトロスペクティブディスカッションに基づく、SMARTアクションアイテム:

1. **デプロイ失敗の削減**
   - 具体的: 月曜日デプロイ用のスモークテストを実装
   - 測定可能: 失敗率<5%
   - 割り当て可能: DevOpsチーム
   - 関連性: 失敗の60%に対処
   - 期限: 次スプリントまで

2. **見積もり改善**
   - 具体的: 全ストーリーでプランニングポーカーを使用
   - 測定可能: 見積もりからの差異<20%
   - 割り当て可能: スクラムマスターが促進
   - 関連性: ベロシティ未達に対処
   - 期限: 次スプリント計画から開始
```

## エラーハンドリング

### GitHub Projectsデータなし
```
"GitHub Projects V2アクセスが制限されています。GitデータとIssuesのみを使用します。

不足している洞察：
- Projects V2のカスタムフィールド分析
- プロジェクトレベルのメトリクス
- チームキャパシティデータ

以下のいずれかを希望しますか：
1. GitとIssuesデータのみで続行
2. スプリントメトリクスを手動入力
3. GitHub Projects V2権限を設定して再試行"
```

### 未完了スプリント
```
"スプリントが進行中のようです。

現在の分析基準：
- 全[Y]日中[X]日
- 作業完了率[Z]%

推奨事項: スプリント終了後に完全分析を実行
部分分析で続行しますか？ [Y/N]"
```

## 高度な機能

### 感情分析
```python
# PRコメントとコミットメッセージを分析
sentiment_indicators = {
    'positive': ['fixed', 'improved', 'resolved', 'great'],
    'negative': ['bug', 'issue', 'broken', 'failed', 'frustrated'],
    'neutral': ['updated', 'changed', 'modified']
}

# 感情レポートを生成
"チーム感情分析:
- ポジティブ指標: 65%
- ネガティブ指標: 25%  
- 中立: 10%

トレンド: 前スプリントから改善（55%ポジティブでした）"
```

### 予測洞察
```
"現在のパターンに基づいて:

⚠️ リスク予測:
- 計画外作業が続く場合のベロシティ未達確率70%
- 介入なしではデプロイ失敗が増加する可能性

💡 機会予測:
- 提案されたプロセス変更で15%のベロシティ向上が可能
- 作業負荷のバランス調整でチーム幸福度向上の可能性"
```

### 実験追跡
```
"過去の実験結果:

1. 'ミーティングなし金曜日' (スプリント12-14)
   - 結果: 生産性20%向上
   - 推奨事項: 恒久化

2. '複雑タスクでのペアプログラミング' (スプリント15)
   - 結果: 欠陥50%減少
   - 推奨事項: ガイドライン付きで継続"
```

## Integration Options

1. **GitHub Issues**: アクションアイテムをIssuesとして作成
2. **GitHub Projects V2**: レトロスペクティブアイテムをプロジェクトに追加
3. **GitHub Discussions**: チームディスカッション投稿
4. **GitHub Actions**: レトロスペクティブレポート自動生成
5. **Slack**: チームチャンネルに概要投稿
6. **GitHub Pages**: フォーマットされたレトロスペクティブページをエクスポート
7. **Calendar**: アクションアイテムチェックインをスケジュール

## GitHub Actions統合

### 自動レトロスペクティブワークフロー
```yaml
# .github/workflows/retrospective.yml
name: Automated Retrospective Analysis
on:
  schedule:
    - cron: '0 16 * * 5'  # 毎週金曜日16時
  workflow_dispatch:
    inputs:
      sprint_number:
        description: 'Sprint/Milestone Number'
        required: false

jobs:
  retrospective-analysis:
    runs-on: ubuntu-latest
    steps:
      - name: Collect Sprint Data
        run: |
          # Get closed issues from last sprint
          gh issue list --state closed --milestone "${{ github.event.inputs.sprint_number || 'current' }}" --json number,title,closedAt,labels,assignees
          
          # Get merged PRs
          gh pr list --state merged --json number,title,mergedAt,additions,deletions,reviewDecision
          
          # Get workflow runs
          gh api /repos/${{ github.repository }}/actions/runs --jq '.workflow_runs[] | select(.created_at > "'$(date -d '2 weeks ago' --iso-8601)'")'

      - name: Generate Retrospective Report
        run: |
          # Analyze data and generate insights
          echo "## Sprint Retrospective Analysis" > retrospective.md
          echo "Date: $(date)" >> retrospective.md
          # Add analysis logic here

      - name: Create Issues for Action Items
        run: |
          # Create GitHub Issues for identified action items
          gh issue create --title "Retrospective Action: Improve CI/CD Pipeline" --body "Based on retrospective analysis..." --label "retrospective,improvement"

      - name: Update Project Board
        run: |
          # Add retrospective items to project
          gh project item-create --owner ${{ github.repository_owner }} --number 1 --title "Sprint Retrospective Completed"
```

### メトリクス収集スクリプト
```bash
#!/bin/bash
# collect-metrics.sh - GitHub レトロスペクティブメトリクス収集

REPO_OWNER="${1:-$(gh repo view --json owner --jq .owner.login)}"
REPO_NAME="${2:-$(gh repo view --json name --jq .name)}"
DAYS_BACK="${3:-14}"

echo "Collecting retrospective metrics for $REPO_OWNER/$REPO_NAME (last $DAYS_BACK days)"

# Issue metrics
echo "## Issue Metrics"
gh issue list --state closed --search "closed:>$(date -d "$DAYS_BACK days ago" --iso-8601)" --json number,title,closedAt,labels --jq '
  group_by(.labels[].name) | 
  map({label: .[0].labels[0].name, count: length}) |
  sort_by(.count) | reverse'

# PR metrics  
echo "## Pull Request Metrics"
gh pr list --state merged --search "merged:>$(date -d "$DAYS_BACK days ago" --iso-8601)" --json number,title,mergedAt,additions,deletions,comments --jq '
  {
    total_prs: length,
    total_additions: map(.additions) | add,
    total_deletions: map(.deletions) | add,
    avg_comments: (map(.comments | length) | add / length)
  }'

# Actions metrics
echo "## GitHub Actions Metrics"
gh api "/repos/$REPO_OWNER/$REPO_NAME/actions/runs?per_page=100" --jq '
  .workflow_runs[] | 
  select(.created_at > "'$(date -d "$DAYS_BACK days ago" --iso-8601)'") |
  {name: .name, status: .status, conclusion: .conclusion, run_started_at: .run_started_at}'
```

## ベストプラクティス

1. **議論前のデータ**: まずメトリクスを確認
2. **パターンに焦点**: 繰り返しテーマを探す
3. **アクション指向**: すべての洞察にアクションが必要
4. **時間制限**: レトロスペクティブの焦点を維持
5. **フォローアップ**: アクションアイテム完了を追跡
6. **成功を祝う**: 改善を認める
7. **安全な場**: 正直なフィードバックを促す
8. **収集の自動化**: 一貫したデータ収集にGitHub Actionsを使用
9. **バージョン管理洞察**: 追跡のためレトロスペクティブ出力をリポジトリに保存