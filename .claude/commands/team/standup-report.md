# スタンドアップレポート

gitコミットとGitHub Issuesアクティビティから日次スタンドアップレポートを生成し、チームメンバーがスタンドアップミーティングの準備をサポートします。

## 実行手順

1. **初期セットアップ**
   - GitHub APIアクセスを確認
   - 時間範囲を決定（デフォルト：過去24時間）
   - チームメンバーを特定（git configまたはユーザー入力から）
   - レポート形式の設定を行う

2. **データ収集**

#### Git活動分析
```bash
# 過去24時間のコミットを収集
git log --since="24 hours ago" --all --format="%h|%an|%ad|%s" --date=short

# ブランチ活動を確認
git for-each-ref --format='%(refname:short)|%(committerdate:short)|%(authoremail)' --sort=-committerdate refs/heads/

# ファイル変更を分析
git diff --stat @{1.day.ago}
```

#### GitHub Issues統合
```
1. 過去24時間に更新されたIssuesを取得
2. Issueステータス変更を取得
3. 新しいコメントとブロッカーを確認
4. 完了済みIssuesを確認
```

#### GitHub PRステータス
```
1. PR更新とレビューを確認
2. マージされたPRを特定
3. 新規作成されたPRを見つける
4. CI/CDステータスを確認
```

3. **レポート生成**

構造化されたスタンドアップレポートを生成：

```markdown
# 日次スタンドアップレポート - [日付]

## チームメンバー: [名前]

### 昨日の成果
- ✅ 完了 [Issue #ID]: [説明]
  - コミット: [リンク付きリスト]
  - PR: [該当する場合のリンク]
- 🔄 進行中 [Issue #ID]: [説明]
  - 現在のステータス: [X]% 完了
  - 最新コミット: [メッセージ]

### 今日の計画
- 🎯 [Issue #ID]: [説明]
  - 完了予定: [時間]
  - 依存関係: [リスト]
- 🔍 PR #[番号] のコードレビュー
- 📝 [機能] のドキュメント更新

### ブロッカーと懸念事項
- 🚫 [Issue #ID] でブロック: [理由]
  - 必要な入力元: [担当者/チーム]
  - 解決予定: [時間]
- ⚠️ 潜在的リスク: [説明]

### メトリクス概要
- コミット数: [数]
- PR更新数: [数]
- 完了Issue数: [数]
- サイクル時間: [平均]
```

4. **マルチフォーマット出力**

様々な形式で出力を提供：

#### Slack形式
```
*日次スタンドアップ - @username*

*昨日:*
• PR #123をマージ: ユーザー認証追加
• 決済処理のバグ修正 (#456)
• 3つのPRをレビュー

*今日:*
• #457開始: レート制限実装
• @teammateとデータベース移行のペアプログラミング
• 午後2時にスプリント計画ミーティング

*ブロッカー:*
• DevOpsからのAPI認証情報待ち
• #458にデザイン明確化が必要
```

#### メール形式
```
件名: 日次スタンドアップ - [名前] - [日付]

チームの皆様

本日のスタンドアップ更新情報をお送りします：

昨日完了した作業:
- [詳細なコンテキスト付きリスト]

本日の予定:
- [優先順位付きタスクリスト]

ブロッカー/サポート要請:
- [障害の明確な説明]

ご質問があればお知らせください。

よろしくお願いします、
[名前]
```

5. **チーム統合ビュー**

チームリーダー向けに統合ビューを生成：

```markdown
# チームスタンドアップ概要 - [日付]

## ベロシティメトリクス
- 総コミット数: [数]
- マージされたPR数: [数]
- 完了Issue数: [数]
- アクティブブロッカー数: [数]

## 個別更新
[各チームメンバーの概要]

## 重要事項
- 緊急対応が必要なブロッカー
- リスクのある成果物
- リソース競合

## チーム健全性指標
- 順調なタスク: [%]
- ブロックされたタスク: [%]
- 期限超過項目: [数]
```

## エラーハンドリング

### GitHub APIアクセスなし
```
"GitHub APIアクセスが使用できません。gitデータのみからレポートを生成します。

完全な機能を有効にするには:
1. GitHub CLIをインストールし認証: gh auth login
2. 適切なスコープでトークンを設定
3. リポジトリアクセス権限を確認

利用可能なデータで続行します..."
```

### 最近の活動なし
```
"過去24時間でgit活動が見つかりませんでした。

考えられる理由:
1. コミットが作成されていない（時間範囲を確認）
2. 追跡されていないブランチで作業中
3. ローカル変更がコミットされていない

以下のいずれかを希望しますか:
- 時間範囲を拡張しますか？
- 特定のブランチを確認しますか？
- 手動で更新を入力しますか？"
```

## インタラクティブ機能

1. **更新のカスタマイズ**
```
"スタンドアップレポートを生成しました。以下を希望しますか:
1. 任意の項目に追加コンテキストを追加？
2. 本日の優先順位を再整理？
3. 見落としたブロッカーや懸念事項を追加？
4. git外で行った作業を含める？"
```

2. **ブロッカー解決**
```
"ブロッカーがあることに注意しました。以下のサポートが必要ですか:
1. ブロック解除のメッセージ作成？
2. 代替アプローチの検討？
3. サポートできる人の特定？"
```

## ベストプラクティス

1. **スタンドアップ前に実行**: ミーティング15-30分前に生成
2. **具体的に**: タスクIDと測定可能な進捗を含める
3. **ブロッカーを早期に強調**: スタンドアップまで待たない
4. **簡潔に保つ**: 重要な更新に焦点を当てる
5. **証拠にリンク**: コミット/PRリンクを含める

## 高度な機能

### トレンド分析
```
"過去1週間を見ると:
- 1日平均コミット数: [数]
- タスク完了率: [%]
- 一般的なブロッカーパターン: [リスト]

改善提案:
[パーソナライズされた推奨事項]"
```

### スマートスケジューリング
```
"カレンダーとタスク見積もりに基づいて:
- 本日5時間の集中時間があります
- 推奨タスク順序: [優先順位付きリスト]
- 潜在的競合: [ミーティング重複]"
```

## GitHub Actions統合

### 自動スタンドアップレポート生成
```yaml
# .github/workflows/standup-report.yml
name: Daily Standup Report
on:
  schedule:
    - cron: '0 8 * * 1-5'  # Weekdays at 8 AM
  workflow_dispatch:

jobs:
  generate-standup:
    runs-on: ubuntu-latest
    steps:
      - name: Collect Activity Data
        run: |
          # Get yesterday's activity
          YESTERDAY=$(date -d yesterday '+%Y-%m-%d')
          
          # Closed issues
          CLOSED_ISSUES=$(gh issue list --state closed --search "closed:$YESTERDAY" --json number,title,assignees,labels)
          
          # Merged PRs
          MERGED_PRS=$(gh pr list --state merged --search "merged:$YESTERDAY" --json number,title,author,additions,deletions)
          
          # New commits
          COMMITS=$(git log --since="$YESTERDAY 00:00" --until="$YESTERDAY 23:59" --pretty=format:"%h|%an|%s" --author="${{ github.actor }}")
          
          echo "CLOSED_ISSUES=$CLOSED_ISSUES" >> $GITHUB_ENV
          echo "MERGED_PRS=$MERGED_PRS" >> $GITHUB_ENV
          echo "COMMITS=$COMMITS" >> $GITHUB_ENV

      - name: Generate Standup Report
        run: |
          cat > standup_report.md << EOF
          # 📋 Daily Standup Report - $(date '+%Y-%m-%d')
          
          **Team Member:** @${{ github.actor }}
          
          ## Yesterday's Accomplishments
          $(echo "$CLOSED_ISSUES" | jq -r '.[] | "- ✅ Closed #\(.number): \(.title)"')
          $(echo "$MERGED_PRS" | jq -r '.[] | "- 🔀 Merged PR #\(.number): \(.title)"')
          
          ## Today's Plans
          $(gh issue list --assignee @me --state open --json number,title --jq '.[] | "- 🎯 #\(.number): \(.title)"')
          
          ## Blockers
          $(gh issue list --assignee @me --label blocked --json number,title --jq '.[] | "- 🚫 #\(.number): \(.title)"')
          
          ## Metrics
          - Commits: $(echo "$COMMITS" | wc -l)
          - Issues closed: $(echo "$CLOSED_ISSUES" | jq 'length')
          - PRs merged: $(echo "$MERGED_PRS" | jq 'length')
          EOF

      - name: Post to Slack
        if: env.SLACK_WEBHOOK_URL != ''
        run: |
          curl -X POST -H 'Content-type: application/json' \
            --data "{\"text\":\"📋 Daily Standup Report for @${{ github.actor }}:\n\`\`\`$(cat standup_report.md)\`\`\`\"}" \
            ${{ secrets.SLACK_WEBHOOK_URL }}

      - name: Create Discussion Post
        run: |
          gh api repos/${{ github.repository }}/discussions \
            --method POST \
            --field title="Daily Standup - ${{ github.actor }} - $(date '+%Y-%m-%d')" \
            --field body="$(cat standup_report.md)" \
            --field category_id="$(gh api repos/${{ github.repository }}/discussions/categories --jq '.[] | select(.name=="General") | .id')"

      - name: Update Project Status
        run: |
          # Move completed issues in project board
          echo "$CLOSED_ISSUES" | jq -r '.[].number' | while read issue_num; do
            gh project item-edit --owner ${{ github.repository_owner }} --number 1 --id "$(gh project item-list --owner ${{ github.repository_owner }} --number 1 --format json | jq -r --arg issue "#$issue_num" '.items[] | select(.content.title == $issue) | .id')" --field-id "Status" --text "Done"
          done
```

### チームスタンドアップ集計
```yaml
# .github/workflows/team-standup.yml
name: Team Standup Summary
on:
  schedule:
    - cron: '30 8 * * 1-5'  # Weekdays at 8:30 AM
  workflow_dispatch:

jobs:
  team-summary:
    runs-on: ubuntu-latest
    steps:
      - name: Collect Team Activity
        run: |
          # Get all team members' activity
          TEAM_MEMBERS=$(gh api orgs/${{ github.repository_owner }}/teams/engineering/members --jq '.[].login')
          
          echo "# 👥 Team Standup Summary - $(date '+%Y-%m-%d')" > team_summary.md
          echo "" >> team_summary.md
          
          echo "$TEAM_MEMBERS" | while read member; do
            echo "## @$member" >> team_summary.md
            
            # Yesterday's issues
            CLOSED=$(gh issue list --state closed --assignee "$member" --search "closed:$(date -d yesterday '+%Y-%m-%d')" --json number,title)
            echo "$CLOSED" | jq -r '.[] | "- ✅ #\(.number): \(.title)"' >> team_summary.md
            
            # Today's plans
            OPEN=$(gh issue list --state open --assignee "$member" --json number,title --limit 3)
            echo "$OPEN" | jq -r '.[] | "- 🎯 #\(.number): \(.title)"' >> team_summary.md
            
            # Blockers
            BLOCKED=$(gh issue list --assignee "$member" --label blocked --json number,title)
            if [ "$(echo "$BLOCKED" | jq 'length')" -gt 0 ]; then
              echo "### 🚫 Blockers:" >> team_summary.md
              echo "$BLOCKED" | jq -r '.[] | "- #\(.number): \(.title)"' >> team_summary.md
            fi
            
            echo "" >> team_summary.md
          done

      - name: Post Team Summary
        run: |
          gh issue create \
            --title "Team Standup Summary - $(date '+%Y-%m-%d')" \
            --body-file team_summary.md \
            --label "standup,team" \
            --assignee "@org/team-leads"
```

## コマンド例

### 基本的な使用方法
```
ユーザー: "スタンドアップレポートを生成して"
アシスタント: [過去24時間の標準レポートを生成]
```

### カスタム時間範囲
```
ユーザー: "過去2日間のスタンドアップを生成して"
アシスタント: [48時間をカバーするレポートを生成]
```

### チームレポート
```
ユーザー: "チームスタンドアップ概要を生成して"
アシスタント: [統合チームビューを生成]
```

### 特定の形式
```
ユーザー: "Slack形式でスタンドアップを生成して"
アシスタント: [貼り付け準備完了のSlack形式メッセージを生成]
```

### GitHub Projects統合
```
ユーザー: "GitHub Projectsのステータス更新を含むスタンドアップを生成して"
アシスタント: [Projects V2のカラム移動とカスタムフィールド更新を含むレポートを生成]
```