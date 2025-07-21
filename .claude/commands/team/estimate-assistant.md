# estimate-assistant

過去のgitデータ、コードの複雑性、チームベロシティに基づいたデータ駆動のタスク見積もりを提供します。

## 目的
このコマンドは過去のコミット、PR完了時間、コード複雑性メトリクス、チームパフォーマンスを分析して正確なタスク見積もりを提供します。チームが勘に基づく見積もりからデータに裏付けられた予測に移行するのを支援します。

## 使用方法
```bash
# 説明に基づいて特定のタスクを見積もり
claude "タスク見積もり: GoogleでOAuth2ログインフローを実装"

# 見積もりの過去の精度を分析
claude "過去10スプリントの見積もり精度を表示"

# コード変更に基づいた見積もり
claude "src/api/usersモジュールのリファクタリングの工数を見積もり"

# チームメンバー固有の見積もりを取得
claude "Aliceが支払いwebhookハンドラーを実装するのにどのくらいかかる？"
```

## 実行手順

### 1. 過去データの収集
git履歴とGitHub Projects/Issuesからデータを収集：

```bash
# Get commit history with timestamps and authors
git log --pretty=format:"%h|%an|%ad|%s" --date=iso --since="6 months ago" > commit_history.txt

# Analyze PR completion times
gh pr list --state closed --limit 100 --json number,title,createdAt,closedAt,additions,deletions,files

# Get file change frequency
git log --pretty=format: --name-only --since="6 months ago" | sort | uniq -c | sort -rn

# Analyze commit patterns by author
git shortlog -sn --since="6 months ago"
```

### 2. コード複雑性メトリクスの計算
コードの特性を分析：

```javascript
function analyzeComplexity(filePath) {
  const metrics = {
    lines: 0,
    cyclomaticComplexity: 0,
    dependencies: 0,
    testCoverage: 0,
    similarFiles: []
  };
  
  // コード行数をカウント
  const content = readFile(filePath);
  metrics.lines = content.split('\n').length;
  
  // サイクロマティック複雑度（簡易版）
  const conditions = content.match(/if\s*\(|while\s*\(|for\s*\(|case\s+|\?\s*:/g);
  metrics.cyclomaticComplexity = (conditions?.length || 0) + 1;
  
  // インポート/依存関係のカウント
  const imports = content.match(/import.*from|require\(/g);
  metrics.dependencies = imports?.length || 0;
  
  // 構造が類似したファイルを検索
  metrics.similarFiles = findSimilarFiles(filePath);
  
  return metrics;
}
```

### 3. 見積もりモデルの構築

#### 時間ベースの見積もり
```javascript
class HistoricalEstimator {
  constructor(gitData, projectData) {
    this.gitData = gitData;
    this.projectData = projectData;
    this.authorVelocity = new Map();
    this.fileTypeMultipliers = new Map();
  }
  
  calculateAuthorVelocity(author) {
    const authorCommits = this.gitData.filter(c => c.author === author);
    const taskCompletions = this.projectData.filter(t => 
      t.assignee === author && t.completedAt
    );
    
    // 1日あたりのコード行数
    const totalLines = authorCommits.reduce((sum, c) => 
      sum + c.additions + c.deletions, 0
    );
    const totalDays = this.calculateWorkDays(authorCommits);
    const linesPerDay = totalLines / totalDays;
    
    // スプリントあたりのストーリーポイント
    const pointsCompleted = taskCompletions.reduce((sum, t) => 
      sum + (t.estimate || 0), 0
    );
    const sprintCount = this.countSprints(taskCompletions);
    const pointsPerSprint = pointsCompleted / sprintCount;
    
    return {
      linesPerDay,
      pointsPerSprint,
      averageTaskDuration: this.calculateAverageTaskDuration(taskCompletions),
      accuracy: this.calculateEstimateAccuracy(taskCompletions)
    };
  }
  
  estimateTask(description, assignee = null) {
    // 説明から主要な特徴を抽出
    const features = this.extractFeatures(description);
    
    // 類似した完了タスクを検索
    const similarTasks = this.findSimilarTasks(features);
    
    // 類似タスクからベース見積もりを算出
    let baseEstimate = this.calculateMedianEstimate(similarTasks);
    
    // 複雑度指標に基づく調整
    const complexityMultiplier = this.calculateComplexityMultiplier(features);
    baseEstimate *= complexityMultiplier;
    
    // 担当者が指定されている場合の調整
    if (assignee) {
      const velocity = this.calculateAuthorVelocity(assignee);
      const teamAvgVelocity = this.calculateTeamAverageVelocity();
      const velocityRatio = velocity.pointsPerSprint / teamAvgVelocity;
      baseEstimate *= (2 - velocityRatio); // 高速な開発者は低い見積もりを取得
    }
    
    // 信頼区間を追加
    const confidence = this.calculateConfidence(similarTasks.length, features);
    
    return {
      estimate: Math.round(baseEstimate),
      confidence,
      range: {
        min: Math.round(baseEstimate * 0.7),
        max: Math.round(baseEstimate * 1.5)
      },
      basedOn: similarTasks.slice(0, 3),
      factors: this.explainFactors(features, complexityMultiplier)
    };
  }
}
```

#### パターン認識
```javascript
function extractFeatures(taskDescription) {
  const features = {
    keywords: [],
    fileTypes: [],
    modules: [],
    complexity: 'medium',
    type: 'feature', // feature, bug, refactor, etc.
    hasTests: false,
    hasUI: false,
    hasAPI: false,
    hasDatabase: false
  };
  
  // 複雑度を示すキーワード
  const complexityKeywords = {
    high: ['refactor', 'migrate', 'redesign', 'optimize', 'architecture'],
    medium: ['implement', 'add', 'create', 'update', 'integrate'],
    low: ['fix', 'adjust', 'tweak', 'change', 'modify']
  };
  
  // タスクタイプの検出
  if (taskDescription.match(/bug|fix|repair|broken/i)) {
    features.type = 'bug';
  } else if (taskDescription.match(/refactor|cleanup|optimize/i)) {
    features.type = 'refactor';
  } else if (taskDescription.match(/test|spec|coverage/i)) {
    features.type = 'test';
  }
  
  // コンポーネントの検出
  features.hasUI = /UI|frontend|component|view|page/i.test(taskDescription);
  features.hasAPI = /API|endpoint|route|REST|GraphQL/i.test(taskDescription);
  features.hasDatabase = /database|DB|migration|schema|query/i.test(taskDescription);
  features.hasTests = /test|spec|TDD|coverage/i.test(taskDescription);
  
  // 言及されたファイルタイプを抽出
  const fileTypeMatches = taskDescription.match(/\.(js|ts|jsx|tsx|py|java|go|rb|css|scss)/g);
  if (fileTypeMatches) {
    features.fileTypes = [...new Set(fileTypeMatches)];
  }
  
  return features;
}
```

### 4. ベロシティトラッキング
チームと個人のパフォーマンスを追跡：

```javascript
class VelocityTracker {
  async analyzeVelocity(timeframe = '3 months') {
    // 見積もりと実際の時間を含む完了タスクを取得
    const completedTasks = await this.getCompletedTasks(timeframe);
    
    const analysis = {
      team: {
        plannedPoints: 0,
        completedPoints: 0,
        averageVelocity: 0,
        velocityTrend: [],
        estimateAccuracy: 0
      },
      individuals: new Map(),
      taskTypes: new Map()
    };
    
    // スプリントごとにグループ化
    const tasksBySprint = this.groupBySprint(completedTasks);
    
    for (const [sprint, tasks] of tasksBySprint) {
      const sprintVelocity = tasks.reduce((sum, t) => sum + (t.estimate || 0), 0);
      const sprintActual = tasks.reduce((sum, t) => sum + (t.actualPoints || t.estimate || 0), 0);
      
      analysis.team.velocityTrend.push({
        sprint,
        planned: sprintVelocity,
        actual: sprintActual,
        accuracy: sprintVelocity ? (sprintActual / sprintVelocity) : 1
      });
    }
    
    // 個人のベロシティ
    const tasksByAssignee = this.groupBy(completedTasks, 'assignee');
    for (const [assignee, tasks] of tasksByAssignee) {
      analysis.individuals.set(assignee, {
        tasksCompleted: tasks.length,
        pointsCompleted: tasks.reduce((sum, t) => sum + (t.estimate || 0), 0),
        averageAccuracy: this.calculateAccuracy(tasks),
        strengths: this.identifyStrengths(tasks)
      });
    }
    
    return analysis;
  }
}
```

### 5. 機械学習による見積もり
過去のパターンを予測に活用：

```javascript
class MLEstimator {
  trainModel(historicalTasks) {
    // 特徴抽出
    const features = historicalTasks.map(task => ({
      // テキスト特徴
      titleLength: task.title.length,
      descriptionLength: task.description.length,
      hasAcceptanceCriteria: task.description.includes('Acceptance'),
      
      // コード特徴
      filesChanged: task.linkedPR?.filesChanged || 0,
      linesAdded: task.linkedPR?.additions || 0,
      linesDeleted: task.linkedPR?.deletions || 0,
      
      // タスク特徴
      labels: task.labels.length,
      hasDesignDoc: task.attachments?.some(a => a.title.includes('design')),
      dependencies: task.blockedBy?.length || 0,
      
      // 履歴特徴
      assigneeAvgVelocity: this.getAssigneeVelocity(task.assignee),
      teamLoad: this.getTeamLoad(task.createdAt),
      
      // ターゲット
      actualEffort: task.actualPoints || task.estimate
    }));
    
    // 単純な線形回帰（実際には適切なMLライブラリを使用）
    return this.fitLinearModel(features);
  }
  
  predict(taskDescription, context) {
    const features = this.extractTaskFeatures(taskDescription, context);
    const prediction = this.model.predict(features);
    
    // 特徴の類似度に基づく不確実性を追加
    const similarityScore = this.calculateSimilarity(features);
    const uncertainty = 1 - similarityScore;
    
    return {
      estimate: Math.round(prediction),
      confidence: similarityScore,
      breakdown: this.explainPrediction(features, prediction)
    };
  }
}
```

### 6. 見積もりレポート形式

```markdown
## タスク見積もりレポート

**タスク:** GoogleでのOAuth2ログインフローの実装
**日付:** 2024-01-15

### 見積もり: 5 ストーリーポイント (±2)
**信頼度:** 78%
**推定時間:** 15-25時間

### 分析内訳

#### 類似した完了タスク:
1. "GitHub OAuth統合の実装" - 5ポイント（実際: 6）
2. "Facebookログイン追加" - 4ポイント（実際: 4）
3. "SAML SSO設定" - 8ポイント（実際: 7）

#### 複雑度要因:
- **認証フロー** (+1ポイント): OAuth2は複数のリダイレクトが必要
- **外部API** (+1ポイント): Google API統合
- **セキュリティ** (+1ポイント): トークン保存と検証
- **テスト** (-0.5ポイント): 類似のテストが既に存在

#### 履歴データ:
- 認証機能のチーム平均: 4.8ポイント
- 過去5つの認証タスクの精度: 85%
- 担当者のベロシティ: チーム平均の1.2倍

#### リスク要因:
⚠️ Google APIは頻繁に変更される
⚠️ 既存のOAuth2インフラストラクチャが存在しない
✅ チームにOAuth経験がある
✅ 良好なドキュメントが利用可能

### 推奨事項:
1. 初期Google API設定に1ポイント割り当て
2. セキュリティレビューの時間を含める
3. モックOAuthサーバーでの統合テストを計画
4. GitHub OAuthを担当したチームメンバーとのペアリングを検討

### スプリント計画:
- 1スプリントで完了可能
- 他の認証関連タスクと組み合わせることが最適
- スプリントの最後のタスクにすべきではない（リスクバッファ）
```

### 7. エラーハンドリング
```javascript
// 履歴データが不足している場合の処理
if (historicalTasks.length < 10) {
  console.warn("履歴データが限られています。見積もりの精度が低くなる可能性があります。");
  // ルールベースの見積もりにフォールバック
}

// 新しいタイプの作業の処理
const similarity = findSimilarIssues(description);
if (similarity.maxScore < 0.5) {
  console.warn("これは新しいタイプのIssueのようです。保守的な見積もりを使用します。");
  // 不確実性乗数を適用
}

// GitHub Projects接続が不足している場合の処理
if (!github.projects.available) {
  console.log("見積もりにはgit履歴とissuesのみを使用します");
  // gitと基本的なGitHubデータによる見積もりを使用
}
```

## 出力例

```
タスク分析中: "ユーザー認証をJWTトークンを使用するようにリファクタリング"

📊 履歴分析:
- 23件の類似認証タスクを発見
- 平均完了: 4.2ストーリーポイント
- 精度率: 82%

🧮 見積もり計算:
ベース見積もり: 4ポイント（類似タスクから）
調整:
  +1ポイント - リファクタリング（高い複雑度）
  +0.5ポイント - セキュリティ影響
  -0.5ポイント - 既存のテストカバレッジ
  
最終見積もり: 5ストーリーポイント

📈 信頼度分析:
- 過去のタスクとの高い類似性（85%）
- 良好な履歴データ（23サンプル）
- 信頼度: 78%

👥 チーム洞察:
- Alice: 3件の類似タスクを完了（平均4.3ポイント）
- Bob: リファクタリングに強い（平均より20%高速）
- 推奨担当者: Bob

⏱️ 時間見積もり:
- 楽観的: 12時間（3ポイント）
- 現実的: 20時間（5ポイント）
- 悲観的: 32時間（8ポイント）

📝 内訳:
1. 現在の認証システムの分析（0.5ポイント）
2. JWTトークン構造の設計（0.5ポイント）
3. JWTサービスの実装（1.5ポイント）
4. 認証ミドルウェアのリファクタリング（1.5ポイント）
5. テストとドキュメントの更新（1ポイント）
```

## GitHub Actions統合

### 自動見積もりワークフロー
```yaml
# .github/workflows/estimation.yml
name: Automated Issue Estimation
on:
  issues:
    types: [opened, labeled]
  workflow_dispatch:

jobs:
  estimate-issue:
    runs-on: ubuntu-latest
    if: contains(github.event.issue.labels.*.name, 'needs-estimate')
    steps:
      - name: Analyze Similar Issues
        run: |
          # Get similar issues based on labels and title
          gh issue list --label "$(echo "${{ github.event.issue.labels }}" | jq -r '.[].name' | head -1)" --state closed --json number,title,createdAt,closedAt,labels
          
          # Calculate average completion time
          SIMILAR_ISSUES=$(gh issue list --search "label:feature is:closed" --json createdAt,closedAt --jq '
            map(select(.closedAt != null)) |
            map(((.closedAt | strptime("%Y-%m-%dT%H:%M:%SZ") | mktime) - (.createdAt | strptime("%Y-%m-%dT%H:%M:%SZ") | mktime)) / 86400) |
            add / length
          ')
          echo "Average completion time: $SIMILAR_ISSUES days"

      - name: Add Estimation Comment
        run: |
          gh issue comment ${{ github.event.issue.number }} --body "
          ## 🤖 Automated Estimation

          Based on similar issues:
          - **Estimated effort**: $SIMILAR_ISSUES days
          - **Confidence**: Medium
          - **Similar issues**: $(gh issue list --search 'label:feature is:closed' --limit 3 --json number,title --jq '.[].title' | paste -sd, -)

          *This is an automated estimate. Please review and adjust as needed.*
          "

      - name: Update Project with Estimate
        run: |
          # Add to project with estimated effort
          gh project item-create --owner ${{ github.repository_owner }} --number 1 --title "${{ github.event.issue.title }}"
```

### 見積もり精度追跡
```yaml
# .github/workflows/estimation-accuracy.yml
name: Estimation Accuracy Tracking
on:
  issues:
    types: [closed]
  schedule:
    - cron: '0 9 * * 1'  # Weekly on Monday

jobs:
  track-accuracy:
    runs-on: ubuntu-latest
    steps:
      - name: Calculate Estimation Accuracy
        run: |
          # Get issues closed in last week with estimates
          CLOSED_ISSUES=$(gh issue list --state closed --search "closed:>$(date -d '7 days ago' --iso-8601)" --json number,title,createdAt,closedAt,body)
          
          # Extract estimates from issue bodies and calculate accuracy
          echo "$CLOSED_ISSUES" | jq -r '.[] | 
            select(.body | test("Estimated effort.*([0-9]+)")) |
            {
              number: .number,
              title: .title,
              estimated: (.body | capture("Estimated effort.*(?<days>[0-9]+)") | .days | tonumber),
              actual: (((.closedAt | strptime("%Y-%m-%dT%H:%M:%SZ") | mktime) - (.createdAt | strptime("%Y-%m-%dT%H:%M:%SZ") | mktime)) / 86400)
            }'

      - name: Generate Accuracy Report
        run: |
          echo "## Weekly Estimation Accuracy Report" > accuracy_report.md
          echo "Generated: $(date)" >> accuracy_report.md
          echo "" >> accuracy_report.md
          
          # Add accuracy metrics
          echo "### Metrics" >> accuracy_report.md
          echo "- Issues analyzed: $ISSUE_COUNT" >> accuracy_report.md
          echo "- Average accuracy: $ACCURACY%" >> accuracy_report.md
          echo "- Estimation trend: $TREND" >> accuracy_report.md

      - name: Create Accuracy Issue
        run: |
          gh issue create --title "Weekly Estimation Accuracy Report" --body-file accuracy_report.md --label "estimation,report"
```

## ヒント
- 最低6ヶ月間の履歴データを維持する
- 各スプリント/マイルストーン後に見積もりを再調整する
- 継続的改善のために実際vs見積もりを追跡する
- 外部要因（休日、チーム変更）を考慮する
- 複雑なIssueにはペアプログラミング乗数を使用する
- 見積もりの前提を文書化する
- レトロスペクティブで見積もりをレビューする
- 自動見積もりにGitHub Actionsを活用する
- 見積もり追跡にGitHub Projects V2のカスタムフィールドを使用する