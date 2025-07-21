# チームワークロード分散バランサー

チームのワークロード分散を最適化します

## 目的
このコマンドはチームメンバーの現在のワークロード、スキル、過去のパフォーマンス、対応可能性を分析して最適なタスク割り当てを提案します。燃え尽き症候群を防ぎ、バランスの取れた分散を確保し、チームメンバーの強みに合わせてタスクをマッチングします。

## 使用方法
```bash
# 現在のチームワークロードを表示
claude "エンジニアリングチームのワークロードバランスを表示"

# 新しいタスクの最適な担当者を提案
claude "新しい決済統合タスクの担当者を提案"

# 現在のスプリントのリバランス
claude "最適な分散のため現在のスプリントのタスクをリバランス"

# 次のスプリントの容量計画
claude "チーム容量に基づく次のスプリントのタスク割り当て計画"
```

## 実行手順

### 1. チームデータの収集
チームメンバーの情報を収集：

```javascript
class TeamAnalyzer {
  async gatherTeamData() {
    const team = {};
    
    // GitHub Organizationからチームメンバーを取得
    const teamMembers = await github.getOrgMembers();
    
    for (const member of teamMembers) {
      team[member.login] = {
        name: member.name || member.login,
        email: await github.getUserEmail(member.login),
        currentTasks: [],
        completedTasks: [],
        skills: new Set(),
        velocity: 0,
        availability: 100, // パーセンテージ
        preferences: {},
        strengths: [],
        timeZone: await this.getTimeZone(member.login)
      };
      
      // 現在のアサインメントを取得（GitHub Issues/Projects）
      const activeIssues = await github.getAssignedIssues(member.login, {
        state: 'open'
      });
      team[member.login].currentTasks = activeIssues;
      
      // 履歴データを取得
      const closedIssues = await github.getAssignedIssues(member.login, {
        state: 'closed',
        since: new Date(Date.now() - 90 * 24 * 60 * 60 * 1000) // 3ヶ月前
      });
      team[member.login].completedTasks = closedIssues;
      
      // Git貢献分析
      const gitStats = await this.analyzeGitContributions(member.email || member.login);
      team[member.login].skills = gitStats.technologies;
      team[member.login].codeContributions = gitStats.contributions;
    }
    
    return team;
  }
  
  async analyzeGitContributions(identifier) {
    // コミット履歴を取得
    const commits = await exec(`git log --author="${identifier}" --since="6 months ago" --pretty=format:"%H"`);
    const commitHashes = commits.split('\n').filter(Boolean);
    
    const stats = {
      technologies: new Set(),
      contributions: {
        frontend: 0,
        backend: 0,
        database: 0,
        devops: 0,
        testing: 0
      },
      linesChanged: 0,
      filesChanged: new Set()
    };
    
    for (const hash of commitHashes) {
      const diffStat = await exec(`git show --stat --format="" ${hash}`);
      const changedFiles = await exec(`git show --name-only --format="" ${hash}`);
      
      // ファイル拡張子から技術を推定
      changedFiles.split('\n').forEach(file => {
        if (!file) return;
        stats.filesChanged.add(file);
        
        const ext = file.split('.').pop()?.toLowerCase();
        switch (ext) {
          case 'js':
          case 'jsx':
          case 'ts':
          case 'tsx':
          case 'vue':
          case 'html':
          case 'css':
          case 'scss':
            stats.technologies.add('frontend');
            stats.contributions.frontend++;
            break;
          case 'py':
          case 'java':
          case 'go':
          case 'rb':
          case 'php':
          case 'cs':
            stats.technologies.add('backend');
            stats.contributions.backend++;
            break;
          case 'sql':
          case 'migration':
            stats.technologies.add('database');
            stats.contributions.database++;
            break;
          case 'yml':
          case 'yaml':
          case 'dockerfile':
            stats.technologies.add('devops');
            stats.contributions.devops++;
            break;
          case 'test':
          case 'spec':
            stats.technologies.add('testing');
            stats.contributions.testing++;
            break;
        }
      });
    }
    
    return stats;
  }
}
```

### 2. ワークロード分析
現在の作業量とキャパシティを評価：

```javascript
class WorkloadAnalyzer {
  analyzeCurrentWorkload(team) {
    const analysis = {};
    
    for (const [memberId, member] of Object.entries(team)) {
      const workload = {
        currentPoints: 0,
        estimatedHours: 0,
        taskComplexity: 'medium',
        overloaded: false,
        underutilized: false,
        capacity: member.availability
      };
      
      // GitHub Issues/Projects からストーリーポイントまたは見積もりを取得
      member.currentTasks.forEach(task => {
        // ラベルやコメントから見積もりを抽出
        const estimate = this.extractTaskEstimate(task);
        workload.currentPoints += estimate.points;
        workload.estimatedHours += estimate.hours;
      });
      
      // 過負荷/低稼働の判定
      const weeklyCapacity = (member.availability / 100) * 40; // 週40時間基準
      workload.overloaded = workload.estimatedHours > weeklyCapacity * 1.2;
      workload.underutilized = workload.estimatedHours < weeklyCapacity * 0.6;
      
      analysis[memberId] = workload;
    }
    
    return analysis;
  }
  
  extractTaskEstimate(issue) {
    // GitHub Issueから見積もり情報を抽出
    let points = 0;
    let hours = 0;
    
    // ラベルから見積もりを取得 (例: "estimate: 3", "3 points")
    const estimateLabels = issue.labels.filter(label => 
      label.name.match(/estimate|points|hours/i)
    );
    
    estimateLabels.forEach(label => {
      const match = label.name.match(/(\d+)/);
      if (match) {
        if (label.name.includes('hour')) {
          hours += parseInt(match[1]);
        } else {
          points += parseInt(match[1]);
          hours += parseInt(match[1]) * 4; // 1ポイント = 4時間の概算
        }
      }
    });
    
    // 本文やコメントからも見積もりを探す
    const bodyMatch = issue.body?.match(/estimate[:\s]+(\d+)(?:\s*(hours?|points?))?/i);
    if (bodyMatch) {
      const value = parseInt(bodyMatch[1]);
      if (bodyMatch[2]?.includes('hour')) {
        hours += value;
      } else {
        points += value;
        hours += value * 4;
      }
    }
    
    // デフォルト見積もり（ラベルベース）
    if (points === 0 && hours === 0) {
      if (issue.labels.some(l => l.name.match(/bug|hotfix/i))) {
        points = 2; hours = 8;
      } else if (issue.labels.some(l => l.name.match(/feature|enhancement/i))) {
        points = 5; hours = 20;
      } else {
        points = 3; hours = 12;
      }
    }
    
    return { points, hours };
  }
}
```

### 3. スキルマッチング
タスクとチームメンバーのスキルをマッチング：

```javascript
class SkillMatcher {
  matchTasksToMembers(tasks, team) {
    const recommendations = [];
    
    tasks.forEach(task => {
      const taskRequirements = this.analyzeTaskRequirements(task);
      const memberScores = {};
      
      Object.entries(team).forEach(([memberId, member]) => {
        const score = this.calculateMatchScore(taskRequirements, member);
        memberScores[memberId] = score;
      });
      
      // スコア順にソート
      const sortedMembers = Object.entries(memberScores)
        .sort(([,a], [,b]) => b.overall - a.overall)
        .slice(0, 3); // トップ3候補
      
      recommendations.push({
        task: task,
        candidates: sortedMembers.map(([memberId, score]) => ({
          member: team[memberId],
          score: score,
          reasoning: this.generateReasoning(taskRequirements, team[memberId], score)
        }))
      });
    });
    
    return recommendations;
  }
  
  analyzeTaskRequirements(issue) {
    const requirements = {
      technologies: new Set(),
      complexity: 'medium',
      type: 'feature',
      urgency: 'normal',
      collaboration: false
    };
    
    // ラベルから要件を抽出
    issue.labels.forEach(label => {
      const name = label.name.toLowerCase();
      
      // 技術タグ
      if (name.includes('frontend')) requirements.technologies.add('frontend');
      if (name.includes('backend')) requirements.technologies.add('backend');
      if (name.includes('database')) requirements.technologies.add('database');
      if (name.includes('devops')) requirements.technologies.add('devops');
      
      // 複雑度
      if (name.includes('simple') || name.includes('easy')) {
        requirements.complexity = 'low';
      } else if (name.includes('complex') || name.includes('hard')) {
        requirements.complexity = 'high';
      }
      
      // タイプ
      if (name.includes('bug')) requirements.type = 'bug';
      if (name.includes('feature')) requirements.type = 'feature';
      if (name.includes('refactor')) requirements.type = 'refactor';
      
      // 緊急度
      if (name.includes('urgent') || name.includes('critical')) {
        requirements.urgency = 'high';
      }
    });
    
    // 本文から追加要件を抽出
    const body = issue.body?.toLowerCase() || '';
    if (body.includes('pair programming') || body.includes('collaboration')) {
      requirements.collaboration = true;
    }
    
    return requirements;
  }
  
  calculateMatchScore(requirements, member) {
    let skillScore = 0;
    let availabilityScore = 0;
    let experienceScore = 0;
    
    // スキルマッチング
    requirements.technologies.forEach(tech => {
      if (member.skills.has(tech)) {
        skillScore += 30;
      }
    });
    
    // 可用性スコア
    availabilityScore = Math.min(member.availability, 100);
    
    // 経験スコア（完了タスク数ベース）
    experienceScore = Math.min(member.completedTasks.length * 2, 40);
    
    // 複雑度とメンバーレベルのマッチング
    const memberLevel = this.assessMemberLevel(member);
    let complexityMatch = 50;
    
    if (requirements.complexity === 'low' && memberLevel === 'junior') complexityMatch = 80;
    if (requirements.complexity === 'medium' && memberLevel === 'mid') complexityMatch = 80;
    if (requirements.complexity === 'high' && memberLevel === 'senior') complexityMatch = 80;
    
    return {
      overall: (skillScore + availabilityScore + experienceScore + complexityMatch) / 4,
      skill: skillScore,
      availability: availabilityScore,
      experience: experienceScore,
      complexity: complexityMatch
    };
  }
  
  assessMemberLevel(member) {
    const completedTasks = member.completedTasks.length;
    const contributions = Object.values(member.codeContributions || {}).reduce((a, b) => a + b, 0);
    
    if (completedTasks > 50 || contributions > 100) return 'senior';
    if (completedTasks > 20 || contributions > 30) return 'mid';
    return 'junior';
  }
}
```

### 4. 最適化推奨事項
```javascript
class WorkloadOptimizer {
  generateRecommendations(workloadAnalysis, team) {
    const recommendations = {
      rebalancing: [],
      capacityAdjustments: [],
      skillDevelopment: [],
      processImprovements: []
    };
    
    // 過負荷メンバーからの作業移管
    Object.entries(workloadAnalysis).forEach(([memberId, workload]) => {
      if (workload.overloaded) {
        const tasksToReassign = team[memberId].currentTasks
          .sort((a, b) => this.getTaskPriority(b) - this.getTaskPriority(a))
          .slice(0, Math.ceil(team[memberId].currentTasks.length * 0.3));
        
        recommendations.rebalancing.push({
          type: 'reassign',
          from: memberId,
          tasks: tasksToReassign,
          reason: '過負荷状態の軽減'
        });
      }
      
      if (workload.underutilized) {
        recommendations.capacityAdjustments.push({
          type: 'assign_more',
          member: memberId,
          additionalCapacity: workload.capacity - workload.estimatedHours,
          reason: '追加タスクの受け入れ可能'
        });
      }
    });
    
    return recommendations;
  }
}
```

### 5. レポート生成
```markdown
## チームワークロードバランス分析

### 現在のワークロード状況
- **過負荷メンバー**: 田中 (125% 稼働), 佐藤 (110% 稼働)
- **適正稼働メンバー**: 鈴木 (85% 稼働), 高橋 (90% 稼働)
- **低稼働メンバー**: 伊藤 (60% 稼働)

### 推奨アクション
1. **即座のリバランス**:
   - 田中の "決済API実装" を鈴木に移管 (8時間削減)
   - 佐藤の "UI改善タスク" を伊藤に移管 (12時間削減)

2. **新規タスク割り当て**:
   - "セキュリティ監査" → 鈴木 (バックエンド経験豊富)
   - "パフォーマンス最適化" → 高橋 (過去の最適化実績)

3. **スキル開発機会**:
   - 伊藤にフロントエンド研修を推奨 (容量に余裕)
   - 田中のプロジェクト管理スキル向上支援

### 次週の予測稼働率
- 田中: 95% (最適範囲)
- 佐藤: 88% (最適範囲)
- 鈴木: 92% (最適範囲)
- 高橋: 87% (最適範囲)
- 伊藤: 75% (やや低いが改善)
```

このコマンドは以下の改善に役立ちます：
- チーム効率の最大化
- 燃え尽き症候群の予防
- スキルベースの最適なタスク配分
- 公平なワークロード分散