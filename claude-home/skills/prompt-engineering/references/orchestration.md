## オーケストレーション リファレンス

サブエージェントを呼び出すプロンプト（Command や Agent）の設計パターン。

### 基本構造

オーケストレーターは **呼び出しテンプレート** を持ち、サブエージェントに作業を委譲する。

```markdown
## Instructions

1. 分析フェーズ（自身で実行）
   - コードベースを調査
   - 問題を特定

2. 実装フェーズ（サブエージェントに委譲）
   以下の Task を並列起動:

   **コード品質チェック**:
   ```
   Task tool:
     subagent_type: code-reviewer
     prompt: "Review /path/to/src for quality issues. Report findings as structured list."
   ```

   **セキュリティチェック**:
   ```
   Task tool:
     subagent_type: security-reviewer
     prompt: "Scan /path/to/src for OWASP Top 10 vulnerabilities."
   ```

3. 統合フェーズ（自身で実行）
   - サブエージェントの結果を統合
   - レポート生成
```

### 責任分割の原則

| 層 | 責任 | 例 |
|----|------|-----|
| **オーケストレーター** | タスク固有のコンテキスト、分割戦略、結果統合 | "src/ を対象に品質チェック" |
| **サブエージェント** | 汎用的な専門処理 | "コードを分析し問題を特定" |

- オーケストレーターはプロジェクト固有の情報を持つ
- サブエージェントはプロジェクト非依存の汎用処理を実行

### 呼び出しテンプレートの書き方

必須要素:
1. **subagent_type**: 使用するエージェント名
2. **prompt**: 具体的な指示（入力と期待する出力を明示）
3. **model**: 必要に応じて指定（デフォルトは sonnet）

```
Task tool:
  subagent_type: typescript-pro
  model: sonnet
  prompt: |
    Refactor the authentication module at src/auth/.
    Requirements:
    - Extract token validation to separate utility
    - Add proper error types
    - Maintain backward compatibility
    Output: List of changed files with brief explanation.
```

### 並列 vs 逐次

| パターン | 使い分け |
|----------|----------|
| **並列** | 独立した複数タスク（品質+セキュリティ+パフォーマンス） |
| **逐次** | 前のタスクの出力が次の入力になる場合 |
| **ファンアウト→集約** | 複数サブエージェント→結果をオーケストレーターが統合 |

### 避けるべきパターン

- サブエージェント内から「オーケストレーター」「親タスク」を参照
- サブエージェントにプロジェクト固有のパスをハードコード
- 1つのサブエージェントに複数の責任を持たせる
