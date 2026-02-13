# Simulation Commands

AI agents を reality simulator として活用 - 実行から指数的な意思決定価値へ。

> **Knowledge Base**: 各コマンドの詳細な手順は `simulation` Skill (`skills/simulation/`) に集約されている。
> コマンドは Skill への薄いエントリーポイントとして機能する。

## 哲学

従来の AI agents は実行に焦点を当てる (10分のメールを0分に)。
これらのシミュレーションコマンドは、AI agents を **world modelers** として使用することで指数的レバレッジを解放する。

コアコンセプト: `LLM + Tools + Guidance + Simulated World = Reality Simulator`

## Available Commands

### コアシミュレーションツール
- **business-scenario-explorer.md** - 制約検証付き複数タイムラインビジネス探索
- **digital-twin-creator.md** - データ品質チェック付きデジタルツイン作成
- **decision-tree-explorer.md** - 確率重み付け意思決定ブランチ分析
- **market-response-modeler.md** - セグメント分析付き顧客/市場反応シミュレーション

### 高度なモデリング
- **timeline-compressor.md** - 信頼区間付き加速シナリオテスト
- **constraint-modeler.md** - 前提検証付き世界制約モデリング
- **future-scenario-generator.md** - 妥当性スコアリング付きシナリオ生成
- **simulation-calibrator.md** - シミュレーション精度テストと改良

### ドキュメント
- **SIMULATION_EXAMPLES.md** - 包括的な例と使用パターン

## はじめに

1. **シミュレーション初心者?** `SIMULATION_EXAMPLES.md` で実践例を確認
2. **制約を理解** - `constraint-modeler` で意思決定環境をマッピング
3. **前提をテスト** - `simulation-calibrator` でデータ品質を検証
4. **特定ツールを適用** - シナリオに基づいてコマンドを選択
5. **反復と改良** - 実世界フィードバックに基づき改善

## Related

- **Skill**: `simulation` - 手順詳細、シミュレーション種別クイックリファレンス、価値レバー
- **Agent**: `strategy-architect` - ビジネス・技術シナリオモデリング専門エージェント
- **Agent**: `decision-navigator` - WFGY semantic validation による戦略的意思決定専門エージェント