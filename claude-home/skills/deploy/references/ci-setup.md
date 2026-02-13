# CI Setup - 継続的インテグレーションパイプライン構築

プロジェクトの継続的インテグレーション (CI) パイプラインを構築する手順。

## 1. プロジェクト分析とプラットフォーム選定

プロジェクトの技術スタックと要件を分析し、適切な CI プラットフォームを選定する。

**主要プラットフォーム**:
- GitHub Actions: GitHub ネイティブ、マーケットプレイス豊富
- GitLab CI: GitLab 統合、包括的 DevOps プラットフォーム
- Jenkins: セルフホスト、高度なカスタマイズ性
- CircleCI / Azure DevOps / AWS CodePipeline

**選定基準**: リポジトリプラットフォーム、コスト、並列実行、セキュリティ、既存インフラとの統合

## 2. リポジトリ設定

ブランチ保護とマージ要件を設定。

- `.gitignore` 適切な設定 (ビルド成果物、シークレット除外)
- ブランチ保護ルール (main/master への直接 push 禁止)
- PR レビュー必須化、CI チェック必須化
- セマンティックバージョニング戦略確立

## 3. パイプライン構成定義

基本的な CI パイプラインを定義。GitHub Actions の例:

```yaml
name: CI Pipeline
on:
  push:
    branches: [main, develop]
  pull_request:
    branches: [main]

jobs:
  test:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4
      - uses: actions/setup-node@v4
        with:
          node-version: '20'
          cache: 'npm'
      - run: npm ci
      - run: npm test
      - run: npm run lint
```

## 4. 環境変数とシークレット管理

CI 環境で必要な設定を管理。

- GitHub Secrets / GitLab CI Variables でシークレット登録
- 環境別変数分離 (dev, staging, prod)
- API キー、データベース接続文字列、認証トークンを安全に管理
- **重要**: シークレットをログに出力しない

## 5. 自動テスト統合

包括的なテストを CI パイプラインに統合。

- ユニットテスト実行
- 統合テスト実行
- E2E テスト実行 (必要に応じて)
- テストカバレッジ計測とレポート生成
- テスト失敗時にパイプライン停止

```yaml
- name: Run Tests with Coverage
  run: npm test -- --coverage
- name: Upload Coverage
  uses: codecov/codecov-action@v3
```

## 6. コード品質ゲート設定

コード品質を自動チェック。

- Linter 実行 (ESLint, Pylint, RuboCop 等)
- Formatter チェック (Prettier, Black 等)
- 静的解析ツール統合 (SonarQube, CodeClimate)
- 型チェック (TypeScript, MyPy 等)
- コードカバレッジ閾値設定

## 7. ビルド最適化

ビルド時間を短縮。

- 依存関係キャッシュ (npm, pip, Maven 等)
- 並列ジョブ実行
- Docker レイヤーキャッシュ
- ビルド成果物アーティファクト管理

```yaml
- uses: actions/cache@v3
  with:
    path: ~/.npm
    key: ${{ runner.os }}-node-${{ hashFiles('**/package-lock.json') }}
    restore-keys: |
      ${{ runner.os }}-node-
```

## 8. セキュリティスキャン統合

セキュリティ脆弱性を自動検出。

- 依存関係脆弱性スキャン (npm audit, Snyk, Dependabot)
- コンテナイメージスキャン (Trivy, Grype)
- SAST (Static Application Security Testing) 統合
- シークレットスキャン (GitGuardian, Gitleaks)

```yaml
- name: Security Scan
  run: |
    npm audit --audit-level=high
    npx snyk test
```

## 9. 通知設定

ビルド結果を関係者に通知。

- Slack / Microsoft Teams 統合
- メール通知 (失敗時のみ)
- GitHub Commit Status / PR コメント
- メトリクスダッシュボード連携

## 10. ドキュメント作成

CI パイプラインをドキュメント化。

- README にビルドバッジ追加
- CI 設定の説明とトラブルシューティング
- ローカルでの CI 再現手順
- 新規メンバー向けオンボーディングガイド

## 関連リファレンス

- `containerize-application.md` - Docker イメージビルド統合
- `setup-automated-releases.md` - リリース自動化
- `hotfix-deploy.md` - 緊急デプロイプロセス
