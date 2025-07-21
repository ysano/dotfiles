# CI/CDセットアップコマンド

あらゆるプロジェクト向けの包括的な継続的統合・継続的デプロイメントパイプラインを設定します。

## 実行手順

以下の体系的なアプローチに従ってCI/CDを実装してください：**$ARGUMENTS**

1. **プロジェクト分析**
   - 技術スタックとデプロイメント要件を識別
   - 既存のビルドおよびテストプロセスをレビュー
   - デプロイメント環境（dev、staging、prod）を理解
   - 現在のバージョン管理とブランチ戦略を評価

2. **CI/CDプラットフォーム選択**
   - 要件に基づいて適切なCI/CDプラットフォームを選択：
     - **GitHub Actions**: ネイティブGitHub統合、豊富なマーケットプレイス
     - **GitLab CI**: GitLab組み込み、包括的DevOpsプラットフォーム
     - **Jenkins**: セルフホスト、高度にカスタマイズ可能、豊富なプラグイン
     - **CircleCI**: クラウドベース、速度最適化
     - **Azure DevOps**: Microsoftエコシステム統合
     - **AWS CodePipeline**: AWSネイティブソリューション

3. **リポジトリセットアップ**
   - 適切な`.gitignore`設定を確保
   - ブランチ保護ルールを設定
   - マージ要件とレビューを設定
   - セマンティックバージョニング戦略を確立

4. **ビルドパイプライン設定**
   
   **GitHub Actions Example:**
   ```yaml
   name: CI/CD Pipeline
   
   on:
     push:
       branches: [ main, develop ]
     pull_request:
       branches: [ main ]
   
   jobs:
     test:
       runs-on: ubuntu-latest
       steps:
         - uses: actions/checkout@v3
         - name: Setup Node.js
           uses: actions/setup-node@v3
           with:
             node-version: '18'
             cache: 'npm'
         - run: npm ci
         - run: npm run test
         - run: npm run build
   ```

   **GitLab CI Example:**
   ```yaml
   stages:
     - test
     - build
     - deploy
   
   test:
     stage: test
     script:
       - npm ci
       - npm run test
     cache:
       paths:
         - node_modules/
   ```

5. **環境設定**
   - 環境変数とシークレットを設定
   - 異なる環境（dev、staging、prod）を設定
   - 環境固有の設定を実装
   - セキュアなシークレット管理を設定

6. **自動テスト統合**
   - ユニットテスト実行を設定
   - 統合テスト実行を設定
   - E2Eテスト実行を実装
   - テストレポートとカバレッジを設定

   **Multi-stage Testing:**
   ```yaml
   test:
     strategy:
       matrix:
         node-version: [16, 18, 20]
     runs-on: ubuntu-latest
     steps:
       - uses: actions/checkout@v3
       - uses: actions/setup-node@v3
         with:
           node-version: ${{ matrix.node-version }}
       - run: npm ci
       - run: npm test
   ```

7. **コード品質ゲート**
   - リンティングとフォーマットチェックを統合
   - 静的コード解析（SonarQube、CodeClimate）を設定
   - セキュリティ脆弱性スキャンを設定
   - コードカバレッジ閾値を実装

8. **ビルド最適化**
   - ビルドキャッシュ戦略を設定
   - 並列ジョブ実行を実装
   - Dockerイメージビルドを最適化
   - アーティファクト管理を設定

   **Caching Example:**
   ```yaml
   - name: Cache node modules
     uses: actions/cache@v3
     with:
       path: ~/.npm
       key: ${{ runner.os }}-node-${{ hashFiles('**/package-lock.json') }}
       restore-keys: |
         ${{ runner.os }}-node-
   ```

9. **Docker統合**
   - 最適化されたDockerfileを作成
   - マルチステージビルドを設定
   - コンテナレジストリ統合を設定
   - イメージのセキュリティスキャンを実装

   **Multi-stage Dockerfile:**
   ```dockerfile
   FROM node:18-alpine AS builder
   WORKDIR /app
   COPY package*.json ./
   RUN npm ci --only=production
   
   FROM node:18-alpine AS runtime
   WORKDIR /app
   COPY --from=builder /app/node_modules ./node_modules
   COPY . .
   EXPOSE 3000
   CMD ["npm", "start"]
   ```

10. **デプロイメント戦略**
    - ブルーグリーンデプロイメントを実装
    - カナリアリリースを設定
    - ローリングアップデートを設定
    - フィーチャーフラグ統合を実装

11. **Infrastructure as Code**
    - Terraform、CloudFormation、または類似ツールを使用
    - インフラストラクチャ定義をバージョン管理
    - インフラストラクチャテストを実装
    - 自動インフラストラクチャプロビジョニングを設定

12. **モニタリングと可観測性**
    - アプリケーションパフォーマンスモニタリングを設定
    - ログ集約と分析を設定
    - ヘルスチェックとアラートを実装
    - デプロイメント通知を設定

13. **セキュリティ統合**
    - 依存関係脆弱性スキャンを実装
    - コンテナセキュリティスキャンを設定
    - SAST（静的アプリケーションセキュリティテスト）を設定
    - シークレットスキャンを実装

   **Security Scanning Example:**
   ```yaml
   security:
     runs-on: ubuntu-latest
     steps:
       - uses: actions/checkout@v3
       - name: Run Snyk to check for vulnerabilities
         uses: snyk/actions/node@master
         env:
           SNYK_TOKEN: ${{ secrets.SNYK_TOKEN }}
   ```

14. **データベースマイグレーション処理**
    - データベーススキーママイグレーションを自動化
    - ロールバック戦略を実装
    - テスト用データベースシーディングを設定
    - バックアップとリカバリ手順を設定

15. **パフォーマンステスト統合**
    - パイプラインに負荷テストを設定
    - パフォーマンスベンチマークを設定
    - パフォーマンス回帰検出を実装
    - パフォーマンスモニタリングを設定

16. **マルチ環境デプロイメント**
    - ステージング環境デプロイメントを設定
    - 承認付きプロダクション環境デプロイメントを設定
    - 環境昇格ワークフローを実装
    - 環境固有の設定を構成

   **Environment Deployment:**
   ```yaml
   deploy-staging:
     needs: test
     if: github.ref == 'refs/heads/develop'
     runs-on: ubuntu-latest
     steps:
       - name: Deploy to staging
         run: |
           # Deploy to staging environment
   
   deploy-production:
     needs: test
     if: github.ref == 'refs/heads/main'
     runs-on: ubuntu-latest
     environment: production
     steps:
       - name: Deploy to production
         run: |
           # Deploy to production environment
   ```

17. **ロールバックとリカバリ**
    - 自動ロールバック手順を実装
    - デプロイメント検証テストを設定
    - 障害検出とアラートを設定
    - 手動リカバリ手順をドキュメント化

18. **通知とレポート**
    - 通知用のSlack/Teams統合を設定
    - 障害時のメールアラートを設定
    - デプロイメントステータスレポートを実装
    - メトリクスダッシュボードを設定

19. **コンプライアンスと監査**
    - デプロイメント監査証跡を実装
    - コンプライアンスチェック（SOC 2、HIPAA等）を設定
    - センシティブなデプロイメントの承認ワークフローを設定
    - 変更管理プロセスをドキュメント化

20. **パイプライン最適化**
    - パイプラインパフォーマンスとコストを監視
    - パイプライン並列化を実装
    - リソース配分を最適化
    - パイプライン分析とレポートを設定

**ベストプラクティス：**

1. **早期失敗**: 早期の障害検出を実装
2. **並列実行**: 独立したジョブを並列実行
3. **キャッシュ**: 依存関係とビルドアーティファクトをキャッシュ
4. **セキュリティ**: ログにシークレットを公開しない
5. **ドキュメント**: パイプラインプロセスと手順をドキュメント化
6. **モニタリング**: パイプラインの健全性とパフォーマンスを監視
7. **テスト**: フィーチャーブランチでパイプライン変更をテスト
8. **ロールバック**: 常にロールバック戦略を用意

**Sample Complete Pipeline:**
```yaml
name: Full CI/CD Pipeline

on:
  push:
    branches: [ main, develop ]
  pull_request:
    branches: [ main ]

jobs:
  lint-and-test:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3
      - uses: actions/setup-node@v3
        with:
          node-version: '18'
          cache: 'npm'
      - run: npm ci
      - run: npm run lint
      - run: npm run test:coverage
      - run: npm run build

  security-scan:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3
      - name: Security scan
        run: npm audit --audit-level=high

  deploy-staging:
    needs: [lint-and-test, security-scan]
    if: github.ref == 'refs/heads/develop'
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3
      - name: Deploy to staging
        run: echo "Deploying to staging"

  deploy-production:
    needs: [lint-and-test, security-scan]
    if: github.ref == 'refs/heads/main'
    runs-on: ubuntu-latest
    environment: production
    steps:
      - uses: actions/checkout@v3
      - name: Deploy to production
        run: echo "Deploying to production"
```

基本的なCIから始めて、チームとプロジェクトの成熟に合わせて段階的により高度な機能を追加してください。