# Containerize Application - アプリケーションのコンテナ化

アプリケーションを Docker コンテナ化し、本番環境へのデプロイに備える手順。

## 1. アプリケーション分析とベースイメージ選定

アプリケーションの要件を分析し、最適なベースイメージを選定。

- ランタイム要件 (Node.js, Python, Go 等)
- 依存関係とシステムライブラリ
- セキュリティ要件 (最小攻撃面)
- イメージサイズ要件

**推奨ベースイメージ**: Alpine (最小サイズ), distroless (セキュリティ), slim variants (バランス)

## 2. マルチステージ Dockerfile 作成

ビルドと実行を分離した Dockerfile を作成。

```dockerfile
# Build stage
FROM node:20-alpine AS builder
WORKDIR /app
COPY package*.json ./
RUN npm ci --only=production
COPY . .
RUN npm run build

# Production stage
FROM node:20-alpine
RUN addgroup -g 1001 -S nodejs && adduser -S nodejs -u 1001
WORKDIR /app
COPY --from=builder --chown=nodejs:nodejs /app/dist ./dist
COPY --from=builder --chown=nodejs:nodejs /app/node_modules ./node_modules
USER nodejs
EXPOSE 3000
CMD ["node", "dist/index.js"]
```

**ポイント**: 非 root ユーザー実行、最小限のファイルコピー、セキュリティベストプラクティス

## 3. .dockerignore 設定

不要なファイルをビルドコンテキストから除外。

```
node_modules
npm-debug.log
.git
.env
*.md
tests/
.github/
```

## 4. ビルド最適化

イメージサイズとビルド時間を最適化。

- レイヤーキャッシュ活用 (COPY package.json → RUN npm install → COPY src)
- ビルド時依存関係のクリーンアップ
- 不要なファイルの削除
- マルチステージビルドでビルドツール除外

## 5. セキュリティハードニング

コンテナのセキュリティを強化。

- 非 root ユーザーで実行 (`USER nodejs`)
- 最小限の権限 (read-only ファイルシステム検討)
- ベースイメージの定期更新
- 脆弱性スキャン (Trivy, Snyk 等)
- シークレット管理 (環境変数、Docker Secrets)

## 6. ヘルスチェック設定

コンテナの健全性を監視。

```dockerfile
HEALTHCHECK --interval=30s --timeout=3s --start-period=5s \
  CMD node healthcheck.js || exit 1
```

## 7. docker-compose.yml 作成

ローカル開発環境を定義。

```yaml
version: '3.8'
services:
  app:
    build: .
    ports:
      - "3000:3000"
    environment:
      - NODE_ENV=development
    volumes:
      - .:/app
      - /app/node_modules
    depends_on:
      - db
  db:
    image: postgres:15-alpine
    environment:
      POSTGRES_DB: mydb
      POSTGRES_PASSWORD: secret
    volumes:
      - db_data:/var/lib/postgresql/data
volumes:
  db_data:
```

## 8. CI/CD 統合

コンテナイメージビルドを CI パイプラインに統合。

```yaml
- name: Build Docker Image
  run: docker build -t myapp:${{ github.sha }} .
- name: Scan Image
  run: trivy image myapp:${{ github.sha }}
- name: Push to Registry
  run: |
    docker tag myapp:${{ github.sha }} registry.example.com/myapp:${{ github.sha }}
    docker push registry.example.com/myapp:${{ github.sha }}
```

## 9. レジストリ管理

コンテナイメージをレジストリに保存。

- Docker Hub / GitHub Container Registry / AWS ECR / GCP GCR
- イメージタグ戦略 (git commit SHA, semantic version, latest)
- イメージの定期クリーンアップ
- アクセス制御とプライベートレジストリ

## 10. 本番環境デプロイ準備

本番環境での実行に備える。

- 環境変数とシークレット管理戦略
- ログ集約設定 (stdout/stderr)
- メトリクス公開 (Prometheus, StatsD)
- グレースフルシャットダウン実装
- リソース制限設定 (CPU, メモリ)

## 関連リファレンス

- `setup-kubernetes-deployment.md` - Kubernetes へのデプロイ
- `ci-setup.md` - CI パイプライン統合
- `hotfix-deploy.md` - 緊急イメージデプロイ
