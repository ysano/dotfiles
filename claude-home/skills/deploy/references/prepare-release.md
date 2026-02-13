# Prepare Release - リリース準備

新バージョンリリースを計画・準備・検証する包括的な手順。

## 1. リリース計画とバージョン決定

セマンティックバージョニングに従いバージョンを決定。

**バージョン規則**:
- MAJOR: 破壊的変更 (API 変更、非互換性)
- MINOR: 後方互換性のある新機能
- PATCH: バグ修正、セキュリティパッチ

```bash
# 現在バージョン確認
git describe --tags --abbrev=0

# コミット履歴からバージョン判断
git log $(git describe --tags --abbrev=0)..HEAD --oneline
```

## 2. リリース前チェックリスト

すべての準備が完了していることを確認。

**コード品質**:
- [ ] すべての PR がマージ済み
- [ ] すべてのテストがパス (ユニット、統合、E2E)
- [ ] コードカバレッジが基準を満たす (>90%)
- [ ] Linter エラーなし
- [ ] セキュリティスキャン完了

**ドキュメント**:
- [ ] API ドキュメント更新
- [ ] ユーザーガイド更新
- [ ] マイグレーションガイド作成 (破壊的変更時)
- [ ] README 更新

**インフラ**:
- [ ] DB マイグレーション確認
- [ ] 環境変数変更の文書化
- [ ] 監視アラート設定
- [ ] ロールバック手順確認

## 3. リリースブランチ作成

リリースブランチで最終準備。

```bash
# main から release ブランチ作成
git checkout main
git pull origin main
git checkout -b release/v1.3.0
```

## 4. バージョン番号更新

プロジェクトファイルのバージョンを更新。

```bash
# Node.js
npm version minor  # または major, patch

# Python (pyproject.toml)
poetry version minor

# 手動更新の場合
# package.json, __version__, go.mod 等を編集
```

## 5. CHANGELOG 生成と編集

変更履歴を生成・整理。

```bash
# Conventional Commits から自動生成
npx conventional-changelog -p angular -i CHANGELOG.md -s

# 手動編集で補足情報追加
# - Breaking Changes の詳細
# - マイグレーション手順
# - パフォーマンス改善の定量データ
```

**CHANGELOG 例**:
```markdown
## [1.3.0] - 2024-01-15

### Added
- OAuth2 authentication support
- Real-time notifications via WebSocket
- Export to PDF functionality

### Changed
- Improved API response format (see migration guide)
- Updated dashboard UI layout

### Fixed
- Memory leak in background worker
- Timezone handling in date picker

### Security
- Fixed XSS vulnerability in search input
```

## 6. リリースノート作成

ユーザー向けのリリースノートを作成。

```markdown
# Release Notes v1.3.0

## 🎉 Highlights
- New OAuth2 authentication for seamless third-party integration
- Real-time notifications keep you updated instantly
- Export your reports to PDF with one click

## 📝 What's New
### OAuth2 Authentication
Connect your account with Google, GitHub, or Microsoft.
Setup guide: [docs/oauth2-setup.md](docs/oauth2-setup.md)

### Real-time Notifications
Enable in Settings > Notifications to receive instant updates.

## 🐛 Bug Fixes
- Fixed memory leak affecting long-running sessions (#234)
- Resolved timezone issues in date picker (#245)

## ⚠️ Breaking Changes
### API Response Format
Old: `{ data: {...}, status: 200 }`
New: `{ result: {...}, meta: { status: 200 } }`

Migration: Update your API clients to use the new format.

## 📊 Performance Improvements
| Metric | Before | After | Improvement |
|--------|--------|-------|-------------|
| API Response Time | 450ms | 180ms | 60% faster |
| Bundle Size | 850KB | 420KB | 50% smaller |
```

## 7. 依存関係監査

セキュリティ脆弱性と互換性を確認。

```bash
# 依存関係の脆弱性スキャン
npm audit
npm audit fix

# 古い依存関係の更新
npm outdated
npm update

# Production 依存関係のみ確認
npm ci --only=production
```

## 8. ビルドとアーティファクト生成

本番用ビルドを作成。

```bash
# ビルド環境クリーンアップ
rm -rf dist/ build/ node_modules/
npm ci

# Production ビルド
NODE_ENV=production npm run build

# ビルド成果物の検証
ls -lh dist/
du -sh dist/

# アーティファクトの生成 (必要に応じて)
tar -czf myapp-v1.3.0.tar.gz dist/
```

## 9. ステージング環境デプロイと検証

ステージング環境で最終検証。

```bash
# ステージングデプロイ
./deploy-staging.sh v1.3.0

# スモークテスト実行
npm run test:smoke:staging

# 手動検証
# [ ] 重要なユーザーフロー動作確認
# [ ] 新機能動作確認
# [ ] パフォーマンス確認
# [ ] セキュリティ確認
```

## 10. タグ作成と GitHub Release

Git タグを作成し、GitHub Release を公開。

```bash
# コミットと署名付きタグ作成
git add .
git commit -m "chore(release): prepare v1.3.0"
git tag -a v1.3.0 -m "Release v1.3.0

- OAuth2 authentication
- Real-time notifications
- PDF export
- Performance improvements"

# Push
git push origin release/v1.3.0
git push origin v1.3.0

# GitHub Release 作成 (CLI)
gh release create v1.3.0 \
  --title "v1.3.0" \
  --notes-file RELEASE_NOTES.md \
  dist/myapp-v1.3.0.tar.gz
```

## 11. 本番デプロイ計画

デプロイウィンドウと戦略を計画。

**デプロイ戦略**:
- Blue-Green: ゼロダウンタイム、即時ロールバック可能
- Canary: 段階的ロールアウト、リスク最小化
- Rolling: 順次更新、リソース効率的

**デプロイウィンドウ**:
- 低トラフィック時間帯選定
- ステークホルダー通知
- サポートチーム待機

## 12. ポストリリース監視計画

リリース後の監視を準備。

**監視項目**:
- エラー率 (< 1%)
- レスポンスタイム (p95 < 500ms)
- スループット (SLA 基準)
- ユーザー報告 (サポートチケット)

**監視期間**: リリース後 24-48 時間は強化監視

## 関連リファレンス

- `setup-automated-releases.md` - 自動リリースワークフロー
- `add-changelog.md` - CHANGELOG 管理
- `rollback-deploy.md` - ロールバック手順
- `hotfix-deploy.md` - 緊急修正プロセス
