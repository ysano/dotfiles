# Changelogコマンドの追加

業界標準に従ったプロジェクト用の包括的なchangelogを作成・維持します。

## 実行手順

以下の手順に従ってchangelogを設定・維持してください：**$ARGUMENTS**

1. **Changelogフォーマット（Keep a Changelog）**
   ```markdown
   # Changelog
   
   このプロジェクトのすべての注目すべき変更はこのファイルに記録されます。
   
   フォーマットは [Keep a Changelog](https://keepachangelog.com/en/1.0.0/) に基づいており、
   このプロジェクトは [Semantic Versioning](https://semver.org/spec/v2.0.0.html) に準拠しています。
   
   ## [Unreleased]
   ### Added
   - 新機能
   
   ### Changed
   - 既存機能の変更
   
   ### Deprecated
   - まもなく削除される機能
   
   ### Removed
   - 削除された機能
   
   ### Fixed
   - バグ修正
   
   ### Security
   - セキュリティ改善
   ```

2. **バージョンエントリ**
   ```markdown
   ## [1.2.3] - 2024-01-15
   ### Added
   - ユーザー認証システム
   - ダークモード切り替え
   - レポートのエクスポート機能
   
   ### Fixed
   - バックグラウンドタスクのメモリリーク
   - タイムゾーン処理の問題
   ```

3. **自動化ツール**
   ```bash
   # gitコミットからchangelogを生成
   npm install -D conventional-changelog-cli
   npx conventional-changelog -p angular -i CHANGELOG.md -s
   
   # Auto-changelog
   npm install -D auto-changelog
   npx auto-changelog
   ```

4. **Commitの規約**
   ```bash
   # 自動生成のためのConventional commits
   feat: add user authentication
   fix: resolve memory leak in tasks
   docs: update API documentation
   style: format code with prettier
   refactor: reorganize user service
   test: add unit tests for auth
   chore: update dependencies
   ```

5. **リリースとの統合**
   - 各リリース前にchangelogを更新
   - リリースノートに含める
   - GitHubリリースとのリンク
   - バージョンタグの一貫性

エントリは分かりやすく分類し、ユーザー向けの変更に焦点を当てて記録することを忘れずに。