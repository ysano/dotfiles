# Add Changelog - CHANGELOG 管理

プロジェクトの変更履歴を Keep a Changelog 形式で管理する手順。

## 1. CHANGELOG.md 初期化

Keep a Changelog 形式で CHANGELOG.md を作成。

```markdown
# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]
### Added
- 新機能をここに記載

### Changed
- 既存機能の変更

### Deprecated
- 将来削除予定の機能

### Removed
- 削除された機能

### Fixed
- バグ修正

### Security
- セキュリティ改善

## [1.0.0] - 2024-01-15
### Added
- Initial release
```

## 2. バージョンエントリの記載方法

リリース時にバージョンセクションを追加。

```markdown
## [1.2.3] - 2024-01-15

### Added
- ユーザー認証システム追加
- ダークモードトグル実装
- レポートエクスポート機能追加

### Changed
- API レスポンス形式改善
- UI レイアウト調整

### Fixed
- バックグラウンドタスクのメモリリーク修正
- タイムゾーン処理の不具合修正
```

**ポイント**:
- ユーザー視点で記載 (実装詳細ではなく、機能・改善を説明)
- 具体的に記載 (「いくつかのバグ修正」ではなく個別に列挙)
- Issue 番号参照 (例: `- メモリリーク修正 (#123)`)

## 3. Conventional Commits からの自動生成

Git コミット履歴から自動生成。

```bash
# conventional-changelog のインストール
npm install -D conventional-changelog-cli

# CHANGELOG.md 生成
npx conventional-changelog -p angular -i CHANGELOG.md -s

# または auto-changelog
npm install -D auto-changelog
npx auto-changelog
```

## 4. package.json にスクリプト追加

CHANGELOG 生成を簡略化。

```json
{
  "scripts": {
    "changelog": "conventional-changelog -p angular -i CHANGELOG.md -s",
    "changelog:all": "conventional-changelog -p angular -i CHANGELOG.md -s -r 0"
  }
}
```

## 5. リリースプロセスへの統合

リリースワークフローに CHANGELOG 更新を組み込む。

```yaml
# .github/workflows/release.yml
- name: Generate Changelog
  run: npm run changelog

- name: Commit Changelog
  run: |
    git config user.name "GitHub Actions"
    git config user.email "actions@github.com"
    git add CHANGELOG.md
    git commit -m "chore: update CHANGELOG for v${{ steps.version.outputs.version }}"
```

## 6. GitHub Releases との連携

GitHub Releases に CHANGELOG を含める。

```yaml
- name: Create GitHub Release
  uses: softprops/action-gh-release@v1
  with:
    tag_name: v${{ steps.version.outputs.version }}
    body_path: CHANGELOG.md
    generate_release_notes: true
```

## 7. 手動メンテナンス

自動生成されない重要な変更を手動追加。

- Breaking Changes の詳細な説明
- マイグレーションガイド
- セキュリティ脆弱性の修正
- パフォーマンス改善の定量的データ

## 8. リンク管理

バージョン比較リンクを CHANGELOG 末尾に追加。

```markdown
[Unreleased]: https://github.com/user/repo/compare/v1.2.3...HEAD
[1.2.3]: https://github.com/user/repo/compare/v1.2.2...v1.2.3
[1.2.2]: https://github.com/user/repo/compare/v1.2.1...v1.2.2
```

## 9. 多言語対応 (オプション)

国際的なプロジェクトの場合、複数言語の CHANGELOG を提供。

```
CHANGELOG.md (English)
CHANGELOG.ja.md (日本語)
```

## 10. 検証とレビュー

CHANGELOG の品質を確認。

- ユーザー視点で読みやすいか
- すべての重要な変更が含まれているか
- リンクが正しく動作するか
- セマンティックバージョニングに従っているか

## 関連リファレンス

- `setup-automated-releases.md` - 自動リリースワークフロー
- `prepare-release.md` - リリース準備プロセス
