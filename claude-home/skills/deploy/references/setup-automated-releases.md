# Setup Automated Releases - 自動リリースワークフロー構築

Conventional Commits とセマンティックバージョニングに基づく自動リリースシステムの構築手順。

## 1. リポジトリ分析とバージョン管理

プロジェクトタイプに応じたバージョン管理方法を決定。

- Node.js: `package.json` の `version` フィールド
- Python: `__init__.py` の `__version__` または `pyproject.toml`
- Go: `go.mod` のバージョン
- その他: `version.txt` ファイル

## 2. Conventional Commits ガイドライン作成

`CONTRIBUTING.md` にコミット規約を記載。

```markdown
# Commit Message Format

- `feat:` 新機能 (MINOR バージョン)
- `fix:` バグ修正 (PATCH バージョン)
- `feat!:` または `BREAKING CHANGE:` 破壊的変更 (MAJOR バージョン)
- `docs:`, `chore:`, `style:`, `refactor:`, `test:` リリースなし

例: `feat(auth): add OAuth2 support`
```

## 3. Pull Request テンプレート作成

`.github/pull_request_template.md` を作成。

```markdown
## Description
<!-- 変更内容を記載 -->

## Type of Change
- [ ] `feat:` 新機能
- [ ] `fix:` バグ修正
- [ ] `docs:` ドキュメント
- [ ] `chore:` その他

## Checklist
- [ ] Conventional Commits 形式に従っている
- [ ] テストを追加/更新した
- [ ] ドキュメントを更新した
```

## 4. リリースワークフロー作成

`.github/workflows/release.yml` を作成。

```yaml
name: Release
on:
  push:
    branches: [main]

jobs:
  release:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4
        with:
          fetch-depth: 0

      - uses: actions/setup-node@v4
        with:
          node-version: '20'

      - name: Install Dependencies
        run: npm ci

      - name: Determine Version Bump
        id: version
        run: |
          # 前回リリース以降のコミット分析
          COMMITS=$(git log $(git describe --tags --abbrev=0)..HEAD --pretty=format:"%s")
          if echo "$COMMITS" | grep -q "BREAKING CHANGE:\|!:"; then
            echo "bump=major" >> $GITHUB_OUTPUT
          elif echo "$COMMITS" | grep -q "^feat"; then
            echo "bump=minor" >> $GITHUB_OUTPUT
          else
            echo "bump=patch" >> $GITHUB_OUTPUT
          fi

      - name: Bump Version
        run: npm version ${{ steps.version.outputs.bump }} --no-git-tag-version

      - name: Generate Changelog
        run: npx conventional-changelog -p angular -i CHANGELOG.md -s

      - name: Commit Changes
        run: |
          VERSION=$(node -p "require('./package.json').version")
          git config user.name "GitHub Actions"
          git config user.email "actions@github.com"
          git add .
          git commit -m "chore(release): v${VERSION}"
          git tag "v${VERSION}"
          git push && git push --tags

      - name: Create GitHub Release
        uses: softprops/action-gh-release@v1
        with:
          tag_name: v${{ steps.version.outputs.version }}
          generate_release_notes: true
```

## 5. PR 検証ワークフロー作成

`.github/workflows/pr-check.yml` を作成。

```yaml
name: PR Check
on:
  pull_request:
    types: [opened, edited, synchronize]

jobs:
  validate:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4

      - name: Validate PR Title
        uses: amannn/action-semantic-pull-request@v5
        env:
          GITHUB_TOKEN: ${{ secrets.GITHUB_TOKEN }}
```

## 6. GitHub Release Notes 設定

`.github/release.yml` を作成。

```yaml
changelog:
  categories:
    - title: 🎉 New Features
      labels: [feat, enhancement]
    - title: 🐛 Bug Fixes
      labels: [fix, bug]
    - title: 📚 Documentation
      labels: [docs, documentation]
    - title: 🔒 Security
      labels: [security]
  exclude:
    labels: [chore, dependencies]
```

## 7. CHANGELOG.md 初期化

Keep a Changelog 形式で CHANGELOG.md を作成。

```markdown
# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

## [1.0.0] - 2024-01-15
### Added
- Initial release
```

## 8. README へのバッジ追加

リリース情報を可視化。

```markdown
![Version](https://img.shields.io/github/v/release/user/repo)
![Build](https://img.shields.io/github/actions/workflow/status/user/repo/release.yml)
```

## 9. ブランチ保護設定

GitHub リポジトリ設定で推奨事項を設定。

- Require PR reviews
- Require status checks to pass
- Require branches to be up to date
- Require conventional commit format in PR titles

## 10. システムテスト

リリースワークフローをテスト。

```bash
# テスト用 PR 作成
git checkout -b feat/test-release
echo "// test" >> index.js
git add .
git commit -m "feat: test automated release"
git push origin feat/test-release

# PR マージ後、自動リリース確認
```

## 関連リファレンス

- `prepare-release.md` - リリース準備詳細
- `add-changelog.md` - CHANGELOG 管理
- `ci-setup.md` - CI パイプライン統合
