---
description: Create a pull request for the current branch
allowed-tools: [Bash, Read]
---

現在のブランチからPull Requestを作成します。リポジトリのPR Templateを尊重します。

## 実行手順

1. **git status確認**
   - 未コミットの変更確認
   - 必要ならコミット作成

2. **ブランチ状態確認**
   - 現在のブランチ名確認
   - リモートとの同期状態確認

3. **PR Template確認**
   - `.github/pull_request_template.md`の存在確認
   - テンプレートがある場合、内容を読み込んで理解

4. **Push実行**
   - `git push -u origin <branch-name>`

5. **PR作成**
   - `gh pr create`で対話的にPR作成
   - PR Templateが自動適用される
   - テンプレートの各セクションを適切に埋める
   - タイトルはConventional Commits形式推奨

## 実行例

### PR Templateが存在する場合

```bash
# PR Template確認
cat .github/pull_request_template.md

# 状態確認
git status
git branch --show-current

# Push
git push -u origin feature-branch

# PR作成（対話的、テンプレート自動適用）
unset GITHUB_TOKEN && gh pr create

# または、タイトルのみ指定してエディタで本文編集
unset GITHUB_TOKEN && gh pr create --title "feat: 新機能追加" --web
```

### PR Templateがない場合

```bash
# Push
git push -u origin feature-branch

# PR作成（シンプル）
unset GITHUB_TOKEN && gh pr create --title "feat: 新機能追加" --body "## Summary
- 変更内容の要点

## Test Plan
- テスト手順"
```

## PR Template活用

リポジトリに`.github/pull_request_template.md`が存在する場合:

1. **`gh pr create`実行時に自動適用**
   - テンプレートがエディタで開かれる
   - 各セクションを適切に埋める

2. **主要セクション**（dotfilesリポジトリの例）
   - Description: 変更の簡潔な説明
   - Type of Change: Bug fix/New feature/Refactoring等
   - Changes Made: 変更ファイルチェックリスト
   - Testing: テスト実施内容、プラットフォーム
   - Platform Testing: macOS/Linux/WSL確認状況
   - Checklist: コード品質、テスト、セキュリティ

3. **注意事項**
   - 該当しないセクションは削除せず、"N/A"または"該当なし"と記載
   - チェックリストは`[x]`で完了マーク
   - "Generated with Claude"等のツール言及は削除

## 注意事項

- GITHUB_TOKEN環境変数が設定されている場合は`unset GITHUB_TOKEN`を実行
- gh authで認証済みであることを確認
- PR Templateを尊重し、すべてのセクションを適切に埋める
- タイトルはConventional Commits形式推奨（feat/fix/refactor/docs等）

**動作を開始します...**