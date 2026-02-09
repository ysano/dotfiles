---
description: Create a pull request for the current branch
allowed-tools: [Bash, Read]
---

現在のブランチからPull Requestを作成する。リポジトリのPR Templateを尊重する。

## 実行手順

1. **git status確認** - 未コミットの変更があれば先にコミット
2. **ブランチ状態確認** - 現在のブランチ名、リモートとの同期状態
3. **PR Template確認** - `.github/pull_request_template.md` の存在を確認し、内容を読み込む
4. **Push** - `git push -u origin <branch-name>`
5. **PR作成** - `unset GITHUB_TOKEN && gh pr create`
   - Template がある場合は各セクションを適切に埋める
   - タイトルは Conventional Commits 形式（feat/fix/refactor 等）
   - 該当しないセクションは "N/A" と記載（削除しない）

## 注意事項

- GITHUB_TOKEN 環境変数が設定されている場合は `unset GITHUB_TOKEN` を実行
- "Generated with Claude" 等のツール言及は削除
