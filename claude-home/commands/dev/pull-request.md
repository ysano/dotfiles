---
description: Create a pull request for the current branch
allowed-tools: [Bash, Read]
---

現在のブランチからPull Requestを作成します。

## 実行手順

1. **git status確認**
   - 未コミットの変更確認
   - 必要ならコミット作成

2. **ブランチ状態確認**
   - 現在のブランチ名確認
   - リモートとの同期状態確認

3. **Push実行**
   - `git push -u origin <branch-name>`

4. **PR作成**
   - `gh pr create`でPull Request作成
   - タイトルと説明を簡潔に記載
   - "Generated with Claude"のようなメッセージは含めない

## 実行例

```bash
# 状態確認
git status
git branch --show-current

# Push
git push -u origin feature-branch

# PR作成
unset GITHUB_TOKEN && gh pr create --title "feat: 新機能追加" --body "## Summary
- 機能の簡潔な説明
- 変更内容の要点"
```

## 注意事項

- GITHUB_TOKEN環境変数が設定されている場合は`unset GITHUB_TOKEN`を実行
- gh authで認証済みであることを確認
- PRの説明に"Generated with Claude"等のメッセージを含めない

**動作を開始します...**