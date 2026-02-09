---
description: "Implement fixes for GitHub issues"
---

## Instructions

Fix the specified GitHub issue: **$ARGUMENTS**

1. **Issue Analysis** - `gh issue view $ARGUMENTS` で要件、再現手順、期待動作を確認
2. **Branch** - `git checkout -b fix/issue-$ARGUMENTS`
3. **Reproduce** - Issue の手順に従い再現を確認。テストケースを作成
4. **Root Cause** - コードベースを調査し根本原因を特定
5. **Implement** - 根本原因に対処する最小変更を実装。プロジェクト規約に従う
6. **Verify** - 新規テスト + 既存テストが全て通ることを確認
