---
description: "Show detailed git repository status"
---

## 実行手順

Analyze the current state of the git repository:

1. **Status** - `git status` で変更状態、`git branch --show-current` で現在のブランチを確認
2. **Diff** - `git diff HEAD origin/main` でリモートとの差分を確認
3. **Summary** - 現在のブランチ、ahead/behind の状態、変更ファイル一覧、必要なアクション（commit/pull 等）を報告
