---
description: "Clean up merged and stale git branches"
---

## Instructions

Git ブランチのクリーンアップを実行: **$ARGUMENTS**

### 1. Safety Check

- 現在のブランチと未コミット変更を確認
- main/master ブランチ名を特定
- main に切り替えて `git pull` で最新化

### 2. Merged Branches の特定

```bash
git branch --merged main | grep -v "main\|master\|develop\|\*"
```

保護対象ブランチ（main, master, develop, staging, production）は除外。

### 3. Stale Branches の特定

```bash
git for-each-ref --format='%(refname:short) %(committerdate:short)' refs/heads --sort=committerdate
```

未マージの変更がないか `git log main..branch-name --oneline` で確認。

### 4. Interactive Cleanup

- 各ブランチの削除前にユーザーへ確認
- ローカル: `git branch -d branch-name`
- リモート: `git push origin --delete branch-name`（確認後）
- `git remote prune origin` で stale tracking branches を整理

### 5. Report

削除したブランチと保持したブランチの一覧を表示。
