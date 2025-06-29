# ブランチクリーンアップコマンド

あらゆるリポジトリでマージ済み、停滞、不要なgitブランチを安全にクリーンアップします。

## 実行手順

以下の体系的なアプローチに従ってgitブランチをクリーンアップしてください：**$ARGUMENTS**

1. **リポジトリ状態分析**
   - 現在のブランチと未コミット変更を確認
   - すべてのローカルおよびリモートブランチを一覧表示
   - main/masterブランチ名を特定
   - 最近のブランチ活動とマージ履歴をレビュー

   ```bash
   # Check current status
   git status
   git branch -a
   git remote -v
   
   # Check main branch name
   git symbolic-ref refs/remotes/origin/HEAD | sed 's@^refs/remotes/origin/@@'
   ```

2. **安全対策**
   - 作業ディレクトリがクリーンであることを確認
   - main/masterブランチに切り替え
   - リモートから最新の変更を取得
   - 必要に応じて現在のブランチ状態のバックアップを作成

   ```bash
   # Ensure clean state
   git stash push -m "Backup before branch cleanup"
   git checkout main  # or master
   git pull origin main
   ```

3. **マージ済みブランチの特定**
   - mainにマージされたブランチを一覧表示
   - 保護されたブランチ（main、master、develop）を除外
   - ローカルとリモートの両方のマージ済みブランチを確認
   - 誤削除を避けるためマージ状態を検証

   ```bash
   # List merged local branches
   git branch --merged main | grep -v "main\\|master\\|develop\\|\\*"
   
   # List merged remote branches
   git branch -r --merged main | grep -v "main\\|master\\|develop\\|HEAD"
   ```

4. **停滞ブランチの特定**
   - 最近活動のないブランチを発見
   - 各ブランチの最終コミット日を確認
   - 指定した期間（例：30日）より古いブランチを特定
   - feature/hotfixブランチの命名パターンを考慮

   ```bash
   # List branches by last commit date
   git for-each-ref --format='%(committerdate) %(authorname) %(refname)' --sort=committerdate refs/heads
   
   # Find branches older than 30 days
   git for-each-ref --format='%(refname:short) %(committerdate)' refs/heads | awk '$2 < "'$(date -d '30 days ago' '+%Y-%m-%d')'"'
   ```

5. **対話的ブランチレビュー**
   - 削除前に各ブランチをレビュー
   - ブランチに未マージの変更があるか確認
   - ブランチの目的と状態を検証
   - 削除前に確認を求める

   ```bash
   # Check for unmerged changes
   git log main..branch-name --oneline
   
   # Show branch information
   git show-branch branch-name main
   ```

6. **保護ブランチ設定**
   - 削除されるべきでないブランチを特定
   - 重要なブランチの保護ルールを設定
   - ブランチ保護ポリシーを文書化
   - 新しいリポジトリの自動保護を設定

   ```bash
   # Example protected branches
   PROTECTED_BRANCHES=("main" "master" "develop" "staging" "production")
   ```

7. **ローカルブランチクリーンアップ**
   - マージ済みローカルブランチを安全に削除
   - 停滞したfeatureブランチを削除
   - 削除されたリモートの追跡ブランチをクリーンアップ
   - ローカルブランチ参照を更新

   ```bash
   # Delete merged branches (interactive)
   git branch --merged main | grep -v "main\\|master\\|develop\\|\\*" | xargs -n 1 -p git branch -d
   
   # Force delete if needed (use with caution)
   git branch -D branch-name
   ```

8. **リモートブランチクリーンアップ**
   - マージ済みリモートブランチを削除
   - リモート追跡参照をクリーンアップ
   - 廃止されたリモートブランチを削除
   - リモートブランチ情報を更新

   ```bash
   # Prune remote tracking branches
   git remote prune origin
   
   # Delete remote branch
   git push origin --delete branch-name
   
   # Remove local tracking of deleted remote branches
   git branch -dr origin/branch-name
   ```

9. **自動クリーンアップスクリプト**
   
   ```bash
   #!/bin/bash
   
   # Git branch cleanup script
   set -e
   
   # Configuration
   MAIN_BRANCH="main"
   PROTECTED_BRANCHES=("main" "master" "develop" "staging" "production")
   STALE_DAYS=30
   
   # Functions
   is_protected() {
       local branch=$1
       for protected in "${PROTECTED_BRANCHES[@]}"; do
           if [[ "$branch" == "$protected" ]]; then
               return 0
           fi
       done
       return 1
   }
   
   # Switch to main branch
   git checkout $MAIN_BRANCH
   git pull origin $MAIN_BRANCH
   
   # Clean up merged branches
   echo "Cleaning up merged branches..."
   merged_branches=$(git branch --merged $MAIN_BRANCH | grep -v "\\*\\|$MAIN_BRANCH")
   
   for branch in $merged_branches; do
       if ! is_protected "$branch"; then
           echo "Deleting merged branch: $branch"
           git branch -d "$branch"
       fi
   done
   
   # Prune remote tracking branches
   echo "Pruning remote tracking branches..."
   git remote prune origin
   
   echo "Branch cleanup completed!"
   ```

10. **チーム調整**
    - 共有ブランチをクリーンアップする前にチームに通知
    - ブランチが他の人に使用されているか確認
    - ブランチクリーンアップスケジュールを調整
    - ブランチクリーンアップ手順を文書化

11. **ブランチ命名規則クリーンアップ**
    - 非標準命名のブランチを特定
    - 一時的または実験的ブランチをクリーンアップ
    - 古いhotfixとfeatureブランチを削除
    - 一貫した命名規則を強制

12. **検証と確認**
    - 重要なブランチがまだ存在することを確認
    - アクティブな作業が削除されていないことを確認
    - リモートブランチ同期を検証
    - チームメンバーに問題がないことを確認

    ```bash
    # Verify cleanup results
    git branch -a
    git remote show origin
    ```

13. **文書化とレポート**
    - クリーンアップされたブランチを文書化
    - 発見された問題や競合をレポート
    - ブランチライフサイクルに関するチーム文書を更新
    - ブランチクリーンアップスケジュールとポリシーを作成

14. **ロールバック手順**
    - 削除されたブランチの回復方法を文書化
    - reflogを使用して削除されたブランチのコミットを発見
    - 緊急回復手順を作成
    - ブランチ復元スクリプトを設定

    ```bash
    # Recover deleted branch using reflog
    git reflog --no-merges --since="2 weeks ago"
    git checkout -b recovered-branch commit-hash
    ```

15. **自動化設定**
    - 自動ブランチクリーンアップスクリプトを設定
    - ブランチクリーンアップ用のCI/CDパイプラインを設定
    - スケジュール化されたクリーンアップジョブを作成
    - ブランチライフサイクルポリシーを実装

16. **ベストプラクティス実装**
    - ブランチライフサイクルガイドラインを確立
    - 自動マージ検出を設定
    - ブランチ保護ルールを設定
    - コードレビュー要件を実装

**高度なクリーンアップオプション：**

```bash
# Clean up all merged branches except protected ones
git branch --merged main | grep -E "^  (feature|hotfix|bugfix)/" | xargs -n 1 git branch -d

# Interactive cleanup with confirmation
git branch --merged main | grep -v "main\|master\|develop" | xargs -n 1 -p git branch -d

# Batch delete remote branches
git branch -r --merged main | grep origin | grep -v "main\|master\|develop\|HEAD" | cut -d/ -f2- | xargs -n 1 git push origin --delete

# Clean up branches older than specific date
git for-each-ref --format='%(refname:short) %(committerdate:short)' refs/heads | awk '$2 < "2023-01-01"' | cut -d' ' -f1 | xargs -n 1 git branch -D
```

以下を忘れずに実行してください：
- クリーンアップ前に重要なブランチを常にバックアップ
- 共有ブランチを削除する前にチームメンバーと調整
- 安全な環境でクリーンアップスクリプトを最初にテスト
- すべてのクリーンアップ手順とポリシーを文書化
- 蓄積を防ぐために定期的なクリーンアップスケジュールを設定