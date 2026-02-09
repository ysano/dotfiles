---
description: "Generate and post technical work logs to Linear issues from git commits"
---

## Instructions

Generate a work log from git commits and post it to the specified Linear issue: **$ARGUMENTS**

1. **Linear MCP 確認**
   - `mcp__linear__*` ツールの利用可否を確認
   - 未導入時はインストール手順を案内して終了

2. **Git 情報の抽出**
   - `git log main..HEAD --oneline` でブランチ固有のコミットを取得
   - `git show --stat [hash]` で各コミットの変更詳細を取得
   - 既存の work log がある場合は差分のみ対象

3. **Work Log 生成**
   - 客観的・技術的な言語（形容詞・絵文字なし）
   - 以下の形式で構造化:

   ```
   ## Work Completed [TODAY'S DATE]

   ### Branch: [branch-name]

   **Commit [short-hash]: [Title]**
   - [Technical detail]
   - [Line count] lines across [file count] files

   ### Status
   - [Current status summary]
   ```

4. **Linear に投稿**
   - 今日の既存 work log がある場合は置換、なければ新規作成
   - 投稿成功を確認して報告
