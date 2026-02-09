---
description: 未コミットのファイルを確認し、適切な粒度でコミットを作成する
allowed-tools: [Bash, Read, Edit, TodoWrite]
---

未コミットのファイルを分析し、論理的に関連する変更を適切な粒度でコミットします。

## 実行手順

1. **作業計画の作成**
   - TodoWriteツールで必要な作業をリスト化
   - 品質チェック、コード整理、コミット準備の各ステップを明確化

2. **品質チェック（コミット前必須）**

   **dotfiles固有の品質チェック**:
   - **Zsh**: `./test_zsh_config.zsh` - 構文、PATH、エイリアス検証
   - **Emacs**: `emacs --batch --eval "(byte-compile-file \"init.el\")"` - byte-compile
   - **tmux**: `.claude/skills/tmux-config/scripts/check_conflicts.sh` - 競合検証
   - **Karabiner**: `python3 -c "import json; json.load(open('karabiner/karabiner.json'))"` - JSON構文

   **汎用的な品質チェック**:
   - プロジェクト固有のコマンドを確認（`make lint`、`npm run check`等）
   - Linter/Formatter実行
   - 関連テスト実行（該当する場合）

3. **未コミット状況確認**
   - `git status`で変更されたファイルをリスト
   - `git diff --stat`で変更の統計情報を確認
   - `git diff`で具体的な変更内容を分析

4. **変更内容のグループ化**
   - 機能追加、バグ修正、リファクタリング、スタイル修正など論理的に分類
   - 関連するファイルをまとめてグループ化
   - 不要なデバッグコード・コメントの削除

5. **適切な粒度でコミット**
   - 各グループごとに独立したコミットを作成
   - Conventional Commits形式でメッセージ作成
   - 1コミット = 1つの論理的変更の原則

6. **品質確保**
   - コミットメッセージ検証が通ることを確認
   - 最終的な`git status`でクリーンな状態を確認

## コミットメッセージ形式

```
<type>(<scope>): <subject>

<body>
```

**Types**: feat, fix, refactor, docs, test, chore, style, perf

**重要**: Co-Authored-Byやツール言及は含めないこと

## 実行例

```bash
# 品質チェック（dotfiles）
./test_zsh_config.zsh
emacs --batch --eval "(byte-compile-file \"init.el\")"
.claude/skills/tmux-config/scripts/check_conflicts.sh

✅ すべての品質チェックツールがパス

# コミット作成例
git add .zshrc .zprofile
git commit -m "feat(zsh): Homebrewパス設定をmacOS/Linux両対応に変更"

git add .emacs.d/init.el .emacs.d/inits/init-org.el
git commit -m "feat(emacs): org-roamプラグインを追加"
```

## 注意事項

- **必ず品質チェックを実施**してからコミット
- **git add . / git add -A 禁止** - 明確なファイル指定のみ
- エラーや警告は可能な限り解消してからコミット
- 変更したファイルに対応するテストがある場合は必ず実行

**動作を開始します...**
