**dotfilesトラブルシューティング**

> `.claude/` モジュール使用時の一般的な問題と解決方法

## Q1: dotfiles-engineerがSkillを読み込まない

### 症状
- エージェントが既存パターンを無視して実装
- Conventionsに違反したコードが生成される

### 解決方法

```bash
# Skill存在確認
ls -la .claude/skills/*/SKILL.md

# 明示的にSkill起動後にエージェント起動
Skill: emacs-config
Task: dotfiles-engineer
# プロンプト: "org-roamプラグイン追加"
```

### 原因
- Agentが自動的にSkillを読み込まない場合がある
- Skill名の指定ミス

---

## Q2: /dev:commit が品質チェックをスキップする

### 症状
- コミット作成時に品質チェックが実行されない
- テスト未実行でコミットされる

### 解決方法

```bash
# コマンドファイル確認
Read: .claude/commands/dev/commit.md
# → 品質チェックセクションの存在確認

# 手動で品質チェック実行
Bash: ./test_zsh_config.zsh
Bash: emacs --batch --eval "(byte-compile-file \"init.el\")"
```

### 原因
- Command定義が古い可能性
- プロジェクト固有の品質チェックが未定義

---

## Q3: 設定変更後に動作しない

### 症状
- 設定ファイル変更後、期待通りに動作しない
- エラーメッセージなし

### 解決方法

```bash
# 検証エージェント起動
Task: dotfiles-validator
# プロンプト: "最後の変更の構文エラーを検証"

# ログ確認
# Zsh:   tail -f ~/.zsh_history
# Emacs: *Messages* バッファ確認
# tmux:  tmux show-messages
```

### 原因
- 構文エラー
- OS固有の機能を他OSで使用
- 依存パッケージ未インストール

---

## Q4: クロスプラットフォーム互換性エラー

### 症状
- macOSで動作するがLinuxで失敗
- WSL環境で特定機能が動作しない

### 解決方法

```bash
# Validatorで互換性検証
Task: dotfiles-validator
# プロンプト: "最後の変更のmacOS/Linux/WSL互換性を検証"

# 該当Skillのcross_platform_check.sh実行
Bash: .claude/skills/zsh-config/scripts/cross_platform_check.sh
```

### 原因
- OS固有コマンドの使用
- パス形式の違い（Windows/Unix）
- グレースフル劣化の未実装

---

## Q5: キーバインド競合

### 症状
- 新しく追加したキーバインドが動作しない
- 既存キーバインドが上書きされた

### 解決方法

```bash
# Emacs
Bash: .claude/skills/emacs-config/scripts/check_keybindings.sh

# tmux
Bash: .claude/skills/tmux-config/scripts/check_conflicts.sh

# Keyboard
Bash: .claude/skills/keyboard-config/scripts/check_skhd_yabai.sh
```

### 原因
- 複数箇所での同一キー定義
- Karabiner ↔ skhd 競合
- エディタ/ターミナル固有のバインド

---

## Q6: 起動時間が遅い

### 症状
- Zsh起動に1秒以上かかる
- Emacs起動に5秒以上かかる

### 解決方法

```bash
# Zshベンチマーク
Bash: .claude/skills/zsh-config/scripts/benchmark.sh

# Emacsプロファイリング
# init.elに追加:
# (setq use-package-verbose t)
# (setq use-package-compute-statistics t)
```

### 原因
- 遅延読み込み未設定（`:defer t` 未指定）
- 外部コマンド呼び出しが未キャッシュ
- プラグインの過剰読み込み

---

## Q7: link.sh実行エラー

### 症状
- `./link.sh` 実行時にシンボリックリンク作成失敗
- 既存ファイルが上書きされる警告

### 解決方法

```bash
# 既存ファイルバックアップ
mv ~/.zshrc ~/.zshrc.bak
mv ~/.emacs.d ~/.emacs.d.bak

# link.sh実行
./link.sh

# エラー確認
echo $?
```

### 原因
- 既存ファイルが既にシンボリックリンク
- パーミッション不足
- ターゲットディレクトリが存在しない

---

## Q8: Skillスクリプトが見つからない

### 症状
- 検証スクリプト実行時に "No such file" エラー
- `scripts/` ディレクトリが存在しない

### 解決方法

```bash
# Skillディレクトリ確認
ls -la .claude/skills/*/scripts/

# 該当Skillのディレクトリ構造確認
tree .claude/skills/emacs-config/
```

### 原因
- Skillが未実装（スクリプト未作成）
- パス指定ミス
- Skill自体が破損

---

## メトリクス・品質目標

### 起動時間
- **Zsh**: < 0.5秒（目標）
- **Emacs**: < 2秒（目標）

### メモリ使用量
- **Emacs**: < 200MB（アイドル時）

### 互換性
- **macOS + Linux/WSL**: 100%動作

### テストカバレッジ
- **Zsh主要機能**: 100%

---

**関連**: `.claude/CLAUDE.md` | `docs/dotfiles-workflows.md` | `.claude/skills/*/SKILL.md`
