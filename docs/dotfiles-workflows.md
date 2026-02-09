**dotfiles典型的なワークフロー**

> `.claude/` モジュールを使用したdotfiles設定変更の典型的なパターン

## 1. 新規設定追加（例: Emacsプラグイン追加）

### ステップ

```bash
# ① Skillで現状把握
Skill: emacs-config
# → Architecture/Conventions を確認

# ② エンジニアエージェント起動
Task: dotfiles-engineer
# プロンプト: "org-roamプラグインを追加し、C-c n プレフィックスで設定"

# ③ 検証（オプション）
Task: dotfiles-validator
# プロンプト: "追加したorg-roam設定の規約準拠を検証"

# ④ コミット
/dev:commit
# 自動的に品質チェック実行 → コミット作成
```

### ポイント

- **Skill先行**: 必ず既存パターンを確認してから実装
- **小さな変更**: 1プラグイン/1機能ずつ追加
- **即座に検証**: 追加後すぐにテスト実行

---

## 2. クロスプラットフォーム対応（例: Zsh PATH設定）

### ステップ

```bash
# ① Skillで現状把握
Skill: zsh-config
# → has_command、safe_path_prepend パターン確認

# ② 実装
Task: dotfiles-engineer
# プロンプト: "HomebrewパスをmacOS/Linux両対応で追加"

# ③ 検証必須（クロスプラットフォームは検証必須）
Task: dotfiles-validator
# プロンプト: "追加したPATH設定のmacOS/Linux互換性を検証"

# ④ テスト実行
Bash: ./test_zsh_config.zsh

# ⑤ コミット
/dev:commit
```

### ポイント

- **クロスプラットフォーム検証必須**: macOS/Linux/WSL全対応を確認
- **グレースフル劣化**: コマンド未導入時のフォールバック実装
- **has_commandガード**: 外部コマンド依存は必ずガード

---

## 3. 複雑な設定変更（例: tmux Claude Voice統合調整）

### ステップ

```bash
# ① Skillで詳細確認
Skill: tmux-config
# → Claude Voice統合セクション確認

# ② 実装
Task: dotfiles-engineer
# プロンプト: "Claude Voice通知の音量を50%に調整"

# ③ 手動テスト
# tmuxセッション内で動作確認

# ④ コミット（品質チェック自動実行）
/dev:commit

# ⑤ PR作成
/dev:pull-request
```

### ポイント

- **統合機能の慎重な変更**: 複数ツール連携は影響範囲を確認
- **手動テスト**: スクリプト化困難な機能は手動確認
- **段階的デプロイ**: バックアップ → 変更 → テスト → コミット

---

## ベストプラクティス

### 1. 常にSkillから開始

```bash
❌ 悪い例: いきなり設定ファイルを編集
Task: dotfiles-engineer
# プロンプト: ".zshrcにエイリアス追加"
# → 既存パターン無視のリスク

✅ 良い例: Skillで既存パターン確認後に編集
Skill: zsh-config
# → Architecture/Conventions 確認
Task: dotfiles-engineer
# プロンプト: ".zshrcにエイリアス追加（safe_path_prependパターン準拠）"
```

### 2. 段階的変更

```bash
✅ 推奨フロー:
1. Skill確認 → 既存パターン理解
2. 小さな変更 → 1機能ずつ追加
3. 即座にテスト → 問題早期発見
4. コミット → 変更履歴保存
5. 次の変更へ
```

### 3. 検証習慣

```bash
✅ 変更後は必ず検証:
- Zsh変更 → ./test_zsh_config.zsh
- Emacs変更 → byte-compile確認
- tmux変更 → 設定リロード
- Keyboard変更 → Karabiner-EventViewer確認
```

---

## 制約事項

### 1. Skill依存

- `dotfiles-engineer` と `dotfiles-validator` は必ずSkillを参照する
- Skill読み込みなしの設定変更は禁止（既存パターン無視リスク）

### 2. git add制限

- `git add .` および `git add -A` 禁止
- 必ず明示的なファイル指定（例: `git add .zshrc .zprofile`）

### 3. テスト必須

- Zsh: `./test_zsh_config.zsh` 実行必須
- Emacs: byte-compile エラーチェック
- tmux: 設定リロード後の動作確認

### 4. バックアップ

- 重要ファイル変更前に `.bak` 作成
- 段階的デプロイ（新ファイル作成 → シンボリックリンク切り替え）

---

**関連**: `.claude/CLAUDE.md` | `docs/dotfiles-troubleshooting.md`
