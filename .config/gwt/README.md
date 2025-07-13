# Git Worktree管理ツール (gwt) 設定ガイド

## 📁 設定ファイルの場所

```
~/.config/gwt/
├── config.yml          # メイン設定ファイル
├── config.yml.example  # サンプル設定ファイル（このリポジトリ）
└── README.md           # このファイル
```

## 🚀 初期セットアップ

### 1. 設定ファイルの作成

```bash
# 設定ディレクトリを作成
mkdir -p ~/.config/gwt

# サンプル設定をコピー
cp ~/.dotfiles/.config/gwt/config.yml.example ~/.config/gwt/config.yml

# または gwt config コマンドを使用
gwt config init
```

### 2. 設定の確認

```bash
# 現在の設定を表示
gwt config show

# 設定ファイルのパスを表示
gwt config path

# 設定ファイルを編集
gwt config edit
```

## ⚙️ 主要設定項目

### エディタ設定

```yaml
editor:
  default: "auto"               # auto, cursor, code, emacs
  auto_detect: true
  
  cursor:
    args: ["--new-window"]
    wait: false
  
  emacs:
    mode: "client"              # client, nw, gui
    server_timeout: 5
    fallback_to_direct: true
```

**使用例:**
- `gwt open --cursor`: Cursorでworktreeを開く
- `gwt open --emacs-nw`: Emacs（ターミナル）で開く

### 環境ファイル管理

```yaml
environment:
  auto_setup: true              # worktree作成時の自動セットアップ
  auto_port_assignment: true    # ポート競合の自動回避
  
  patterns:                     # 検出パターン（優先順位順）
    - ".env.development.example"
    - ".env.local.example"
    - ".env.example"
  
  port_range:
    start: 3000
    end: 9999
    increment: 100
```

**動作:**
- `gwt create feature/auth`: 環境ファイルを自動セットアップ
- `gwt env analyze --all`: ポート競合をチェック

### tmux統合

```yaml
tmux:
  default_action: "window"      # window, split-h, split-v, session
  auto_detect: true
  naming_pattern: "gwt-{branch}"
```

**使用例:**
- `gwt open --tmux-window`: 新しいtmuxウィンドウで開く
- `gwt open --tmux-split-h`: 水平分割で開く

## 🎯 使用例とワークフロー

### 基本的なワークフロー

```bash
# 1. 新しい機能開発用worktreeを作成（環境ファイル自動セットアップ）
gwt create feature/user-auth main

# 2. エディタで開く
gwt open --cursor

# 3. 開発完了後、PR作成
gwt pr

# 4. マージ後のクリーンアップ
gwt remove feature/user-auth --delete-branch
```

### 環境ファイル管理

```bash
# 環境ファイルの検出
gwt env detect -c

# ポート競合の分析
gwt env analyze --all

# 手動で環境ファイルをセットアップ
gwt env setup --template .env.example --auto-port
```

### 複数worktreeでの作業

```bash
# 全worktreeの状態確認
gwt status -v

# 全worktreeで並列テスト実行
gwt exec --parallel 'npm test'

# 全worktreeを最新に同期
gwt sync --rebase
```

## 🔧 カスタマイズ例

### Next.js プロジェクト用設定

```yaml
environment:
  patterns:
    - ".env.local.example"
    - ".env.example"
  port_range:
    start: 3000
    increment: 1

editor:
  default: "cursor"
  
tmux:
  default_action: "split-h"
```

### バックエンド開発用設定

```yaml
environment:
  patterns:
    - ".env.development.example"
    - ".env.test.example"
    - ".env.example"
  port_range:
    start: 8000
    increment: 10

editor:
  default: "emacs"
  emacs:
    mode: "nw"
```

### 大規模チーム用設定

```yaml
github:
  auto_push: false              # 手動プッシュ
  open_browser: false           # ブラウザを開かない

advanced:
  confirm_destructive: true     # 破壊的操作の確認
  backup_before_modify: true    # 変更前バックアップ
```

## 🐛 トラブルシューティング

### 設定が反映されない

```bash
# 設定ファイルの構文チェック
gwt config show

# 設定ファイルの再読み込み
source ~/.zsh/git-worktree.zsh
```

### エディタが起動しない

```bash
# エディタの検出状況確認
gwt config show | grep editor

# 手動でエディタを指定
gwt open --cursor
gwt open --emacs-nw
```

### 環境ファイルが作成されない

```bash
# テンプレートファイルの検出確認
gwt env detect -c

# 手動セットアップの実行
gwt env setup --template .env.example --auto-port --dry-run
```

## 📚 関連コマンド

- `gwt help`: 全コマンドのヘルプ
- `gwt env help`: 環境ファイル管理のヘルプ
- `gwt config help`: 設定管理のヘルプ

## 🔗 参考リンク

- [dotfiles リポジトリ](../../../README.md)
- [CLAUDE.md](../../../CLAUDE.md)
- [Git Worktree 公式ドキュメント](https://git-scm.com/docs/git-worktree)