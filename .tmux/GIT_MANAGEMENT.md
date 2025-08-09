# tmux Claude Voice - Git管理ガイドライン

## Git管理の判断基準

### ✅ Git管理すべきファイル
1. **ソースコード** - すべてのシェルスクリプト（.sh）
2. **設定テンプレート** - デフォルト設定ファイル（.conf）
3. **ドキュメント** - README、アーキテクチャ文書（.md）
4. **共有設定** - チーム共通の設定値

### ❌ Git管理から除外すべきファイル（.gitignore対象）

#### 1. キャッシュファイル
- `.config_cache` - 設定のキャッシュ
- `.wsl_host_cache` - WSL環境のホストIPキャッシュ
- `*.cache` - その他のキャッシュファイル

**理由**: 環境依存で自動生成される一時データ

#### 2. ランタイムステータスファイル
- `window-*.status` - 各ウィンドウの現在ステータス
- `.last_notify_*` - 最終通知時刻
- `.rate_limit_*` - レート制限管理
- `.monitor.pid` - プロセスID

**理由**: 実行時に動的に生成・更新される

#### 3. ログファイル
- `*.log` - すべてのログファイル
- `logs/` ディレクトリ

**理由**: デバッグ情報で個人環境依存

### 🤔 判断が分かれるファイル

#### `notification_mode`
- **Git管理する場合**: デフォルト値として全員で共有
- **除外する場合**: 個人の好みとして各自設定

現在の推奨: **Git管理する**（デフォルトは"sound"モード）

## ディレクトリ構造と管理方針

```
.tmux/
├── claude/
│   ├── .gitignore          # Claude Voice用ignore設定
│   ├── .config_cache        # ❌ 除外: 設定キャッシュ
│   ├── .wsl_host_cache      # ❌ 除外: WSLホストキャッシュ
│   ├── notification_mode    # ✅ 管理: デフォルト設定
│   ├── core/               # ✅ 管理: コアモジュール
│   ├── platforms/          # ✅ 管理: プラットフォーム別実装
│   └── bin/                # ✅ 管理: 実行スクリプト
│
├── status/                 # ❌ ディレクトリ全体が除外（ルート.gitignore）
│   ├── window-*.status     # ❌ 除外: 動的ステータス
│   ├── .last_notify_*      # ❌ 除外: 通知タイムスタンプ
│   ├── .rate_limit_*       # ❌ 除外: レート制限
│   └── .monitor.pid        # ❌ 除外: プロセスID
│
└── scripts/                # ✅ 管理: すべてのスクリプト
```

## メンテナンス

### キャッシュのクリア
```bash
# すべてのキャッシュファイルを削除
find ~/.tmux -name "*.cache" -o -name ".*.cache" | xargs rm -f

# ステータスファイルのクリーンアップ
rm -f ~/.tmux/status/window-*.status
rm -f ~/.tmux/status/.last_notify_*
rm -f ~/.tmux/status/.rate_limit_*
```

### 新しいファイルを追加する際の判断フロー
1. このファイルは環境依存か？ → Yes: .gitignore
2. 実行時に自動生成されるか？ → Yes: .gitignore
3. ユーザー固有の設定か？ → 要検討
4. チームで共有すべきか？ → Yes: Git管理

## 参考: .gitignoreパターン

```gitignore
# キャッシュ
*.cache
.*.cache

# ランタイムファイル
*.pid
*.status
.last_*
.rate_limit_*

# ログ
*.log
logs/

# 一時ファイル
*.tmp
.*.swp
.*.swo
```