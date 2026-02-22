**dotfiles Managed Tools一覧**

> dotfilesプロジェクトで管理する全ツールの設定ファイルとデプロイ方法

## Skill対応ツール（主要4ツール）

| ツール | Config ファイル | Skill | デプロイ方法 | 備考 |
|---|---|---|---|---|
| **Emacs** | `.emacs.d/` (init.el, inits/, elisp/) | emacs-config | `link.sh` → `$HOME` | 4,800行、12モジュール |
| **tmux** | `.tmux.conf`, `.tmux/` | tmux-config | `link.sh` → `$HOME` | 3,800行、Claude Voice統合 |
| **Zsh** | `.zshrc`, `.zprofile`, `.zsh/` | zsh-config | `link.sh` → `$HOME` | 4,600行、Zinit管理 |
| **Keyboard** | | keyboard-config | 複合 | Karabiner/skhd/yabai |
| ├─ Karabiner | `karabiner/karabiner.json` | keyboard-config | 手動 → `~/.config/karabiner/` | 4,209行、JSON形式 |
| ├─ skhd | `.skhdrc` | keyboard-config | `link.sh` → `$HOME` | 230行、アプリランチャー |
| ├─ yabai | `.yabairc` | keyboard-config | `link.sh` → `$HOME` | 71行、ウィンドウ管理 |
| └─ Keyboard Maestro | `keyboard-maestro/*.kmmacros` | keyboard-config (参照のみ) | 手動インポート | macOS専用 |

詳細: 各Skillの`SKILL.md`を参照

---

## その他のツール（Skillなし）

### 開発ツール

| ツール | Config ファイル | デプロイ方法 | 用途 |
|---|---|---|---|
| **Git** | `.config/git/config`, `.config/git/ignore` | `link.sh` → `$XDG_CONFIG_HOME` | バージョン管理 |
| **bat** | `.config/bat/config` | `link.sh` → `$XDG_CONFIG_HOME` | catの代替（構文ハイライト） |
| **ripgrep** | `.config/ripgrep/config` | `link.sh` → `$XDG_CONFIG_HOME` | grepの代替（高速検索） |
| **gwt** | `.config/gwt/` | `link.sh` → `$XDG_CONFIG_HOME` | Git worktree管理 |
| **Aspell** | `.aspell.conf` | `link.sh` → `$HOME` | スペルチェッカー |

### パッケージ管理

| ツール | Config ファイル | デプロイ方法 | 用途 |
|---|---|---|---|
| **Homebrew** | `.Brewfile` | `link.sh` → `$HOME` | macOS/Linuxパッケージ管理 |
| **RPM** | `.rpmmacros` | `link.sh` → `$HOME` | Red Hat系パッケージビルド設定 |

### プラットフォーム固有

| ツール | Config ファイル | デプロイ方法 | 対応OS |
|---|---|---|---|
| **X11** | `.xinitrc`, `.Xresources` | `link.sh` → `$HOME` | Linux |
| **WSL** | `wsl/` (fonts, mozc, vcxsrv) | 手動 | Windows/WSL |
| **mayu** | `mayu/104onFKB.mayu` | 手動 | Windows |

### エディタ/AI

| ツール | Config ファイル | デプロイ方法 | 用途 |
|---|---|---|---|
| **Cursor** | `.cursor/rules/*.mdc` | 手動 | AI補完エディタ |
| **Claude Code** | `.claude/` | プロジェクト内 | AIコーディング支援 |

---

## デプロイ方法詳細

### link.sh自動デプロイ（大部分）

```bash
./link.sh
```

**対象**:
- Emacs, tmux, Zsh, skhd, yabai（Skill対応）
- Git, bat, ripgrep, gwt, Aspell, Homebrew, X11, RPM（その他）

**動作**:
1. ソースファイルへのシンボリックリンク作成
2. 既存ファイルは`.bak`にバックアップ
3. OS検出で適切なディレクトリに配置（`$HOME` or `$XDG_CONFIG_HOME`）

### 手動デプロイ

#### Karabiner (macOS)
```bash
# 初回
cp -r karabiner ~/.config/

# 更新
cp karabiner/karabiner.json ~/.config/karabiner/
```

#### WSL (Windows/WSL)
```bash
# フォント
cp wsl/fonts/*.ttf /mnt/c/Windows/Fonts/

# mozc辞書
# → WSL内で手動セットアップ
```

#### Keyboard Maestro (macOS)
1. アプリ起動
2. File → Import Macros
3. `keyboard-maestro/*.kmmacros`選択

---

## 検証方法

### Skill対応ツール
各Skillの検証スクリプトを使用:

```bash
# Emacs
.claude/skills/emacs-config/scripts/validate.sh

# tmux
.claude/skills/tmux-config/scripts/check_conflicts.sh

# Zsh
.claude/skills/zsh-config/scripts/validate.sh

# Keyboard
.claude/skills/keyboard-config/scripts/check_karabiner.sh
.claude/skills/keyboard-config/scripts/check_skhd_yabai.sh
```

### その他のツール

```bash
# Git設定確認
git config --list --show-origin

# bat設定確認
bat --config-file

# ripgrep設定確認
rg --version && echo $RIPGREP_CONFIG_PATH

# link.sh整合性確認（新規ファイル追加時）
# → link.sh内の files/dirs/config_dirs 配列を確認
```

---

## 新規ツール追加手順

1. **設定ファイル作成**: dotfilesリポジトリに配置
2. **link.sh更新**: `files`/`dirs`/`config_dirs`配列に追加（自動デプロイの場合）
3. **検証方法追加**: 構文チェック、動作確認方法を定義
4. **このドキュメント更新**: 新規ツールを適切なセクションに追記

---

**関連**: `.claude/agents/dotfiles-engineer.md` | `.claude/skills/*/SKILL.md` | `link.sh`
