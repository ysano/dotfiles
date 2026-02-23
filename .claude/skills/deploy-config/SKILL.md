---
name: deploy-config
description: >
  dotfilesデプロイ（link.sh）の変更・デバッグガイド（クロスプラットフォーム symlink, backup, XDG対応）。
  デプロイ対象追加、Windows symlink対応、リンク整合性確認時に使用。
allowed-tools: Read, Write, Edit, MultiEdit, Bash, Glob, Grep
---

# Deploy Config Skill

## Architecture

`link.sh` (zsh スクリプト, ~100行) がエントリーポイント。3種類のデプロイ対象を管理:

```
link.sh
  ├── files[]       ホーム直下ファイル (.zshrc, .tmux.conf, ...)
  │   └── $HOME/$f -> $HOME/dotfiles/$f
  │
  ├── dirs[]        ホーム直下ディレクトリ (.zsh, .emacs.d, .tmux)
  │   └── $HOME/$d -> $HOME/dotfiles/$d
  │
  └── config_dirs[] XDG_CONFIG_HOME配下 (gwt, bat, ripgrep, git)
      └── $XDG_CONFIG_HOME/$d -> $HOME/dotfiles/.config/$d
```

## Platform Behavior

| OS | symlink 方式 | パス形式 | 備考 |
|----|-------------|---------|------|
| Linux/macOS/WSL | `ln -s` | Unix | 標準動作 |
| MSYS2/MINGW64 | `cmd //c mklink` | Windows (cygpath) | `ln -s` は deepcopy になるため回避 |

OS 判定: `$OSTYPE` で分岐。zsh では MINGW64 環境で `cygwin` を返す点に注意。

## Conventions

- **バックアップ規約**: 既存の実ファイル/ディレクトリは `.orig` 付きにリネーム、既存 symlink は削除して再作成
- **XDG_CONFIG_HOME**: 未設定時は `$HOME/.config` にフォールバック
- **親ディレクトリ**: `ensure_parent_dir()` で自動作成
- **Windows**: ファイルは `mklink`、ディレクトリは `mklink /D`、絶対パスには `cygpath -w`

### デプロイ対象追加手順

1. `link.sh` の該当配列に追加:
   - ホーム直下ファイル → `files=()` に追加
   - ホーム直下ディレクトリ → `dirs=()` に追加
   - XDG_CONFIG_HOME 配下 → `config_dirs=()` に追加、`.config/` にディレクトリ作成
2. dotfiles リポジトリにソースファイル/ディレクトリをコミット
3. `validate.sh` で検証

<constraints>
- link.sh は zsh スクリプト — bash 構文を混ぜない
- OSTYPE 分岐は msys|cygwin) で両方マッチさせる（zsh=cygwin, bash=msys）
- Windows 側では ln -s を使わない（deepcopy になる）
- ホーム直下の相対パス mklink には cygpath 不要、XDG 配下の絶対パスには cygpath -w 必須
- backup_if_exists / ensure_parent_dir ヘルパーを活用する（ロジック重複禁止）
</constraints>

## References

詳細な知識は `references/` を参照:
- `references/windows-symlinks.md` — MSYS2/MINGW64 symlink の技術的制約（OSTYPE の罠、mklink、cmd //c、cygpath）

## Scripts

検証用スクリプト。Claude は**実行して結果を受け取る**（中身を読む必要なし）。

```bash
# デプロイ検証（構文、ソース存在、リンク整合性、MSYS2 環境）
.claude/skills/deploy-config/scripts/validate.sh
```

## Common Modifications Checklist

### デプロイ対象追加
- [ ] ソースファイル/ディレクトリが dotfiles リポジトリに存在する
- [ ] `link.sh` の該当配列（files/dirs/config_dirs）に追加
- [ ] Windows 分岐にも対応を追加（mklink / mklink /D）
- [ ] `validate.sh` で PASS を確認

### Windows symlink 対応変更
- [ ] `references/windows-symlinks.md` を確認
- [ ] `$OSTYPE` 分岐が `msys|cygwin)` を含む
- [ ] 絶対パスには `cygpath -w` を使用
- [ ] Developer Mode 要件を考慮

## Debugging

```bash
# link.sh 構文チェック
zsh -n link.sh

# デプロイ検証
.claude/skills/deploy-config/scripts/validate.sh

# 特定リンクの確認
readlink -f ~/.zshrc
ls -la ~/.emacs.d

# MSYS2 環境の OSTYPE 確認
echo $OSTYPE

# Windows パス変換テスト
cygpath -w "$HOME/.config/git"
```

## Do Not Edit

- デプロイ先（`$HOME` 配下）の実体ファイル — dotfiles リポジトリ側を編集すること
