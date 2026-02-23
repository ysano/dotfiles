#!/usr/bin/env zsh

# ================================
# Configuration
# ================================

# ホーム直下のファイル
files=(.zshrc .zprofile .tmux.conf .aspell.conf .xinitrc .Xresources .yabairc .skhdrc .Brewfile)

# ホーム直下のディレクトリ
dirs=(.zsh .emacs.d .tmux)

# XDG_CONFIG_HOME配下のディレクトリ
config_dirs=(gwt bat ripgrep git)

dotfiles=dotfiles

# ================================
# Helper Functions
# ================================

backup_if_exists() {
    local target="$1"
    if [[ -e "$target" && ! -L "$target" ]]; then
        mv "$target" "${target}.orig"
    elif [[ -L "$target" ]]; then
        rm "$target"
    fi
}

ensure_parent_dir() {
    local target="$1"
    local parent="$(dirname "$target")"
    [[ ! -d "$parent" ]] && mkdir -p "$parent"
}

# ================================
# Main Logic
# ================================

case "${OSTYPE}" in
    msys|cygwin)
        # MSYS2/MINGW64: cmd mklink でネイティブシンボリックリンク作成
        # ln -s はデフォルトで deepcopy になるため mklink を使用

        # ホーム直下ファイル
        for f in $files; do
            backup_if_exists "$HOME/$f"
            cmd //c "mklink $f $dotfiles\\$f"
        done

        # ホーム直下ディレクトリ
        for d in $dirs; do
            backup_if_exists "$HOME/$d"
            cmd //c "mklink /D $d $dotfiles\\$d"
        done

        # XDG_CONFIG_HOME配下
        local config_home="${XDG_CONFIG_HOME:-$HOME/.config}"
        for d in $config_dirs; do
            local src="$HOME/$dotfiles/.config/$d"
            local dst="$config_home/$d"
            if [[ -d "$src" ]]; then
                ensure_parent_dir "$dst"
                backup_if_exists "$dst"
                cmd //c "mklink /D $(cygpath -w "$dst") $(cygpath -w "$src")"
            fi
        done
        ;;
    *)
        # Unix系 (Linux/macOS/WSL)

        # ホーム直下ファイル
        for f in $files; do
            backup_if_exists "$HOME/$f"
            ln -s "$HOME/$dotfiles/$f" "$HOME/$f"
        done

        # ホーム直下ディレクトリ
        for d in $dirs; do
            backup_if_exists "$HOME/$d"
            ln -s "$HOME/$dotfiles/$d" "$HOME/$d"
        done

        # XDG_CONFIG_HOME配下
        local config_home="${XDG_CONFIG_HOME:-$HOME/.config}"
        for d in $config_dirs; do
            local src="$HOME/$dotfiles/.config/$d"
            local dst="$config_home/$d"
            if [[ -d "$src" ]]; then
                ensure_parent_dir "$dst"
                backup_if_exists "$dst"
                ln -s "$src" "$dst"
            fi
        done
        ;;
esac

# Claude Code 拡張は claude-plugins リポジトリで管理
# /plugin install <name>@ysano-plugins でインストール
