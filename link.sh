#!/usr/bin/env zsh

# ================================
# Configuration
# ================================

# ホーム直下のファイル
files=(.zshrc .zprofile .tmux.conf .aspell.conf .xinitrc .Xresources .yabairc .skhdrc)

# ホーム直下のディレクトリ
dirs=(.zsh .emacs.d .tmux)

# XDG_CONFIG_HOME配下のディレクトリ
config_dirs=(bat ripgrep git)

# ~/.claude/ 配下に個別配備するファイル
# (claude-plugins とは別管理。ホスト固有な統合スクリプト等を symlink で展開)
claude_files=(statusline-command.sh)

# Claude Code の追加プロファイル（CLAUDE_CONFIG_DIR で使う ~/.claude-* の config dir）
# 既定プロファイル ~/.claude の共有資産を symlink で配備する。リンク元は dotfiles ではなく
# ~/.claude（グローバル CLAUDE.md・prompts・個人 skill は dotfiles 管理外のため）。
# 対象は .claude.json を持つ ~/.claude-* を自動検出する（プロファイル名はホスト側に置く）。
claude_profile_shared=(CLAUDE.md prompts)
# skills/ は個別に配備する。synced/（claude.ai アカウントから同期）と learned/（ツールが
# 書き込む領域）は共有しない
claude_profile_skip_skills=(synced learned)

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

# OS に応じてシンボリックリンクを作成する（msys/cygwin は mklink、ディレクトリは /D）
make_symlink() {
    local src="$1" dst="$2"
    case "${OSTYPE}" in
        msys|cygwin)
            local opt=""
            [[ -d "$src" ]] && opt="/D "
            cmd //c "mklink ${opt}\"$(cygpath -w "$dst")\" \"$(cygpath -w "$src")\""
            ;;
        *)
            ln -s "$src" "$dst"
            ;;
    esac
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

        # ~/.claude/ 配下のファイル
        for f in $claude_files; do
            local src="$HOME/$dotfiles/.claude/$f"
            local dst="$HOME/.claude/$f"
            if [[ -e "$src" ]]; then
                ensure_parent_dir "$dst"
                backup_if_exists "$dst"
                cmd //c "mklink $(cygpath -w "$dst") $(cygpath -w "$src")"
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

        # ~/.claude/ 配下のファイル
        for f in $claude_files; do
            local src="$HOME/$dotfiles/.claude/$f"
            local dst="$HOME/.claude/$f"
            if [[ -e "$src" ]]; then
                ensure_parent_dir "$dst"
                backup_if_exists "$dst"
                ln -s "$src" "$dst"
            fi
        done
        ;;
esac

# Claude Code の追加プロファイルへ共有資産を配備
for profile_dir in "$HOME"/.claude-*(N); do
    [[ -d "$profile_dir" && -f "$profile_dir/.claude.json" ]] || continue
    # ~/.claude 自体を指すプロファイルは共有元と同一実体（退避で共有元を壊す）のでスキップ
    default_dir="$HOME/.claude"
    [[ "${profile_dir:A}" == "${default_dir:A}" ]] && continue

    # 配備先が解決後に共有元と同一実体なら触らない（張り済み、または skills/ 等の親が
    # 共有元を指す symlink。後者で退避すると共有元を壊す）
    for item in $claude_profile_shared; do
        src="$HOME/.claude/$item"
        dst="$profile_dir/$item"
        [[ -e "$src" ]] || continue
        [[ -e "$dst" && "${dst:A}" == "${src:A}" ]] && continue
        backup_if_exists "$dst"
        make_symlink "$src" "$dst"
    done

    # (-/) で symlink の先がディレクトリのものも含める（正本を machine-management に置き、
    # ~/.claude/skills/<名前> が symlink の構成。(/) だと実体のディレクトリしか一致しない）
    for skill_src in "$HOME"/.claude/skills/*(-/N); do
        skill="${skill_src:t}"
        (( ${claude_profile_skip_skills[(Ie)$skill]} )) && continue
        dst="$profile_dir/skills/$skill"
        [[ -e "$dst" && "${dst:A}" == "${skill_src:A}" ]] && continue
        ensure_parent_dir "$dst"
        backup_if_exists "$dst"
        make_symlink "$skill_src" "$dst"
    done
done

# Claude Code 拡張は claude-plugins リポジトリで管理
# /plugin install <name>@ysano-plugins でインストール
