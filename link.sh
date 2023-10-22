#!/usr/bin/zsh
files=(.zshrc .gitconfig .tmux.conf .tmux-linux.conf .tmux-freebsd.conf .aspell.conf .xinitrc .Xresources .yabairc .skhdrc)
dirs=(.zsh .emacs.d)
dotfiles=dotfiles

case "${OSTYPE}" in
    msys)
        for f in $files; do
            cmd <<< "mklink ${f} ${dotfiles}\\${f}"
        done
        for d in $dirs; do
            cmd <<< "mklink /D ${d} ${dotfiles}\\${d}"
        done
        ;;
    *)
        for f in $files; do
            if [[ -f $HOME/$f ]] && mv $HOME/$f $HOME/$f.orig
               ln -s $dotfiles/$f $HOME/$f
        done
        for d in $dirs; do
            if [[ -d $HOME/$d ]] && mv $HOME/$d $HOME/$d.orig
               ln -s $dotfiles/$d $HOME/$d
        done
        ;;
esac

