#!/usr/bin/zsh
files=(.zshrc .gitconfig .tmux.conf .tmux-linux.conf .tmux-freebsd.conf .aspell.conf .xinitrc .Xresources)
dirs=(.zsh .emacs.d)
dotfiles=./dotfiles

for f in $files; do
    if [[ -f $HOME/$f ]] && mv $HOME/$f $HOME/$f.orig
    ln -s $dotfiles/$f $HOME/$f
done

for d in $dirs; do
    if [[ -d $HOME/$d ]] && mv $HOME/$d $HOME/$d.orig
    ln -s $dotfiles/$d $HOME/$d
done
