# ~/.zsh/core_settings.zsh

# Contains:
# - ZDOTDIR, ZUSERDIR definitions
# - Basic options (setopt, unsetopt)
# - History settings
# - WORDCHARS definition

# Define ZDOTDIR and ZUSERDIR
export ZDOTDIR=${ZDOTDIR:-$HOME}
export ZUSERDIR=$ZDOTDIR/.zsh

# changing directories
unsetopt auto_cd
setopt cdable_vars
setopt pushd_minus
setopt pushd_silent
setopt pushd_to_home

# completions
setopt complete_in_word
setopt glob_complete
setopt list_packed
setopt rec_exact
unsetopt auto_param_slash

# expansion and globbing
unsetopt extended_glob            # '#~^' characters as filename generation
setopt equals                   # =filename
setopt magic_equal_subst        # --prefix=/usr
setopt multibyte
setopt nomatch
setopt mark_dirs
setopt glob_dots

# input / output
setopt correct                  # command spelling correction
unsetopt correct_all              # arguments spelling correction
setopt print_eight_bit          # japanese fix
setopt mail_warning
setopt rc_quotes

# job control
setopt notify
setopt long_list_jobs
setopt auto_resume
unsetopt bg_nice

# prompting
unsetopt promptcr

# scripts and functions

# shell emulation

# shell state

# shell env
MAILCHECK=300
DIRSTACKSIZE=20

# zle
setopt emacs

# History settings (core only - main config in OMZ history.zsh)
export HISTCONTROL=ignorespace # ignore commands that start with space


# Autoload zsh modules
zmodload -a zsh/zpty zpty
zmodload -a zsh/zprof zprof
zmodload -ap zsh/mapfile mapfile
zmodload -aF zsh/stat b:zstat

