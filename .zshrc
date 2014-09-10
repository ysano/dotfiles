# -*- mode:shell-script -*-
echo "Loading $HOME/.zshrc"

# Search path for the cd command
cdpath=(.. ~ ~/src)

case "${OSTYPE}" in
cygwin*)
    ;;
freebsd*|darwin*)
    unlimit
    limit stack 8192
    limit core 0
    limit -s
    alias ls='ls -G'            # use LSCOLORS
    #        dilnsopiexbdcdsusgtwow
    export LSCOLORS=ExGxFxdaCxDaDahbadacec
    ;;
linux*)
    unlimit
    limit stack 8192
    limit core 0
    limit -s
    alias ls='ls --color'       # use LS_COLORS
    ;;
esac

umask 022

########################################
# Set up aliases
########################################
alias mv='nocorrect mv'       # no spelling correction on mv
alias cp='nocorrect cp'       # no spelling correction on cp
alias mkdir='nocorrect mkdir' # no spelling correction on mkdir
alias rm='nocorrect rm -i'
alias j=jobs
alias pu=pushd
alias po=popd
alias d='dirs -v'
alias h=history
alias grep=egrep
alias ll='ls -l'
alias la='ls -a'

# List only directories and symbolic
# links that point to directories
alias lsd='ls -ld *(-/DN)'

# List only file beginning with "."
alias lsa='ls -ld .*'

# others
alias mketags="jexctags --langmap=html:+.rhtml,ruby:+.yml,php:+.thtml -e -R --append=no --exclude=.svn"
alias wget-domain="wget -r -L -l 10"
alias wget-subdir="wget -r -np"
alias irb='irb -r irb/completion'

########################################
# Shell functions
########################################
setenv() { typeset -x "${1}${1:+=}${(@)argv[2,$#]}" }  # csh compatibility
freload() { while (( $# )); do; unfunction $1; autoload -U $1; shift; done }

# Where to look for autoloaded function definitions
fpath=($fpath ~/.zfunc)

# Autoload all shell functions from all directories in $fpath (following
# symlinks) that have the executable bit on (the executable bit is not
# necessary, but gives you an easy way to stop the autoloading of a
# particular shell function). $fpath should not be empty for this to work.
for func in $^fpath/*(N-.x:t); autoload $func

# automatically remove duplicates from these arrays
typeset -U path cdpath fpath manpath

########################################
# Global aliases -- These do not have to be
# at the beginning of the command line.
########################################
alias -g M='|more'
alias -g H='|head'
alias -g T='|tail'

# Hosts to use for completion (see later zstyle)
#hosts=(`hostname` ftp.math.gatech.edu prep.ai.mit.edu wuarchive.wustl.edu)

########################################
# Set prompts
########################################

setopt prompt_subst

# If this is an xterm set the title to user@host:dir
case "$TERM" in
xterm*|rxvt*)
    PROMPT_COMMAND='echo -ne "\033]0;${USER}@${HOSTNAME}: ${PWD/$HOME/~}\007"'
    ;;
dumb | emacs)
    PROMPT="%n@%~%(!.#.$)"
    RPROMPT=""
    unsetopt zle
    ;;
*)
    PROMPT='%m%# '    # default prompt
    RPROMPT=' %~'     # prompt for right side of screen
    ;;
esac

# load vcs prompt
if [ -f $ZUSERDIR/my_vcs_info ]; then
  source $ZUSERDIR/my_vcs_info
fi

########################################
# Some environment variables
########################################

MAILCHECK=300
HISTFILE=~/.histfile
HISTSIZE=1000
SAVEHIST=1000
DIRSTACKSIZE=20

################################
## Startup/Shutdown Files
################################

autoload colors
colors

# personal setting directory
if [ -z $ZDOTDIR ]; then
  export ZDOTDIR=$HOME
fi

# zsh setting files directory
export ZUSERDIR=$ZDOTDIR/.zsh

# local setting
[ -f ~/.zshrc.local ] && source ~/.zshrc.local

################################
## keybind
################################

# emacs
bindkey -e

# bind C-s and C-q
stty -ixon

# meta key on xterm
stty pass8

################################
## color
################################

# load colored ls setting
if [ -f $ZUSERDIR/lscolors ]; then
  source $ZUSERDIR/lscolors
fi

# use ls_colors on completion
zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}

# tcsh color key are few.
case "${OSTYPE}" in
freebsd*|darwin*)
        unset LS_COLORS
        ;;
esac

# set a fancy prompt (non-color, unless we know we "want" color)
case "$TERM" in
    xterm-color|xterm-256color) color_prompt=yes;;
esac

# enable color support of ls and also add handy aliases
if [ "$TERM" != "dumb" ] && [ -x /usr/bin/dircolors ]; then
    eval "`dircolors -b`"
    alias ls='ls --color=auto'
    alias dir='ls --color=auto --format=vertical'
    alias vdir='ls --color=auto --format=long'

    alias grep='grep --color=auto'
    alias fgrep='fgrep --color=auto'
    alias egrep='egrep --color=auto'
fi

################################
## functions
################################

autoload -Uz compinit
compinit

function cd() { builtin cd "$@"; echo $PWD; }

################################
# Options
################################
# changing directories
setopt auto_cd
setopt auto_pushd
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
setopt extended_glob            # '#~^' characters as filename generation
setopt equals                   # =filename
setopt magic_equal_subst        # --prefix=/usr
setopt multibyte
setopt nomatch
setopt mark_dirs
setopt glob_dots

# history
setopt hist_ignore_dups
setopt hist_save_nodups
setopt share_history            # ksh only

# initialisation

# input / output
#setopt correct                  # command spelling correction
#setopt correct_all              # arguments spelling correction
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

# zle
setopt emacs

################################################
# Autoload zsh modules when they are referenced
################################################
zmodload -a zsh/zpty zpty
zmodload -a zsh/zprof zprof
zmodload -ap zsh/mapfile mapfile
# stat(1) is now commonly an external command, so just load zstat
zmodload -aF zsh/stat b:zstat

#C-W (backword-kill-word)
#C-[D (kill-word)
# の単語境界
WORDCHARS='*?_-.[]~=&;!#$%^(){}<>'

################################
# completions
################################

zstyle ':completion:*' completer _expand _complete _correct _ignored _approximate

# allow one error for every three characters typed in approximate completer
zstyle -e ':completion:*:approximate:*' max-errors \
    'reply=( $(( ($#PREFIX+$#SUFFIX)/3 )) numeric )'

# insert all expansions for expand completer
zstyle ':completion:*:expand:*' tag-order all-expansions

# formatting and messages
zstyle ':completion:*' verbose yes
zstyle ':completion:*:descriptions' format '%B%d%b'
zstyle ':completion:*:messages' format '%d'
zstyle ':completion:*:warnings' format 'No matches for: %d'
zstyle ':completion:*:corrections' format '%B%d (errors: %e)%b'
zstyle ':completion:*' group-name ''

# match uppercase from lowercase
zstyle ':completion:*' matcher-list '' 'm:{a-z}={A-Z} r:|[-_.]=**'

# offer indexes before parameters in subscripts
zstyle ':completion:*:*:-subscript-:*' tag-order indexes parameters

# command for process lists, the local web server details and host completion
zstyle ':completion:*:processes' command 'ps -o pid,s,nice,stime,args'

# hosts
zstyle '*' hosts $hosts

# Filename suffixes to ignore during completion (except after rm command)
zstyle ':completion:*:*:(^rm):*:*files' ignored-patterns '*?.o' '*?.c~' \
    '*?.old' '*?.pro'
# the same for old style completion
#fignore=(.o .c~ .old .pro)

# ignore completion functions (until the _ignored completer)
zstyle ':completion:*:functions' ignored-patterns '_*'

# process menu selection
zstyle ':completion:*:(processes|jobs)' menu yes select=2

# history predict
autoload -U predict-on
zle -N predict-on
zle -N predict-off
bindkey '^xp' predict-on        # C-x p
bindkey '^x^p' predict-off      # C-x C-p

#LISTMAX=-1                     # 黙って表示(どんなに多くても)
LISTMAX=0                       # ウィンドウから溢れるときは尋ねる。

# compact list
setopt listpacked

# some function create caches in ~/.zcompcache/
zstyle ':completion:*' use-cache true

# load gibo-completion.zsh
if [ -f $ZUSERDIR/gibo-completion.zsh ]; then
  echo "Loading $ZUSERDIR/gibo-completion.zsh"
  source $ZUSERDIR/gibo-completion.zsh
fi


################################
## path setting
################################
path=($path /usr/libexec /usr/local/libexec)
path=($path /usr/local/sysutil)
path=($path /usr/ucb /usr/etc)  # for SunOS
path=($path $HOME/lib/android-sdk/tools)  # for android
path=($path $HOME/local/pig-0.12.1/bin)  # for apache-pig

# qmail path
if [ -d /var/qmail/bin ]; then
    path=($path /var/qmail/bin)
fi
# rbenv
if [ -d $HOME/.rbenv ]; then
    path=($HOME/.rbenv/bin $path)
	eval "$(rbenv init -)"
fi
path=($HOME/bin $path)

################################
## environment
################################
export CVSROOT=$HOME/cvs
export SVNROOT=$HOME/svn
export ACK_COLOR_MATCH='underline white'
export GZIP='-v9N'
export LESS=-cex3M
export LANG=ja_JP.UTF-8
export LC_CTYPE=ja_JP.UTF-8
export JAVA_HOME=${JAVA_HOME:-/usr/local/openjdk6}

# editor
if which emacs > /dev/null 2>&1; then
    export EDITOR=emacs
elif which vi > /dev/null 2>&1; then
    export EDITOR=vi
else
    export EDITOR=ee
fi

# pager
export LESSCHARSET=
if which lv > /dev/null 2>&1; then
    if [ `tput colors` -gt 0 ]; then
        export PAGER='lv -c'
    else
        export PAGER=lv
    fi
elif which jless > /dev/null 2>&1; then
    export PAGER=jless
else
    export PAGER=less
fi
