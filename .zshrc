# -*- mode:shell-script -*-
echo "Loading $HOME/.zshrc"

# zprof
# zmodload zsh/zprof && zprof

# Search path for the cd command
cdpath=(.. ~ ~/src ~/DEV ~/MY)

case "${OSTYPE}" in
cygwin*)
    export TERM=cygwin
    ;;
freebsd*|darwin*)
    unlimit
    limit stack 8192
    limit core 0
    limit -s
    ;;
linux*)
    unlimit
    limit stack 8192
    limit core 0
    limit -s
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
alias wget-site-save="wget -m -np -p -k -K -E -nv --random-wait -U 'Mozilla/5.0 (Windows NT 6.3; WOW64; Trident/7.0; Touch; rv:11.0) like Gecko'"
alias wget-page-save="wget -m -np -p -k -K -E -nv --random-wait -U 'Mozilla/5.0 (Windows NT 6.3; WOW64; Trident/7.0; Touch; rv:11.0) like Gecko' -H -D 'stat.ameba.jp,livedoor.blogimg.jp'"
alias mysql="mysql --pager='less -S -n -i -F -X'"

# gitignore
function gi() { curl -sLw n https://www.toptal.com/developers/gitignore/api/$@ ;}

########################################
# Shell functions
########################################
setenv() { typeset -x "${1}${1:+=}${(@)argv[2,$#]}" }  # csh compatibility
freload() { while (( $# )); do; unfunction $1; autoload -U $1; shift; done }

# Where to look for autoloaded function definitions
fpath=($fpath ~/.zfunc)
# append completions to fpath
fpath+=${ASDF_DIR}/completions

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
    if [ -x $HOME/.zsh/pure ]; then
        # pure prompt
        fpath+=($HOME/.zsh/pure)
        zstyle :prompt:pure:git:stash show yes
        autoload -U promptinit; promptinit
        prompt pure
    elif [ -f $ZUSERDIR/my_vcs_info ]; then
        PROMPT_COMMAND='echo -ne "\033]0;${USER}@${HOSTNAME}: ${PWD/$HOME/~}\007"'
        # load vcs prompt
        source $ZUSERDIR/my_vcs_info
    else
        PROMPT_COMMAND='echo -ne "\033]0;${USER}@${HOSTNAME}: ${PWD/$HOME/~}\007"'
    fi
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

case "${TERMCAP}" in
    emacs*)
    ;;

    *)
        # emacs
        bindkey -e
        # bind C-s and C-q
        stty -ixon
        # meta key on xterm
        stty pass8
        ;;
esac


################################
## color
################################

# load default colored ls setting
if [ -f $ZUSERDIR/lscolors ]; then
  source $ZUSERDIR/lscolors
fi

# 256dark
if [ -f ~/.zsh/dircolors.256dark ]; then
    echo "Loading dircolors.256dark"
    if type dircolors > /dev/null 2>&1; then
        eval $(dircolors ~/.zsh/dircolors.256dark)
    elif type gdircolors > /dev/null 2>&1; then
        eval $(gdircolors ~/.zsh/dircolors.256dark)
    fi
else
    eval "`dircolors -b`"
fi


# use ls_colors on completion
zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}

# tcsh color key are few.
case "${OSTYPE}" in
freebsd*|darwin*)
    unset LS_COLORS
    alias ls='ls -G'            # use LSCOLORS
    #               dilnsopiexbdcdsusgtwow
    export LSCOLORS=ExGxFxdaCxDaDahbadacec
    alias whoislistening="lsof -i -P|grep LISTEN|grep :$PORT"
    ;;
linux*)
    alias ls='ls --color'       # use LS_COLORS
    alias whoislistening="netstat -pntl |grep $PORT"
esac

# set a fancy prompt (non-color, unless we know we "want" color)
case "$TERM" in
    xterm-color|xterm-256color) color_prompt=yes;;
esac

# enable color support of ls and also add handy aliases
if [ "$TERM" != "dumb" ] && [ -x /usr/bin/dircolors ]; then
    alias ls='ls --color=auto'
    alias dir='ls --color=auto --format=vertical'
    alias vdir='ls --color=auto --format=long'

    alias grep='grep --color=auto'
    alias fgrep='fgrep --color=auto'
    alias egrep='egrep --color=auto'
fi

# emacs 24bits alias
if which toe > /dev/null 2>&1; then
    if [ `toe | grep "xterm-24bits" | wc -l` -gt 0 ]; then
        alias emacs='TERM=xterm-24bits emacs'
    fi
fi

# xorg for wsl
# vcxsrv options memo:
# - disable access control
# - approve public-network in firewall
if [[ `uname -a` =~ 'microsoft' ]]; then
    export DISPLAY=`hostname`.mshome.net:0.0
    export LANG=ja_JP.utf-8
    export LIBGL_ALWAYS_INDIRECT=1
    source $HOME/.xinitrc
fi

################################
## functions
################################

# gh
if which gh > /dev/null 2>&1; then
    eval "$(gh completion -s zsh)"
fi

autoload -Uz compinit
setopt EXTENDEDGLOB
for dump in $HOME/.zcompdump(#qN.m1); do
    compinit
    if [[ -s "$dump" && (! -s "$dump.zwc" || "$dump" -nt "$dump.zwc") ]]; then
        zcompile "$dump"
    fi
done
unsetopt EXTENDEDGLOB
compinit -C

function cd() { builtin cd "$@"; echo $PWD; }

################################
# Options
################################
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

# history
setopt hist_ignore_dups
setopt hist_save_nodups
setopt share_history            # ksh only

# initialisation

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

# menu select
zstyle ':completion:*:default' menu select interactive

#zstyle ':completion:*' completer _expand _complete _correct _ignored _approximate
#zstyle ':completion:*' completer _expand _complete _match _prefix _approximate _list _history
zstyle ':completion:*' completer _complete _approximate _match _expand _history _prefix

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
    '*?.old' '*?.pro' '*?.~undo-tree~'
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

if [ -f $ZUSERDIR/_docker-compose ]; then
    echo "Loading $ZUSERDIR/_docker-compose"
    source $ZUSERDIR/_docker-compose
    compdef _docker-compose docker-compose
fi

################################
## path setting
################################
path=($path /usr/libexec /usr/local/libexec)
path=($path /usr/local/sysutil)
path=($path /usr/ucb /usr/etc)  # for SunOS
path=($path $HOME/my/android/sdk/platform-tools)  # for android
path=($path $HOME/local/pig-0.12.1/bin)  # for apache-pig
path=($path $HOME/.composer/vendor/bin)  # for composer
path=($path $HOME/DEV/flutter/bin)       # for flutter
path=($path /usr/local/opt/openjdk/bin)  # for jdk
path=($path /usr/local/sbin) # for brew doctor

# qmail path
if [ -d /var/qmail/bin ]; then
    path=($path /var/qmail/bin)
fi
# rbenv
if [ -d $HOME/.rbenv ]; then
    path=($HOME/.rbenv/bin $path)
    eval "$(rbenv init - --no-rehash zsh)"
fi
# asdf
if [ -d /opt/asdf-vm ]; then
    path=(/opt/asdf-vm/bin $path)
fi
path=($HOME/bin $path)

# asdf
case "${OSTYPE}" in
    darwin*)
        eval ". $(brew --prefix asdf)/libexec/asdf.sh"
        ;;
    *)
        if [ -f $HOME/.asdf/asdf.sh ]; then
            . $HOME/.asdf/asdf.sh
        fi
        ;;
esac

# node path
path=($path ./node_modules/.bin)

# lima docker
# > limactl start docker
# > docker context use lima
if [ -d $HOME/.lima ]; then
    export LIMA_INSTANCE=docker
fi

################################
## environment
################################
export CVSROOT=$HOME/cvs
export SVNROOT=$HOME/svn
export ACK_COLOR_MATCH='underline white'
export GZIP='-v9N'
export LESS=-FXx3M
export LANG=ja_JP.UTF-8
export LC_CTYPE=ja_JP.UTF-8
export CPPFLAGS="-I/usr/local/opt/openjdk/include"
export JAVA_HOME="/usr/local/opt/openjdk/"

# gnu global + exuberant ctags + pygments in local
export GTAGSLABEL=pygments

# export
export GPG_TTY=$(tty)

# editor
case "${TERM_PROGRAM}" in
    vscode*)
        export EDITOR='code --wait'
        ;;
    *)
        if which emacs > /dev/null 2>&1; then
            export EDITOR=emacs
        elif which vi > /dev/null 2>&1; then
            export EDITOR=vi
        else
            export EDITOR=ee
        fi
        ;;
esac


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
    if [ `tput colors` -gt 0 ]; then
        export PAGER='less -R'
    else
        export PAGER=less
    fi
fi

# zprof
# if (which zprof > /dev/null) ;then
#     zprof | less
# fi
