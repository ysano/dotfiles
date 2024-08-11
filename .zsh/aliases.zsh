# ~/.zsh/aliases.zsh

# 1. Basic Command Extensions
alias mv='nocorrect mv'
alias cp='nocorrect cp'
alias mkdir='nocorrect mkdir'
alias rm='nocorrect rm -i'

# 2. File System Operations
alias ll='ls -l'
alias la='ls -a'
alias lsd='ls -ld *(-/DN)'  # List only directories and symbolic links that point to directories
alias lsa='ls -ld .*'       # List only files beginning with "."

# 3. Navigation
alias j=jobs
alias pu=pushd
alias po=popd
alias d='dirs -v'

# 4. Text Tools
alias grep=egrep

# 5. Development Tools
alias e='emacsclient -n'
alias ee='open -a /Applications/Emacs.app $1'
alias ekill='emacsclient -e "(kill-emacs)"'
alias mketags="jexctags --langmap=html:+.rhtml,ruby:+.yml,php:+.thtml -e -R --append=no --exclude=.svn"
alias irb='irb -r irb/completion'

# 6. Network Tools
alias wget-domain="wget -r -L -l 10"
alias wget-subdir="wget -r -np"
alias wget-site-save="wget -m -np -p -k -K -E -nv --random-wait -U 'Mozilla/5.0 (Windows NT 6.3; WOW64; Trident/7.0; Touch; rv:11.0) like Gecko'"
alias wget-page-save="wget -m -np -p -k -K -E -nv --random-wait -U 'Mozilla/5.0 (Windows NT 6.3; WOW64; Trident/7.0; Touch; rv:11.0) like Gecko' -H -D 'stat.ameba.jp,livedoor.blogimg.jp'"

# 7. Database
alias mysql="mysql --pager='less -S -n -i -F -X'"

# 8. Utilities
alias h=history
alias dt='date "+%Y-%m-%d %H:%M:%S"'

# 9. Global Aliases
alias -g M='|more'
alias -g H='|head'
alias -g T='|tail'

# 10. Terminal-specific settings
setup_terminal_env() {
    case "$TERM" in
        xterm*|rxvt*|urxvt*|linux*|vt*)
            # Color settings for xterm-like terminals
            ;;
        dumb)
            # Settings for dumb terminals
            ;;
        *)
            # Default settings
            ;;
    esac

    if [ "$TERM" != "dumb" ]; then
        alias grep='grep --color=auto'
        alias fgrep='fgrep --color=auto'
        alias egrep='egrep --color=auto'
    fi

    if [ "$TERM_PROGRAM" = "vscode" ]; then
        export EDITOR='code --wait'
    else
        if which emacs > /dev/null 2>&1; then
            export EDITOR=emacsclient
        elif which vi > /dev/null 2>&1; then
            export EDITOR=vi
        else
            export EDITOR=ee
        fi
    fi

    if which toe > /dev/null 2>&1; then
        if [ `toe | grep "xterm-direct" | wc -l` -gt 0 ]; then
            alias emacs='COLORTERM=truecolor emacs'
        fi
    fi
}

# Apply terminal-specific settings
setup_terminal_env

# 11. OS-specific aliases
case "${OSTYPE}" in
    linux*)
        source "${ZDOTDIR:-$HOME}/.zsh/aliases_linux.zsh"
        ;;
    darwin*)
        source "${ZDOTDIR:-$HOME}/.zsh/aliases_darwin.zsh"
        ;;
    freebsd*)
        source "${ZDOTDIR:-$HOME}/.zsh/aliases_freebsd.zsh"
        ;;
    msys*)
        source "${ZDOTDIR:-$HOME}/.zsh/aliases_msys.zsh"
        ;;
    *)
        echo "Unknown operating system. Some aliases may not work correctly."
        ;;
esac
