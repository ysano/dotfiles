# Standard command aliases
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
alias dt='date "+%Y-%m-%d %H:%M:%S"'

# Directory listing aliases
alias lsd='ls -ld *(-/DN)'  # List only directories and symbolic links that point to directories
alias lsa='ls -ld .*'       # List only files beginning with "."

# Emacs aliases
alias e='emacsclient -n'
alias ee='open -a /Applications/Emacs.app $1'
alias ekill='emacsclient -e "(kill-emacs)"'

# Wget aliases
alias wget-domain="wget -r -L -l 10"
alias wget-subdir="wget -r -np"
alias wget-site-save="wget -m -np -p -k -K -E -nv --random-wait -U 'Mozilla/5.0 (Windows NT 6.3; WOW64; Trident/7.0; Touch; rv:11.0) like Gecko'"
alias wget-page-save="wget -m -np -p -k -K -E -nv --random-wait -U 'Mozilla/5.0 (Windows NT 6.3; WOW64; Trident/7.0; Touch; rv:11.0) like Gecko' -H -D 'stat.ameba.jp,livedoor.blogimg.jp'"

# Other utility aliases
alias mketags="jexctags --langmap=html:+.rhtml,ruby:+.yml,php:+.thtml -e -R --append=no --exclude=.svn"
alias irb='irb -r irb/completion'
alias mysql="mysql --pager='less -S -n -i -F -X'"

# Global aliases
alias -g M='|more'
alias -g H='|head'
alias -g T='|tail'

# Standard command aliases
# ... (前の部分は変更なし)

# OS-specific aliases
case "${OSTYPE}" in
    linux*)
        if grep -q Microsoft /proc/version; then
            # WSL specific aliases
            alias open='explorer.exe'
            alias cmd='cmd.exe'
            alias powershell='powershell.exe'
        else
            # Standard Linux aliases
            alias ls='ls --color=auto'
            alias whoislistening="netstat -pntl |grep \$PORT"
        fi
        ;;
    darwin*)
        # macOS specific aliases
        unset LS_COLORS
        alias ls='ls -G'
        export LSCOLORS=ExGxFxdaCxDaDahbadacec
        alias whoislistening="lsof -i -P|grep LISTEN|grep :\$PORT"
        alias showfiles='defaults write com.apple.finder AppleShowAllFiles YES; killall Finder /System/Library/CoreServices/Finder.app'
        alias hidefiles='defaults write com.apple.finder AppleShowAllFiles NO; killall Finder /System/Library/CoreServices/Finder.app'
        ;;
    freebsd*)
        # FreeBSD specific aliases
        alias ls='ls -G'
        export LSCOLORS=ExGxFxdaCxDaDahbadacec
        alias whoislistening="sockstat -4 -l | grep \$PORT"
        ;;
    msys*)
        # MSYS (MinGW) specific aliases
        alias ls='ls --color=auto'
        alias start='cmd //c start'
        ;;
    *)
        # Default aliases for unknown systems
        echo "Unknown operating system. Some aliases may not work correctly."
        ;;
esac

# Color support for ls and grep (if available)
if [ "$TERM" != "dumb" ]; then
    case "${OSTYPE}" in
        linux*|msys*)
            if [ -x /usr/bin/dircolors ]; then
                eval "$(dircolors -b)"
                alias ls='ls --color=auto'
                alias dir='dir --color=auto'
                alias vdir='vdir --color=auto'
            fi
            ;;
        darwin*|freebsd*)
            alias ls='ls -G'
            ;;
    esac

    alias grep='grep --color=auto'
    alias fgrep='fgrep --color=auto'
    alias egrep='egrep --color=auto'
fi

# Emacs 24-bit color support
if which toe > /dev/null 2>&1; then
    if [ `toe | grep "xterm-direct" | wc -l` -gt 0 ]; then
        alias emacs='COLORTERM=truecolor emacs'
    fi
fi

# editor
case "${TERM_PROGRAM}" in
    vscode*)
        export EDITOR='code --wait'
        ;;
    *)
        if which emacs > /dev/null 2>&1; then
            export EDITOR=emacsclient
        elif which vi > /dev/null 2>&1; then
            export EDITOR=vi
        else
            export EDITOR=ee
        fi
        ;;
esac
