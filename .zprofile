# ~/.zprofile

## core environment variables
# Contains:
# - EDITOR, PAGER settings
# - Other environment variables (e.g., LESSCHARSET)

# Common environment variables
export LANG=ja_JP.UTF-8
export LC_CTYPE=ja_JP.UTF-8
export EDITOR='emacsclient'
export VISUAL='emacsclient'
export PAGER='less'
export LESS='-RFXx3M'

# Development related
export CVSROOT=$HOME/cvs
export SVNROOT=$HOME/svn
export GZIP='-v9N'
export ACK_COLOR_MATCH='underline white'
export CPPFLAGS="-I/usr/local/opt/openjdk/include"
export JAVA_HOME="/usr/local/opt/openjdk/"
export GTAGSLABEL=pygments

# GPG
export GPG_TTY=$(tty)


## OS-specific settings
# Contains:
# - Case statement for different OS types
# - WSL-specific settings

# Detect the operating system
case "${OSTYPE}" in
    darwin*)
        # macOS specific settings
        # Homebrew
        if [ -f /opt/homebrew/bin/brew ]; then
            eval "$(/opt/homebrew/bin/brew shellenv)"
        fi

        # Use GNU versions of tools if available
        if [ -d /opt/homebrew/opt/gnu-tar/libexec/gnubin ]; then
            path=(/opt/homebrew/opt/gnu-tar/libexec/gnubin $path)
        fi

        # macOS specific aliases or functions can be added here
        ;;
    linux*)
        # Linux specific settings
        if [[ $(uname -a) =~ 'microsoft' ]]; then
            # WSL specific settings
            # xorg for wsl
            # vcxsrv options memo:
            # - disable access control
            # - approve public-network in firewall
            export DISPLAY=$(hostname).mshome.net:0.0
            export LANG=ja_JP.utf-8
            export LIBGL_ALWAYS_INDIRECT=1
            [ -f $HOME/.xinitrc ] && source $HOME/.xinitrc

            # WSL Mozc setup
            if [[ -f /mnt/c/opt/mozc/mozc_emacs_helper.sh ]]; then
                path+=/mnt/c/opt/mozc
            fi
        else
            # Regular Linux settings
        fi
        ;;
    freebsd*)
        # FreeBSD specific settings
        ;;
    cygwin*)
        # Cygwin specific settings
        ;;
esac

# Common settings for Unix-like systems
if [[ "$OSTYPE" != "cygwin"* ]]; then
    unlimit
    limit stack 8192
    limit core 0
    limit -s
fi

# Set umask
umask 022

## PATH settings

typeset -U path
path=(
  $HOME/bin
  /usr/local/bin
  /usr/local/sbin
  /usr/bin
  /bin
  /usr/sbin
  /sbin
  $path
)

# Conditional PATH additions
if [ -d "/var/qmail/bin" ]; then
  path+=(/var/qmail/bin)
fi

if [ -d "$HOME/rd/.bin" ]; then
  path+=($HOME/rd/.bin)
fi

if [[ -f /mnt/c/opt/mozc/mozc_emacs_helper.sh ]]; then
  path+=(/mnt/c/opt/mozc)
fi

# Additional PATH entries (uncomment if needed)
# path=($path /usr/libexec /usr/local/libexec)
# path=($path /usr/local/sysutil)
# path=($path /usr/ucb /usr/etc)  # for SunOS
# path=($path $HOME/my/android/sdk/platform-tools)  # for android
# path=($path $HOME/local/pig-0.12.1/bin)  # for apache-pig
# path=($path $HOME/.composer/vendor/bin)  # for composer
# path=($path $HOME/DEV/flutter/bin)       # for flutter
# path=($path /usr/local/opt/openjdk/bin)  # for jdk

# Remove duplicate entries from PATH
typeset -U PATH
