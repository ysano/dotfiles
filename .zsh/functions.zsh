# ~/.zsh/functions.zsh

# csh compatibility
setenv() { typeset -x "${1}${1:+=}${(@)argv[2,$#]}" }

# Function to reload autoloaded functions
freload() { while (( $# )); do; unfunction $1; autoload -U $1; shift; done }

# Enhanced cd command
function cd() { builtin cd "$@"; echo $PWD; }

# Generate gitignore file
function gi() { curl -sLw n https://www.toptal.com/developers/gitignore/api/$@ ;}

# Automatically remove duplicates from these arrays
typeset -U path cdpath fpath manpath

# Where to look for autoloaded function definitions
fpath=($fpath ~/.zfunc)

# Append completions to fpath
fpath=(${ASDF_DATA_DIR:-$HOME/.asdf}/completions $fpath)

# Autoload all shell functions from all directories in $fpath (following
# symlinks) that have the executable bit on (the executable bit is not
# necessary, but gives you an easy way to stop the autoloading of a
# particular shell function). $fpath should not be empty for this to work.

# for func in $^fpath/*(N-.x:t); autoload $func
### managed by Zinit
