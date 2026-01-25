# Linux specific aliases
# Only set ls alias if eza is not available (eza alias is set in aliases.zsh)
if ! has_command eza; then
    alias ls='ls --color=auto'
fi
alias whoislistening="netstat -pntl |grep \$PORT"

# WSL specific aliases (if needed)
if grep -q Microsoft /proc/version; then
    alias open='explorer.exe'
    alias cmd='cmd.exe'
    alias powershell='powershell.exe'
fi
