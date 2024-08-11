# Linux specific aliases
alias ls='ls --color=auto'
alias whoislistening="netstat -pntl |grep \$PORT"

# WSL specific aliases (if needed)
if grep -q Microsoft /proc/version; then
    alias open='explorer.exe'
    alias cmd='cmd.exe'
    alias powershell='powershell.exe'
fi
