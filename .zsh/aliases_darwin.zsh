# macOS specific aliases
alias ls='ls -G'
export LSCOLORS=ExGxFxdaCxDaDahbadacec
alias whoislistening="lsof -i -P|grep LISTEN|grep :\$PORT"
alias showfiles='defaults write com.apple.finder AppleShowAllFiles YES; killall Finder /System/Library/CoreServices/Finder.app'
alias hidefiles='defaults write com.apple.finder AppleShowAllFiles NO; killall Finder /System/Library/CoreServices/Finder.app'
