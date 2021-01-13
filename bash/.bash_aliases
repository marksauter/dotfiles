# -*-conf-unix-*-
# Quick Switch To Directories
alias down="cd ~/Downloads"
alias music="cd ~/Music"
alias vids="cd ~/Videos"
alias codego="cd ~/go/src/github.com/marksauter/"
alias coders="cd ~/rust/src/github.com/marksauter/"
alias codejs="cd ~/javascript/src/github.com/marksauter/"
alias pics="cd ~/Pictures"
alias clones="cd ~/Clones"

# Quick Config Editing
alias spacemacs="emacs ~/.spacemacs &"
alias nvimrc="emacs ~/.config/nvim/init.vim &"
alias bashrc="emacs ~/.bashrc &"
alias bashalias="emacs ~/.bash_aliases &"
alias reload="source ~/.bashrc"

# Miscellaneous Commands
alias ls="ls --group-directories-first --time-style=+'%m.%d.%Y %H:%M' --color=auto -hFX"
alias ll="ls -l --group-directories-first --time-style=+'%m.%d.%Y %H:%M' --color=auto -F"
alias la="ls -al --group-directories-first --time-style=+'%m.%d.%Y %H:%M' --color=auto -F"
alias grep="grep --color=tty -d skip"
alias google="ping -c1 8.8.8.8"
alias cp="cp -i"
alias df="df -h"
alias free="free -m"
alias rm="rm -i"
