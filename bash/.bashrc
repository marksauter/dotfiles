#
# ~/.bashrc
#

export PATH=$HOME/.local/bin:$PATH
export PATH=$HOME/.cargo/bin:$PATH
export PATH="$HOME/.yarn/bin:$HOME/.config/yarn/global/node_modules/.bin:$PATH"
export GOPATH=$HOME/Code/go
export PATH=$GOPATH/bin:$PATH
# export RANGER_LOAD_DEFAULT_RC=FALSE
export VISUAL=nvim
export EDITOR=nvim
export BROWSER=firefox
export RTV_BROWSER=w3m
export TZ=America/Chicago
export XDG_CONFIG_HOME="$HOME/.config"
export RUST_SRC_PATH="$(rustc --print sysroot)/lib/rustlib/src/rust/src"

source ~/.bash_git

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

# ===============================================
# Prompt
# ------

# Syntactic sugar for ANSI escape sequences
txtblk='\e[0;30m' # Black - Regular
txtred='\e[0;31m' # Red
txtgrn='\e[0;32m' # Green
txtylw='\e[0;33m' # Yellow
txtblu='\e[0;34m' # Blue
txtpur='\e[0;35m' # Purple
txtcyn='\e[0;36m' # Cyan
txtwht='\e[0;37m' # White
bldblk='\e[1;30m' # Black - Bold
bldred='\e[1;31m' # Red
bldgrn='\e[1;32m' # Green
bldylw='\e[1;33m' # Yellow
bldblu='\e[1;34m' # Blue
bldpur='\e[1;35m' # Purple
bldcyn='\e[1;36m' # Cyan
bldwht='\e[1;37m' # White
unkblk='\e[4;30m' # Black - Underline
undred='\e[4;31m' # Red
undgrn='\e[4;32m' # Green
undylw='\e[4;33m' # Yellow
undblu='\e[4;34m' # Blue
undpur='\e[4;35m' # Purple
undcyn='\e[4;36m' # Cyan
undwht='\e[4;37m' # White
bakblk='\e[40m'   # Black - Background
bakred='\e[41m'   # Red
badgrn='\e[42m'   # Green
bakylw='\e[43m'   # Yellow
bakblu='\e[44m'   # Blue
bakpur='\e[45m'   # Purple
bakcyn='\e[46m'   # Cyan
bakwht='\e[47m'   # White
txtrst='\e[0m'		# Text Reset

PS1='[\u@\h \W]\$ '
# PS1='\[\e[0;31m\]┌─\e[0m\e[34m[\[\e[0m\e[0;32m\] \w\[\e[0m\] \e[34m]\e[0m $(__git_ps1 "\[\e[0;35m\]@\[\e[0m\]\[\e[0;36m\]\[\e[5m \]%s\[\e[25m\]\[\e[0m\]")\n└─>\[\e[0m\] '

# ===============================================
# Aliases
# -------

if [[ -f ~/.alias ]];then
	. ~/.alias
fi

# ===============================================
# Colors for man pages
# --------------------

export LESS_TERMCAP_mb=$'\e[0;33m'
export LESS_TERMCAP_md=$'\e[0;35m'
export LESS_TERMCAP_me=$'\e[0m'
export LESS_TERMCAP_se=$'\e[0m'
export LESS_TERMCAP_so=$'\e[0;34;32m'
export LESS_TERMCAP_ue=$'\e[0m'
export LESS_TERMCAP_us=$'\e[0;36m'

# ===============================================
# Keybindings
# -----------

setxkbmap -option caps:swapescape

# ===============================================
# NVM
# ---

export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion

# ===============================================
# Z
# ---

if [[ -r /usr/share/z/z.sh ]];then
	source /usr/share/z/z.sh
fi

# ===============================================
# FZF
# ---

export FZF_DEFAULT_COMMAND='rg --files --hidden --follow --glob "!.git/*"'

# vim: set ts=2 sw=2 tw=80 noet :
