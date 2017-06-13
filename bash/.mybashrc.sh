#!/bin/bash

# color man pages
export LESS_TERMCAP_mb=$(printf '\e[01;31m') # enter blinking mode - red
export LESS_TERMCAP_md=$(printf '\e[01;35m') # enter double-bright mode - bold, magenta
export LESS_TERMCAP_me=$(printf '\e[0m') # turn off all appearance modes (mb, md, so, us)
export LESS_TERMCAP_se=$(printf '\e[0m') # leave standout mode
export LESS_TERMCAP_so=$(printf '\e[01;33m') # enter standout mode - yellow
export LESS_TERMCAP_ue=$(printf '\e[0m') # leave underline mode
export LESS_TERMCAP_us=$(printf '\e[04;36m') # enter underline mode - cyan

# Extend arrow function behaviour
# bind '"\e[A"':history-search-backward
# bind '"\e[B"':history-search-forward

# rvm
export PATH="$PATH:$HOME/.rvm/bin" # Add RVM to PATH for scripting
source $HOME/.rvm/scripts/rvm

# Prompt
WHITE="\[\e[m\]"
RED="\[\033[31m\]"
GREEN="\[\033[32m\]"
YELLOW="\[\e[;33m\]"
BLUE="\[\e[;34m\]"
PS1="\n[${RED}\j${WHITE}][${YELLOW}\t${WHITE}]${GREEN}\$(__git_ps1)${WHITE}\n{${BLUE}\w${WHITE}}$> "

source $HOME/datananas/env.sh
