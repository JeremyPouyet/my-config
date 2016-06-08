#!/bin/bash
##
## list of aliases used in bash
##
alias cleaner='rm *~ ; clear'
alias l='cleaner; ls -lh'
alias s='sudo '
alias ne='emacs -nw'
alias sne='s emacs -nw'
alias ..='cd ..; l'
alias cl='cd ~; l'
alias nfs_restart='sudo /etc/init.d/nfs-kernel-server restart'