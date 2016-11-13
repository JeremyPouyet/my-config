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
alias python='python3.5'

# Ubuntu/Linux 64-bit, GPU enabled, Python 3.5
# Requires CUDA toolkit 7.5 and CuDNN v4. For other versions, see "Install from sources" below.
export TF_BINARY_URL=https://storage.googleapis.com/tensorflow/linux/gpu/tensorflow-0.10.0rc0-cp35-cp35m-linux_x86_64.whl
