#!/bin/bash
##
## list of aliases used in bash
##
alias emacs='emacs-29.0.50'
alias l='clear; ls -lh'
alias ..='cd ..; l'
alias cl='cd ~; l'
alias nfs_restart='sudo /etc/init.d/nfs-kernel-server restart'
#alias python='python3.5'
alias yt='mpsyt'
alias youtube='yt'
alias s="source ~/.bashrc"
alias map="telnet mapscii.me"
alias jgrep='grep -Fnr --exclude-dir={node_modules,.git,.yarn} --exclude yarn.lock --exclude .yarn-integrity --exclude .eslintcache --exclude \*.perl'
alias jgrephard='grep -Fnr --exclude-dir={dictionaries,node_modules,.git,.yarn,tests,test} --exclude \*.spec.js --exclude \*.md --exclude yarn.lock --exclude .yarn-integrity --exclude .eslintcache --exclude \*.json --exclude \*.csv --exclude \*.perl'
alias mongo='mongosh'

# Ubuntu/Linux 64-bit, GPU enabled, Python 3.5
# Requires CUDA toolkit 7.5 and CuDNN v4. For other versions, see "Install from sources" below.
export TF_BINARY_URL=https://storage.googleapis.com/tensorflow/linux/gpu/tensorflow-0.10.0rc0-cp35-cp35m-linux_x86_64.whl
