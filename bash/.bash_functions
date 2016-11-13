#!/bin/bash
##
## functions used in bash
##

work() {
    clear ; cd ~/datananas/ ; ls -lh
    detach slack spotify google-chrome
    #wargs
    echo "Starting mongod..."
    nohup mongod --dbpath='/home/jeremy/datananas/data' < /dev/null > /dev/null 2>&1 &
    disown $!
    echo "Done! Have a nice day ! ;)"
}

##
## decompress all kind of file
##
extract () {
    if [ ! -f "$1" ] ; then
      echo "'$1' is not a valid file!"
      return 1
    fi

    # Assoc. array of commands for extracting archives
    declare -A xcmd
    xcmd=(
      [.tar.bz2]="tar xvjf"
      [.tar.gz]="tar xvzf"
      [.bz2]="bunzip2"
      [.rar]="unrar x"
      [.gz]="gunzip"
      [.tar]="tar xvf"
      [.zip]="unzip"
      [.Z]="uncompress"
      [.7z]="7z x"
    )
    # extension aliases
    xcmd[.tbz2]="${xcmd[.tar.bz2]}"
    xcmd[.tgz]="${xcmd[.tar.gz]}"

    # See which extension the given file uses
    fext=""
    for i in ${!xcmd[@]}; do
      if [ $(grep -o ".\{${#i}\}$" <<< $1) == "$i" ]; then
        fext="$i"
        break
      fi
    done

    # Die if we couldn't discover what archive type it is
    if [ -z "$fext" ]; then
      echo "don't know how to extract '$1'..."
      return 1
    fi

    # Extract & cd if we can
    fbase=$(basename "$1" "$fext")
    if ${xcmd[$fext]} "$1" && [ -d "$fbase" ]; then
      cd "$fbase"
    fi
}

calc () {
     echo "$1" | bc
}

##
## change brightness to $1
## work for lenovo under ubuntu
brightness() {
    if [ "$#" -ne 1 ]; then
	echo "Missing brightness parameter [1;937]"
	exit
    fi
    if [ "$1" -gt 937 ]; then
	echo "Asked brightness is too high"
	exit
    fi
    if [ "$1" -lt 1 ]; then
	echo "Asked brightness is too small"
	exit
    fi
    echo "$1" | sudo tee /sys/class/backlight/intel_backlight/brightness
}

##
## destroy the current vagrant environment and destroy all virtualbox VM
## To use if I only have one VM
vagrant_clean() {
    vagrant destroy
    rm -rf .vagrant
    rm -rf "~/VirtualBox\ VMs/*"
}

##
## reduce brightness at its minimum and start gtk-redshift
##
night_mode() {
    brightness 1
    detach "gtk-redshift"
    l
}

##
## detach processes from the current terminal
##
detach() {
    if [ "$#" -eq 0 ]; then
	echo "Missing command parameter"
	exit
    fi
    for prgrm in "$@"; do
	echo "detaching $prgrm"
	nohup "$prgrm" < /dev/null > /dev/null 2>&1 &
	disown $! # $! -> pid of the last process
    done
}
