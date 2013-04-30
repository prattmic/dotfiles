#!/bin/bash

# Sets up basic human interface support and starts the window manager

function run_xmonad() {
    trayer --edge top --align right --SetDockType true --SetPartialStrut true \
         --expand true --width 2 --transparent true --tint 0x000000 --height 12 &
    wicd-client --tray &

    exec xmonad
}

function run_i3() {
    exec i3
}

function run_awesome() {
    exec awesome
}

function run_gnome-session() {
    exec gnome-session
}

# Set up basic terminal config
xrdb -merge "${HOME}/.Xresources"

# Keymapings
xmodmap ~/.xmodmap
xsetroot -cursor_name left_ptr &  # sets the cursor icon

# Multi-monitor support
sh ~/dotfiles/scripts/monitors.sh

managers="xmonad i3 awesome gnome-session"

default_manager=xmonad

for manager in $managers; do
    if [[ $1 == $manager ]]; then
        found=1
        run_$manager
    fi
done

if [ -z $found ]; then
    run_$default_manager
fi
