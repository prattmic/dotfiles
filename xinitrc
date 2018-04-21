#!/bin/bash

# Sets up basic human interface support and starts the window manager

function run_xmonad() {
    trayer --edge top --align right --SetDockType true --SetPartialStrut true \
         --expand true --width 2 --transparent true --tint 0x000000 --height 12 &
    wicd-client --tray &
    keepassxc &

    exec ck-launch-session dbus-launch xmonad
}

# Set up basic terminal config
xrdb -merge "${HOME}/.Xresources"

# Keymapings
xmodmap ~/.xmodmap
xsetroot -cursor_name left_ptr &  # sets the cursor icon

# Multi-monitor support
sh ~/dotfiles/scripts/monitors.sh

run_xmonad
