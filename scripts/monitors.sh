#!/bin/bash

# Configure monitors

set -e

function output_connected() {
    local name=$1
    xrandr | grep -q "$name connected"
}

# LVDS1 needs to be turned off before the others can be turned on
if output_connected "HDMI1" && output_connected "VGA1"; then
    xrandr --output LVDS1 --off
fi


xrandr --output HDMI1 --preferred --right-of LVDS1 --primary || true
xrandr --output VGA1 --preferred --right-of HDMI1 || true
xrandr --output LVDS1 --preferred --left-of HDMI1 || true
