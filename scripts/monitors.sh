#!/bin/bash

# Configure monitors

set -e

function output_connected() {
    local name=$1
    xrandr | grep -q "$name connected"
}

# LVDS1 needs to be turned off before the others can be turned on
if output_connected "HDMI-1" && output_connected "VGA-1"; then
    xrandr --output LVDS-1 --off
fi


xrandr --output HDMI-1 --preferred --right-of LVDS-1 --primary || true
xrandr --output VGA-1 --preferred --right-of HDMI-1 || true
xrandr --output LVDS-1 --preferred --left-of HDMI-1 || true
