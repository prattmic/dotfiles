#!/bin/bash

# Configure monitors

set -e

set -x

function output_connected() {
    local name=$1
    xrandr | grep -q "$name connected"
}

# Start clean; everything off.
xrandr --output HDMI-1 --off
xrandr --output VGA-1 --off
xrandr --output LVDS-1 --off

# Only enable connected monitors. Ordered by priority. LVDS-1 will fail if
# HDMI-1 and VGA-1 are connected.
if output_connected "HDMI-1"; then
    xrandr --output HDMI-1 --preferred --primary || true
fi

if output_connected "VGA-1"; then
    xrandr --output VGA-1 --preferred --right-of HDMI-1 || true
fi

xrandr --output LVDS-1 --preferred --right-of HDMI-1 || true
