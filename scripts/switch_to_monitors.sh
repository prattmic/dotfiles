#!/bin/bash

# Configure monitors
# If any are missing, they will not affect the others

xrandr --output LVDS-1 --off
xrandr --output HDMI-1 --preferred --right-of LVDS-1 --primary
xrandr --output VGA-1 --preferred --right-of HDMI-1
