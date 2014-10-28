#!/bin/bash

# Configure monitors
# If any are missing, they will not affect the others

xrandr --output LVDS1 --off
xrandr --output HDMI1 --preferred --right-of LVDS1
xrandr --output VGA1 --preferred --left-of HDMI1 --primary
