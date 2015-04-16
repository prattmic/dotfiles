#!/bin/bash

# Configure monitors
# If any are missing, they will not affect the others

xrandr --output LVDS1 --off
xrandr --output HDMI1 --preferred --right-of LVDS1 --primary
xrandr --output VGA1 --preferred --right-of HDMI1
