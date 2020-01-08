#!/bin/bash
intern=eDP1
extern=HDMI2

if xrandr | grep "$extern disconnected"; then
    xrandr --output HDMI2 --off --output eDP1 --auto --primary
else
    xrandr --output eDP1 --auto --output HDMI2 --auto --right-of eDP1 --primary
fi

nitrogen --restore
killall conky
start_conky_green &
