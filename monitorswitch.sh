#!/bin/bash
intern=eDP1
extern=HDMI1

if xrandr | grep "$extern disconnected"; then
    xrandr --output "$extern" --off --output "$intern" --auto
else
    xrandr --output eDP1 --primary --auto --output HDMI1 --auto --right-of eDP1
fi

nitrogen --restore
killall conky
start_conky_green
killall keynav
/home/hoagie/keynav/keynav
