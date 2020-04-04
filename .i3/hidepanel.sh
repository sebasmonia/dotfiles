#!/bin/sh
# Modified from: https://peterme.net/adding-touch-controls-to-the-i3-window-manager.html

WID=$(xwininfo -root -tree -children | grep "xfce4-panel" | grep "x66" | { read first _ ; echo $first; }) # Get the window ID of the panel

xdotool windowunmap "$WID"
