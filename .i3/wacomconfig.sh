#!/bin/bash

# Top button is right click. Works on hover, which is convenient to open context menus.
# Default value: button +2 (middle click held down)
xsetwacom set "Wacom Pen and multitouch sensor Pen stylus" Button 2 "3"

# Bottom button, pressed when touching the screen is middle click
# Default value: button +1 (same as regular click)
# xsetwacom set "Wacom Pen and multitouch sensor Pen eraser" Button 1 "2"

# Experimental: Bottom button, pressed when touching the screen brings up CellWriter
# Uses xbindkeys to run a script on mouse button 11
xsetwacom set "Wacom Pen and multitouch sensor Pen eraser" Button 1 "11"
