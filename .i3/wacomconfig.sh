#!/bin/bash

# Top button is right click. Works on hover, which is convenient to open context menus.
# Default value: button +2 (middle click held down)
xsetwacom set "Wacom Pen and multitouch sensor Pen stylus" Button 2 "3"

# Bottom button, pressed when touching the screen is middle click
# Default value: button +1 (same as regular click)
xsetwacom set "Wacom Pen and multitouch sensor Pen eraser" Button 1 "2"


# ???
xsetwacom set "Wacom Pen and multitouch sensor Pen eraser" Touch on
xsetwacom set "Wacom Pen and multitouch sensor Pen stylus" Touch on


# In theory I should use touchegg instead of these two, something to look at in the future:

# Get zoom gestures working - UPDATE: has a bug in current version
# xsetwacom set "Wacom Pen and multitouch sensor Finger touch" Gesture on
# Enable "long tap for right click" via evdev-rce, only for touch screen
TOUCH_DEVICE_BLACKLIST="Wacom Pen and multitouch sensor Pen|Elan Touchpad" evdev-rce &
