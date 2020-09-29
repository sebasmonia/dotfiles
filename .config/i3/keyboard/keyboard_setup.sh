#!/bin/bash

# First apply xkb configuration:
setxkbmap -layout us,us\(intl\) -variant altgr-intl -option "" -option ctrl:nocaps -option grp:shifts_toggle

# 1. US and US International layouts
# 2. the -variant parameter is so that US has a working right alt key
# 3. clear all options
# 4. apply caps as ctrl <3
# 5. use both shifts to toggle layout

# Second, apply xmodmap. In theory I can do this with _only_ xkb, but it seems so complicated that I'd rather
# keep this in xmodmap. With caps as control out of the way, the last change is "use print screen as menu"
xmodmap /home/hoagie/.config/i3/keyboard/xmodmap_prtscr_to_menu
