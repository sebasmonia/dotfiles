#!/bin/sh
# Source: https://gist.github.com/mortie/e725d37a71779b18e8eaaf4f8a02bf5b
# (via Reddit)
# 2020-02-22 After reporting a bug in xsetwacom, which wasn't really their
# problem, updated the script to use xsetwacom and no matrix, see
# https://github.com/linuxwacom/xf86-input-wacom/wiki/Rotation

# Automatically rotate the screen when the device's orientation changes.

monitor-sensor \
	| grep --line-buffered "Accelerometer orientation changed" \
	| grep --line-buffered -o ": .*" \
	| while read -r line; do
		line="${line#??}"
		if [ "$line" = "normal" ]; then
			screenrotate=normal
                        wacomrotate=none
		elif [ "$line" = "left-up" ]; then
			screenrotate=left
                        wacomrotate=ccw
		elif [ "$line" = "right-up" ]; then
			screenrotate=right
                        wacomrotate=cw
		elif [ "$line" = "bottom-up" ]; then
			screenrotate=inverted
                        wacomrotate=half
		else
			echo "Unknown rotation: $line"
			continue
		fi

		xrandr --output "eDP1" --rotate "$screenrotate"
                xsetwacom set "Wacom Pen and multitouch sensor Finger touch" rotate $wacomrotate
                xsetwacom set "Wacom Pen and multitouch sensor Pen stylus" rotate $wacomrotate
                xsetwacom set "Wacom Pen and multitouch sensor Pen eraser" rotate $wacomrotate
                sleep 1s
                nitrogen --restore
	done
