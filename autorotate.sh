#!/bin/sh
# Source: https://gist.github.com/mortie/e725d37a71779b18e8eaaf4f8a02bf5b
# via Reddit

# Automatically rotate the screen when the device's orientation changes.

monitor-sensor \
	| grep --line-buffered "Accelerometer orientation changed" \
	| grep --line-buffered -o ": .*" \
	| while read -r line; do
		line="${line#??}"
		if [ "$line" = "normal" ]; then
			rotate=normal
			matrix="0 0 0 0 0 0 0 0 0"
		elif [ "$line" = "left-up" ]; then
			rotate=left
			matrix="0 -1 1 1 0 0 0 0 1"
		elif [ "$line" = "right-up" ]; then
			rotate=right
			matrix="0 1 0 -1 0 1 0 0 1"
		elif [ "$line" = "bottom-up" ]; then
			rotate=inverted
			matrix="-1 0 1 0 -1 1 0 0 1"
		else
			echo "Unknown rotation: $line"
			continue
		fi

		xrandr --output "eDP1" --rotate "$rotate"
		xinput set-prop "Wacom Pen and multitouch sensor Finger touch" --type=float "Coordinate Transformation Matrix" $matrix
		xinput set-prop "Wacom Pen and multitouch sensor Pen stylus" --type=float "Coordinate Transformation Matrix" $matrix
		xinput set-prop "Wacom Pen and multitouch sensor Pen eraser" --type=float "Coordinate Transformation Matrix" $matrix
	done
