#!/usr/bin/env python3
"""Display and input rotation. Invoked via Gester."""

import subprocess
import sys
import time

wacom_to_xrandr = {"none": "normal",
                   "ccw": "right",
                   "cw": "left",
                   "half": "inverted"}

wacom_devices = ["Wacom Pen and multitouch sensor Finger touch",
                 "Wacom Pen and multitouch sensor Pen stylus",
                 "Wacom Pen and multitouch sensor Pen eraser"]


orientations = ["none", "cw", "half", "ccw"]


def determine_next_orientation(gesture_direction):
    """Based on the current orientation and the gesture made, determine
what should be the next orientation."""
    global orientations
    # There are fancier ways of doing this but I don't want to get lost
    # on the logic:
    if gesture_direction == "left":
        direction = +1
    elif gesture_direction == "right":
        direction = -1
    elif gesture_direction == "flip":
        direction = +2

    current = orientations.index(xsetwacom_get_orientation())
    # Using modulo operator to wrap around the list in case we are
    # at one of the edges
    return orientations[(current + direction) % len(orientations)]


def xrandr_rotate(orientation):
    """Apply rotation to the laptop screen."""
    global wacom_to_xrandr
    # first, convert the parameter from xsetwacom format to xrandr format
    xr_orientation = wacom_to_xrandr[orientation]
    p = ["xrandr", "--output", "eDP1", "--rotate", xr_orientation]
    result = subprocess.run(p,
                            stdout=subprocess.PIPE)
    return result.stdout.decode('utf-8')


def xsetwacom_rotate(orientation):
    """Apply rotation to the touch devices (pen & screen)."""
    global wacom_devices
    outputs = []
    for device in wacom_devices:
        p = ["xsetwacom", "set", device, "rotate", orientation]
        result = subprocess.run(p, stdout=subprocess.PIPE)
        outputs.append(result.stdout.decode('utf-8'))
    # xsetwacom doesn't seem to return anything...it does print output
    # just doesn't return it. Still:
    return outputs


def xsetwacom_get_orientation():
    """Get current orientation from xsetwacom."""
    # most scripts I've seen parse the current orientation from xrandr, but
    # turns out it's really easy to get it from xsetwacom instead, no need to
    # analyze many lines of script
    p = ["xsetwacom", "get", "Wacom Pen and multitouch sensor Finger touch",
         "rotate"]
    result = subprocess.run(p,
                            stdout=subprocess.PIPE)
    return result.stdout.decode('utf-8').strip()


def get_arg_or_exit():
    if len(sys.argv) < 2:
        sys.exit(1)
    arg = sys.argv[1]
    # gester attaches the angle to rotation parameter (!), I tried adding a
    # space but then there's a newline (!!) so, workaround:
    if "left" in arg:
        return "left"
    if "right" in arg:
        return "right"
    return "flip"


arg = get_arg_or_exit()

subprocess.run(["killall", "gester"])
subprocess.run(["killall", "conky"])

new_orientation = determine_next_orientation(arg)
xrandr_rotate(new_orientation)
xsetwacom_rotate(new_orientation)

# slight delay so everyone catches up on the changes
time.sleep(0.25)

# in theory I should be sending Gester a SIGUSR1 signal when rotating
# the screen, but it didn't register, and since restarting it is quite cheap...
subprocess.run("gester &", shell=True)
# another slight delay so everyone else catches up on the changes
time.sleep(0.5)
subprocess.run(["nitrogen", "--restore"])
subprocess.run(["/home/hoagie/.config/i3/start_conky.sh"])
