#!/usr/bin/env python3
"""Helper to adjust brightness from xfce4-panel, because I hate it when the
screen turns off and I have to leave tablet mode to turn it on"""

import subprocess
import sys


def xbacklight(params):
    p = ["xbacklight"]
    p.extend(str(e) for e in params)
    result = subprocess.run(p,
                            stdout=subprocess.PIPE)
    return result.stdout.decode('utf-8')


def show_notification(msg):
    p = ["dunstify", "-t", "1000", "-r", "25252525", msg]
    subprocess.run(p)


def get_arg_or_exit():
    if len(sys.argv) < 2:
        sys.exit(1)
    arg = sys.argv[1]
    if arg not in ("inc", "dec"):
        sys.exit(1)
    return arg


brightness_step = 5
arg = get_arg_or_exit()
current_value = int(xbacklight(["-get"]))
if current_value <= brightness_step and arg == "dec":
    xbacklight(["-set", 1])
elif current_value < brightness_step and arg == "inc":
    xbacklight(["-set", brightness_step])
else:
    xbacklight(["-" + arg, brightness_step])

show_notification("Brightness: " + xbacklight(["-get"]).strip() + "%")
