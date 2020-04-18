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


def notify_send(msg, timeout=1000):
    p = ["notify-send", "-t", str(timeout), msg]
    subprocess.run(p)


def get_arg_or_exit():
    if len(sys.argv) < 2:
        sys.exit(1)
    arg = sys.argv[1]
    if arg not in ("inc", "dec"):
        sys.exit(1)
    return arg


arg = get_arg_or_exit()
current_value = int(xbacklight(["-get"]))
if arg == "dec" and (current_value - 5) < 1:
    # fail safe so we don't go too low
    notify_send("If brightness goes lower the screen"
                " will turn off...", 2000)
else:
    xbacklight(["-" + arg, 5])
    notify_send("Brightness: " + xbacklight(["-get"]).strip() + "%")
