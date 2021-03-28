#!/usr/bin/env python3
"""Helper for i3 to adjust volume over 100%, since pa-applet doesn't support
it and other solutions seem more complicated than writing this :)"""

import subprocess
import sys

def pulseaudio_ctl(params):
    p = ["pulseaudio-ctl"]
    p.extend(str(e) for e in params)
    result = subprocess.run(p,
                            stdout=subprocess.PIPE)
    return result.stdout.decode('utf-8')


def show_notification(msg):
    p = ["dunstify", "-t", "1000", "-r", "252525", msg]
    subprocess.run(p)


def get_arg_or_exit():
    if len(sys.argv) < 2:
        sys.exit(1)
    arg = sys.argv[1]
    if arg not in ("up", "down", "mute"):
        sys.exit(1)
    return arg


arg = get_arg_or_exit()
pulseaudio_ctl(list(arg))
volume, mute, _ = pulseaudio_ctl(["full-status"]).split()
if mute == "yes":
    show_notification(f"Audio muted")
else:
    show_notification(f"Volume: {volume}%")
