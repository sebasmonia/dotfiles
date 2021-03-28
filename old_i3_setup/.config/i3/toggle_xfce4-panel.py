#!/usr/bin/env python3
"""Toggle xfce4-panel, invoked via 3-finger swipe up using Gester."""

import subprocess


def is_panel_running():
    p = ["pidof", "xfce4-panel"]
    result = subprocess.run(p,
                            stdout=subprocess.PIPE)
    # '' if not running, else the process id
    return result.stdout.decode('utf-8')


if is_panel_running():
    subprocess.run("xfce4-panel -q &", shell=True)
else:
    subprocess.run("xfce4-panel -d &", shell=True)
