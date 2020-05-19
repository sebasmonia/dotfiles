#!/usr/bin/env python3
"""External screen setup via xnrandr. The internal screen is always
primary, unless turned off."""

import subprocess
import sys


def get_arg_or_exit():
    global xrandr_params
    if len(sys.argv) < 2:
        sys.exit(1)
    arg = sys.argv[1]
    if arg not in xrandr_params:
        sys.exit(1)
    return arg


xrandr_params = {"internal": ["--output", "HDMI2", "--off",
                              "--output", "eDP1", "--auto", "--primary"],
                 "external": ["--output", "HDMI2", "--auto", "--primary",
                              "--output", "eDP1", "--off"],
                 "both-right": ["--output", "eDP1", "--auto", "--primary",
                                "--output", "HDMI2", "--auto", "--right-of",
                                "eDP1"],
                 "both-left": ["--output", "eDP1", "--auto", "--primary",
                               "--output", "HDMI2", "--auto", "--left-of",
                               "eDP1"]}

arg = get_arg_or_exit()

params = ["xrandr"]
params.extend(xrandr_params[arg])
subprocess.run(params)

subprocess.run(["killall", "conky"])
subprocess.run(["nitrogen", "--restore"])
subprocess.run(["start_conky_green"])
