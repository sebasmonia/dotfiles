#!/usr/bin/env python3
"""External screen setup via xnrandr. The internal screen is always
primary, unless turned off."""

import subprocess
import sys

def xrandr(params=None):
    p = ["xrandr"]
    if params:
        p.extend(str(e) for e in params)
    result = subprocess.run(p,
                            stdout=subprocess.PIPE)
    return result.stdout.decode('utf-8')

def get_outputs():
    data = xrandr().splitlines()
    return ([x.split()[0] for x in data if " connected" in x],
            [x.split()[0] for x in data if " disconnected" in x])


def get_arg_or_exit():
    global xrandr_params
    if len(sys.argv) < 2:
        sys.exit(1)
    arg = sys.argv[1]
    if arg not in xrandr_configs:
        sys.exit(1)
    return arg

xrandr_configs = {"internal": ["--output", "HDMI2", "--off",
                               "--output", "DP2", "--off",
                               "--output", "eDP1", "--auto", "--primary"],
                  "external": ["--output", "{external}", "--auto", "--primary",
                               "--output", "eDP1", "--off"],
                  "both-right": ["--output", "eDP1", "--auto", "--primary",
                                 "--output", "{external}", "--auto", "--right-of",
                                 "eDP1"],
                  "both-left": ["--output", "eDP1", "--auto", "--primary",
                                "--output", "{external}", "--auto", "--left-of",
                                "eDP1"]}

arg = get_arg_or_exit()

# This script is meant for ONE external output at a time
connected, disconnected = get_outputs()
external_output = "DP2" if "DP2" in connected else "HDMI2"

# If the config selected has a marker for {external}, replace it
params = [external_output if x == "{external}" else x
          for x in xrandr_configs[arg]]
print("\n\n!!!", params, "!!!\n\n")
xrandr(params)

subprocess.run(["killall", "conky"])
subprocess.run(["nitrogen", "--restore"])
subprocess.run(["start_conky_green"])
