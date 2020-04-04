#!/usr/bin/env python3
"""Helper called from xfce4-panel to move the the next/prev workspace
depending on the one currently selected."""

import json
import subprocess
import sys

def i3_msg_call(params):
    p = ["i3-msg"]
    p.extend(str(e) for e in params)
    result = subprocess.run(p,
                            stdout=subprocess.PIPE)
    return result.stdout.decode('utf-8')


def get_arg_or_exit():
    if len(sys.argv) < 2:
        sys.exit(1)
    arg = sys.argv[1]
    if arg not in ("prev", "next"):
        sys.exit(1)
    return arg

arg = get_arg_or_exit()
get_workspaces = i3_msg_call(['-t', 'get_workspaces'])
workspaces = json.loads(get_workspaces)
visible = [w for w in workspaces if w["visible"]][0]

if arg == "prev":
    select = visible["num"] - 1
else: # next
    select = visible["num"] + 1

# bound checking
if select < 1:
    select = 10
if select > 10:
    select = 1

i3_msg_call(["workspace", select])
