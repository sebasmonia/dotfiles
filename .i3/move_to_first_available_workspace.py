#!/usr/bin/env python3
"""Helper called from xfce4-panel to move the currently focused window
to a new container. Since when used in table mode I don't expect to have
more than 4 or maybe 5 containers, no error or limit checking is done
in the script. There isn't a check for empty workspaces in the middle
either."""

import json
import subprocess


def i3_msg_call(params):
    p = ["i3-msg"]
    p.extend(str(e) for e in params)
    result = subprocess.run(p,
                            stdout=subprocess.PIPE)
    return result.stdout.decode('utf-8')


get_workspaces = i3_msg_call(['-t', 'get_workspaces'])
workspaces = json.loads(get_workspaces)
new_workspace = max(w["num"] for w in workspaces) + 1

i3_msg_call(["move", "workspace", new_workspace])
i3_msg_call(["workspace", new_workspace])
