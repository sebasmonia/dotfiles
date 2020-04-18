#!/usr/bin/env python3
"""Helper called from xfce4-panel to move the currently focused window
to a new container. It will pick the first one available between 1 and
10, and if none is avaiable will fail...I wouldn't expect that to happen...
"""

import json
import subprocess


def i3_msg_call(params):
    p = ["i3-msg"]
    p.extend(str(e) for e in params)
    result = subprocess.run(p,
                            stdout=subprocess.PIPE)
    return result.stdout.decode('utf-8')


get_workspaces = i3_msg_call(['-t', 'get_workspaces'])
in_use = set(w["num"] for w in json.loads(get_workspaces))
available = set(x for x in range(1, 11))
new_workspace = min(available - in_use)


i3_msg_call(["move", "workspace", new_workspace])
i3_msg_call(["workspace", new_workspace])
