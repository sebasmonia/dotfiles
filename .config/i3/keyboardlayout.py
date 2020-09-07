#!/usr/bin/env python3
"""Toggle the keyboard layout between US and US-International.
The later is useful si quiero escribir los caracteres con tilde de
mi español nativo :)"""

import subprocess
import sys


def setxkbmap(params):
    p = ["setxkbmap"]
    p.extend(str(e) for e in params)
    result = subprocess.run(p,
                            stdout=subprocess.PIPE)
    return result.stdout.decode('utf-8')


def apply_xmodmap():
    p = ["xmodmap", "/home/hoagie/.Xmodmap"]
    subprocess.run(p)


def show_notification(msg):
    p = ["dunstify", "-t", "1000", "-r", "250000", msg]
    subprocess.run(p)


# Harcoded list...
layouts = ["us", "us(intl)"]

xkb_query = setxkbmap(["-query"]).splitlines()
layout_line = [line for line in xkb_query if "layout" in line][0]
current_value = layout_line.split(":")[1].strip()

# modulo to wrap around the list
new_value = layouts[(layouts.index(current_value) + 1) %
                    len(layouts)]

setxkbmap(["-layout", new_value])
apply_xmodmap()

show_notification("Keyboard layout: " + new_value)
