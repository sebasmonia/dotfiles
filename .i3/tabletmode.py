#!/usr/bin/env python3
"""Shows or hides the touch control panel. There's potential to handle volume
and brightness and many other things using this same script. But the solutions
I have now for those work ok. The script can easily be expanded for more...
Based on https://stackoverflow.com/a/13530331/91877

Don't forget to start acpid.service, and also autostart it:
sudo systemctl enable acpid.service.
"""
import socket
import subprocess

DEBUG = False


def debug_message(msg):
    if DEBUG:
        print(msg)


def handle_tabletmode(event_data):
    if (event_data[0] != "video/tabletmode"):
        return
    going_tablet = (event_data[3][-1] == "1")
    panel_call = ["xfce4-panel", "-d"]
    # If changing to tablet, runs the panel, else
    # make the panel close
    if not going_tablet:
        panel_call.append("-q")
    subprocess.Popen(panel_call)


sock = socket.socket(socket.AF_UNIX, socket.SOCK_STREAM)
sock.connect("/var/run/acpid.socket")
debug_message("Connected to acpid")
while True:
    raw_event = sock.recv(4096)
    for event in raw_event.decode().split('\n'):
        if event:
            data = event.split()
            debug_message(data)
            # TODO: add a dictionary data[0] => function
            # to handle multiple events
            handle_tabletmode(data)
