#!/usr/bin/env python3
"""Select an audio output profile, using only the keyboard.
Relies on pacmd and ~dmenu~ rofi."""

import subprocess

selected = ""
profiles_list = {}


def pacmd(params):
    p = ["pacmd"]
    p.extend(str(e) for e in params)
    result = subprocess.run(p,
                            stdout=subprocess.PIPE)
    return result.stdout.decode('utf-8')


def dmenu(options, prompt=None, lines=None):
    p = ["dmenu", "-i"]
    if lines:
        p.extend(("-l", str(lines)))
    if prompt:
        p.extend(("-p", prompt))
    input_text = "\n".join(options).encode()
    result = subprocess.run(p,
                            input=input_text,
                            stdout=subprocess.PIPE)
    return result.stdout.decode('utf-8').strip()


def rofi(options, prompt=None, lines=None):
    p = ["rofi", "-dmenu", "-i"]
    if lines:
        p.extend(("-l", str(lines)))
    if prompt:
        p.extend(("-p", prompt))
    input_text = "\n".join(options).encode()
    result = subprocess.run(p,
                            input=input_text,
                            stdout=subprocess.PIPE)
    return result.stdout.decode('utf-8').strip()


def populate_profiles_data():
    global selected
    global profiles_list
    try:
        output = pacmd(("list-cards",))
        lines = [x.strip() for x in output.splitlines()]
        profiles = lines.index("profiles:")
        active_profile = [x for x in lines if "active profile" in x][0]
        active_profile_ndx = lines.index(active_profile)
        profiles_list = filter_profiles(lines[profiles + 1:active_profile_ndx])
        # reduce the line to keep the clean profile key
        active_profile = active_profile.split(": ")[1][1:-1]
        # from https://stackoverflow.com/a/8023329/91877, find key by value
        selected = next((name for name, profile in profiles_list.items()
                         if profile == active_profile), None)
    except Exception as e:
        print(e)
        profiles_list = ("Error retrieving list of profiles",)


def filter_profiles(raw_lines):
    output_lines = [x for x in raw_lines if "output:" in x]
    formatted = {}
    for line in output_lines:
        profile, text, _ = line.split(": ")
        text = text.split(" (priori")[0]
        formatted[text] = profile
    # sort by key before returning
    return dict(sorted(formatted.items()))


populate_profiles_data()
prompt = "Current: " + selected
profile_name = dmenu(profiles_list.keys(), prompt, 10)
# profile_name = rofi(profiles_list.keys(), prompt, 10)

if profile_name:
    pacmd(("set-card-profile", "0", profiles_list[profile_name]))
