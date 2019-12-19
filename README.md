A place to store my i3 and Emacs configuration, plus other misc scripts.

## Prerequisites for a fresh install:

* Manjaro i3 (duh!)
* Consolas font
* Deadgrep requires rg

There are other third party dependencies, but those should leave the more important things working

## Emacs config notes:

A use-package based configuration, that tries its best to be platform agnostic. I moved the Windows-only things to a 
private repo @ work, since they only make sense in that environment.

## i3 config notes:

* The monitor switch bound to Win+P activates internal monitor + HDMI if something is plugged, else only internal monitor. All screen names and positions are hardcoded to my setup.
* .Xmodmap swaps ctrl and caps, and maps Menu to Print Screen because Thinkpad keyboards.
* From https://bbs.archlinux.org/viewtopic.php?id=223949, to activate BT @ startup:

```
  1. set bluetooth service to start on boot: systemctl enable bluetooth.service
  2. set bluetooth adapter to automatically power on: edit /etc/bluetooth/main.conf and set AutoEnable=true
  3. set paired devices as trusted *this is what I was missing*: from the bluetoothctl util, enter trust XX:XX:XX:XX:XX:XX for each paired device (replace XX... with mac address)
  Also:
  https://wiki.archlinux.org/index.php/Bluetooth_keyboard
```
* Probaly should move the conky changes to my home folder. I usually drop the files in this repo to /usr/share/conky, overriding the defaults
* All `sh` scripts go in /home/hoagie/.i3

## Thinkpad tablet mode notes

* Firefox touch scroll and zoom via: https://superuser.com/a/1485044
* Screen rotation via autorotate.sh, autostarted in i3 config
  * Script rotates all three screen inputs (touch + 2 devices for pen)
* Pen button configuration coming soon
* There is a chance the fingerprint reader can work. Coming soon too
* Touch keyboard via `onboard`, started/stopped via libinput-gestures
