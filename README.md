A place to store my i3 and Emacs configuration, plus other misc scripts.

## Prerequisites for a fresh install:

* Manjaro i3 (duh!)
* i3blocks
* Consolas font
* i3blocks, sysstat (for tablet mode i3status)
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
* If brightness keys do nothing, remember to check xfce4-powermanager

## Thinkpad tablet mode notes

* Firefox touch scroll and zoom via: https://superuser.com/a/1485044
* Screen rotation via autorotate.sh, autostarted in i3 config
  * Script rotates three screen inputs (touch, pen input, pen eraser)
* Via i3blocks,there are  buttons in the status bar for some actions:
  * First one opens xfce4-panel
  * Next is `onboard`
  * Last button is to close the current application
* Pen button configuration:
  * Tip is left click
  * Lower button is middle click
  * Top button (eraser) is right click
* Fingerprint reader doesn't have a driver yet.

### Touch keyboard

* Testing CellWriter as an input method. Like it quite a bit
* Screen keyboard is `onboard`, added to the lightdm greeter
* Change `/usr/bin/i3exit` to use `dm-tool lock` instead of `blurlock`

### xfce4-panel

Modified from the idea of https://peterme.net/adding-touch-controls-to-the-i3-window-manager.html, it has two panels.

Panel 1: switch workspaces
Panel 2: close panel, volume up/down, brightness up/down, onboard/folders/application menu, sleep/shutdown

Will keep experimenting with the buttons to see what else I need and what I can discard. Configuring the panel is a drag, I added to the repository all the xfce4 config stuff (panel + power manager).
