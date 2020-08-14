A place to store my i3 and Emacs configuration, plus other misc scripts.

## Prerequisites for a fresh install:

* Manjaro i3 (duh!)
* Consolas font
* xbacklight, pulseaudio_ctl for brightness and volume controls
* acpid to show the touch panel automatically
* evdev-right-click-emulation for right click w/long touch tap
* Probably others I missed but will keep adding here

## Emacs config notes:

A use-package based configuration, that tries its best to be platform agnostic. I moved the Windows-only things to a 
private repo @ work, since they only make sense in that environment.

## i3 config notes:

* The monitor switch bound to Win+p has a mode with some common options, backed by `monitorswitch.py`. Win+Shift+p works as a lifesaver by enabling the laptop screen only.
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

## Thinkpad tablet mode notes

* Firefox touch scroll and zoom via: https://superuser.com/a/1485044
* Screen rotation via autorotate.sh, autostarted in i3 config
  * Script rotates three screen inputs (touch, pen input, pen eraser)
* Pen button configuration:
  * Tip is left click
  * Lower button is middle click
  * Top button (eraser) works on hover, used as right click
* Fingerprint reader doesn't have a driver yet.
* Touch control panel is activated when the laptop changes to table mode
  * Handled via tabletmode.py, autostarted in i3 config
  * Listens to all ACPI events, there's potential to handle more things
  * Details on panel buttons below
* Gestures and right click support via https://github.com/PeterCxy/evdev-right-click-emulation and the Gesture option in the Wacom drivers. In theory should use touchegg for those, since I don't have a DE.

### Touch keyboard

* Testing CellWriter as an input method. Like it quite a bit.
* Screen keyboard is `onboard`, added to the lightdm greeter
* Changed `/usr/bin/i3exit` to not lock the screen on suspend, more convenient to keep as tablet

### xfce4-panel

Modified from the idea of https://peterme.net/adding-touch-controls-to-the-i3-window-manager.html, it has a single panel at the bottom of the screen.

Buttons:
* Hold-for-menu button with window controls: toggle floating, move up/down/left/right
* Go to to previous/next workspace, from 1~10, wraps around
* Close focused application
* Move current app to a new workspace (uses `move_to_first_available_workspace.py`)
* Volume up/down, using a custom script to allow volume > 100%
* Brightness up/down, using a custom script to avoid the screen turning off when going to 0%
* Onboard. Added autocomplete to it.
* CellWriter/Xournal++/Application menu
* Sleep/Shutdown

Will keep experimenting with the buttons to see what else I need and what I can discard. Configuring the panel is a drag, I added to the repository all the xfce4 config stuff (panel + power manager).

This is how the panel looks:

![Control bar screenshot](/screenshots/Bar-vertical.png)

