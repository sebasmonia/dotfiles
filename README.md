Just a place to store my Emacs configuration plus other misc scripts.

Prerequisites for a fresh install:

* IBM Plex Mono font
* Deadgrep requires rg

There are other third party dependencies, but those should leave the more important things working

Emacs config notes:

The config grew a lot (as they do...) and was recently simplified with use-package. I also moved the Windows-only things to a 
private repo @ work, since they only make sense in that environment.
I had a set of functions to determine the machine running and other config, the new approach means I was able to remove them.
Almost like a fresh start! :)

i3 config notes:
* The monitor switch bound to Win+P  is tied to my specific needs to my current laptop, didn't try to make it generic.
* .xmodmap swaps ctrl and caps, also makes left control menu for the BT keyboard
* From https://bbs.archlinux.org/viewtopic.php?id=223949, to activate BT @ startup:

```
  1. set bluetooth service to start on boot: systemctl enable bluetooth.service
  2. set bluetooth adapter to automatically power on: edit /etc/bluetooth/main.conf and set AutoEnable=true
  3. set paired devices as trusted *this is what I was missing*: from the bluetoothctl util, enter trust XX:XX:XX:XX:XX:XX for each paired device (replace XX... with mac address)
  Also:
  https://wiki.archlinux.org/index.php/Bluetooth_keyboard
```
