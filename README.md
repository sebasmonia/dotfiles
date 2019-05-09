Just a place to store my Emacs configuration plus other misc scripts.

Prerequisites for a fresh install:

* Consolas font (yes, can be installed in Linux too)
* For the theme to load correctly you need to launch Emacs twice
* Deadgrep requires rg

There are other third party dependencies, but those should leave the more important things working

Emacs config notes:

The config grew a lot (as they do...) and was recently simplified with use-package. I also moved the Windows-only things to a 
private repo @ work, since they only make sense in that environment.
I had a set of functions to determine the machine running and other config, the new approach means I was able to remove them.
Almost like a fresh start! :)

i3 config notes:
* The monitor switch bound to Win+P  is tied to my specific needs to my current laptop, didn't try to make it generic.
* .xmodmap is limited to making CAPS act like Control. Seems a better choice than setxbmap, since it works with BT keyboard even after it sleeps.
