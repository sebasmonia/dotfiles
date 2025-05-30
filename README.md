A place to store my keyboard and Emacs configuration. I used to have more things here, but I
simplified a lot of my setup.

## Emacs config

A [use-package](https://www.gnu.org/software/emacs/manual/html_mono/use-package.html) based
configuration, that tries its best to be platform agnostic.  
In most cases I prefer smaller packages, if built-in, even better (eg project.el over projectile).  
In the same spirit, I use Eglot for my LSP needs.  
My package of preference for Common Lisp is [Sly](https://github.com/joaotavora/sly), a fork of
SLIME.  
My theme of choice is [modus-operandi](https://gitlab.com/protesilaos/modus-themes). Every once in a
while I try other themes, but at most a day later I end up going back to modus.  
I use a custom mode-line, based on [mood-line](https://gitlab.com/jessieh/mood-line) (a text-variant
of doom-modeline). I had enough little patches and additions to it that I figured I might as well do
my own thing. Which is of course more limited in scope, but tailored to the packages I use.  
&nbsp;  
I have a very soft spot for the 
[Consolas font](https://learn.microsoft.com/en-us/typography/font-list/consolas) (showing my old
time MS heritage here) and in the past I have used [IBM Plex Mono](https://www.ibm.com/plex/), and
[Iosevka Comfy Wide Fixed](https://git.sr.ht/~protesilaos/iosevka-comfy).  
My lastest font infatuation, which I loved so much that after a few weeks I ended up buying, is 
[Berkeley Mono](https://berkeleygraphics.com/typefaces/berkeley-mono/).  

### Version numbers

Up until April 2022, the init file Version was arbitrary and bumped on milestones (moving to 
use-package, changing too many settings, etc.)  
Starting with Emacs 28, my config file version number will match the minimum Emacs version needed to
run it. I do my best to update the minor version on "big" changes.  

## Keyboard config

I have a Dygma Raise. There's a README with notes about the config, and exported JSON of my current
layers, in the corresponding sub-directory.  
&nbsp;  
There's a legacy setup using [Karabiner](https://karabiner-elements.pqrs.org/) for macOS, from when
I had a Mac at work.  
And another legacy setup with [KMonad](https://github.com/kmonad/kmonad) from before I had the Raise
and used a Mistel MD770 instead.

## Where this runs

### Home setup

Lenovo L390 Yoga running Fedora Workstation, with just a few extensions installed:

* [Clipboard history](https://extensions.gnome.org//extension/4839/clipboard-history/): I got used
to having a clipboard history in Emacs, and in i3, now I need it in Gnome too.  
* [Tray icons reloaded](https://extensions.gnome.org//extension/2890/tray-icons-reloaded/): For
~~Slack~~ Discord, mostly.  
* Background logo, because it looks fancy.  

### Work setup

HP ZBook Power 15.6" G10 running Windows 11. Emacs compiled from source using MinGW, executed under
Win32, with native-comp.  
I could use WSL or a VM, but I'd rather not have one more layer of indirection (plus I used Windows
for so many years, I can find my way around it...).  



