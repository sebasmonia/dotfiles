A place to store my keyboard and Emacs configuration. I used to have more things here, but I simplified a lot of my setup.

## Emacs config

A [use-package](https://github.com/jwiegley/use-package) based configuration, that tries its best to be platform agnostic.  
In most cases I prefer smaller packages, if built-in, even better (eg icomplete over ivy/helm, project.el over projectile, etc.).  
I use/used [lsp-mode](https://emacs-lsp.github.io/lsp-mode/) for C#, Python, Java, Go and PHP. Haven't revisited dap-mode but I used it in the past for C# with success.  
I use [Sly](https://github.com/joaotavora/sly) for Common Lisp.  
My theme of choice is [modus-operandi](https://gitlab.com/protesilaos/modus-themes). Every once in a while I try other themes, but at most a day later I end up going back to modus.  
My mode line of choice is [mood-line](https://gitlab.com/jessieh/mood-line), a text-only variant of doom-modeline, with two patches: narrowing indicator, and displaying region size when it is active.  

I have a very soft spot for the Consolas font (showing my old time MS heritage here) and in the past I have used IBM Plex Mono. Nowadays though, my font of choice is [Iosevka Comfy Wide Fixed](https://git.sr.ht/~protesilaos/iosevka-comfy)

### Version numbers

Up until April 2022, the init file Version was arbitrary and bumped on milestones (moving to use-package, changing too many settings, etc.)  
Starting with Emacs 28, my config file version number will match the minimum Emacs version needed to run it. I will do my best to update the minor version on changes.  

## Keyboard config

I have a pair of [Mistel MD770 Bluetooth](https://mistelkeyboard.com/products/94f05206cb24bbeeb103e664e89d7b98) keyboards (blue & brown switches). The most important configuration set directly in the keyboards is remapping the right spacebar to control.  

The rest of the configuration is handled by [KMonad](https://github.com/kmonad/kmonad):  

* CapsLock is Control
* Tapping the spacebar inserts a space, holding it down is Control  
* Tapping control produces ESC, holding (for chording) is Control  

In Emacs, ESC ~= meta/alt. For example C-M-SPC `mark-sexp` is input only with thumbs by tapping the right spacebar (ESC), then holding it (C), and pressing space on the left side (SPC).  

There's a legacy setup using [Karabiner](https://karabiner-elements.pqrs.org/) for macOS, from when I used that at work.

## Where this runs

### Home setup

Lenovo L390 Yoga running Fedora Silverblue, with just a few extensions installed:

* [Clipboard history](https://extensions.gnome.org//extension/4839/clipboard-history/): I got used to having a clipboard history in Emacs, and in i3, now I need it in Gnome too.  
* [Tray icons reloaded](https://extensions.gnome.org//extension/2890/tray-icons-reloaded/): For Slack, mostly.  
* Background logo, because it looks fancy.  

I really like the idea of not layering anything over the base image, so all apps are installed via Flatpak, except Emacs that I compile in each Toolbox.  

### Work setup

Dell Latitude 5421 with Windows 10.  



