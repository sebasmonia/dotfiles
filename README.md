A place to store my Emacs configuration. I used to have more things here, but I simplified a lot of my setup.

## Emacs config

A [use-package](https://github.com/jwiegley/use-package) based configuration, that tries its best to be platform agnostic, even though nowadays I use Linux everywhere.  
In most cases I prefer smaller packages, if built-in, even better (eg icomplete over ivy/helm, project.el over projectile, etc.).  
I use [lsp-mode](https://emacs-lsp.github.io/lsp-mode/) for C# and Python LSP, haven't re-configured dap-mode yet (for debugging).  
I got used to [Sly](https://github.com/joaotavora/sly) for Common Lisp.  
My theme of choice is [modus-operandi](https://gitlab.com/protesilaos/modus-themes). Every once in a while I try other themes, but at most a day later I end up going back to modus.  
My mode line of choice is [mood-line](https://gitlab.com/jessieh/mood-line), a text-only variant of doom-modeline, with two patches: narrowing indicator, displaying region size when it is active.  

This config uses the Consolas font :)  

### Version numbers

Up until April 2022, the init file Version was arbitrary and bumped on milestones (moving to use-package, changing too many settings, etc.)  
Starting with Emacs 28, my config file version number will match the minimum Emacs version needed to run it. I will do my best to update the minor version on changes.  

## Where this runs

I run this configuration in Fedora Silverblue, with just a few extensions installed:

* [Clipboard history](https://extensions.gnome.org//extension/4839/clipboard-history/): I got used to having a clipboard history in Emacs, and in i3, now I need it in Gnome too.  
* [Tray icons reloaded](https://extensions.gnome.org//extension/2890/tray-icons-reloaded/): For Slack, mostly.  
* Background logo, because it looks fancy.  

I really like the idea of not layering anything over the base image.  

