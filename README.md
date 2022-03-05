A place to store my Emacs configuration. I used to have more things here, but I simplified a lot of my setup.

## Emacs config

A [use-package](https://github.com/jwiegley/use-package) based configuration, that tries its best to be platform agnostic, even though nowadays I use Linux everywhere.  
In most cases I prefer smaller packages, if built-in, even better (eg icomplete over ivy/helm, project.el over projectile, etc.).  
I use [lsp-mode](https://emacs-lsp.github.io/lsp-mode/) for C# and Python LSP, haven't re-configured dap-mode yet (for debugging).  
I got used to [Sly](https://github.com/joaotavora/sly) for Common Lisp.  
My theme of choice is [modus-operandi](https://gitlab.com/protesilaos/modus-themes). Every once in a while I try other themes, but at most a day later I end up going back to modus.  
My mode line of choice is [mood-line](https://gitlab.com/jessieh/mood-line), a text-only variant of doom-modeline, with two patches: narrowing indicator, displaying region size when it is active.  

This config uses the Consolas font :)  

## Where this runs

I run this configuration in Fedora Silverblue, with just a few extensions installed:

* Clipboard indicator
* Vertical overview (with the Gnome 3 bindings as [suggested here](https://github.com/RensAlthuis/vertical-overview/issues/7#issuecomment-816054137))
* Background logo :P

In addition, I setup Caps Lock as another Control key using dconf. I really like the idea of not layering anything over the base image.  
