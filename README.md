A place to store my Emacs configuration. I used to have more things here, but I simplified a lot of my setup.

## Emacs config

A [use-package](https://github.com/jwiegley/use-package) based configuration, that tries its best to be platform agnostic, even though nowadays I use Linux everywhere.  
In most cases I prefer smaller packages, if built-in, even better (eg icomplete over ivy/helm, project.el over projectile, etc.).  
After some work, I managed to get dap-mode working for C#, and then...I never used it. That was my #1 reason to try lsp-mode over [Eglot](https://github.com/joaotavora/eglot/), so I have gone back to the latter. That said, I think most people should try lsp-mode first, it works really well.  
My theme of choice is modus-operandi. Every once in a while I try other themes, but at most a day later I end up going back to modus.  
I got used to [Sly](https://github.com/joaotavora/sly) for Common Lisp.  
My mode line of choice is [mood-line](https://gitlab.com/jessieh/mood-line), a text-only variant of doom-modeline, with two patches: narrowing indicator, displaying region size when it is active.  

## Where this runs

I run this configuration in Fedora Silverblue, with just a few extensions installed:

* Clipboard indicator
* Vertical overview (with the Gnome 3 bindings as [suggested here](https://github.com/RensAlthuis/vertical-overview/issues/7#issuecomment-816054137))
* Background logo :P

In addition, I setup Caps Lock as another Control key using dconf. I really like the idea of not layering anything the base image.  
