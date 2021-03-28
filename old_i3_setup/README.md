A place to store my Emacs configuration, plus other misc scripts.

## Emacs config notes:

A use-package based configuration, that tries its best to be platform agnostic. I moved the Windows-only things to a 
private repo @ work, since they only make sense in that environment.

Some high level notes:

* In most cases I prefer smaller packages, if built-in, even better (eg icomplete over ivy/helm)
* LSP (but sometimes Eglot) used for C# and Python
* Sly for Common Lisp
* Deadgrep (rg) for non-LSP code search
* Modus-operandi theme
* Doom modeline

