(tool-bar-mode 0)
(menu-bar-mode 0)
(scroll-bar-mode 0)
(tooltip-mode 0)

;; Ignore .Xresources
(advice-add #'x-apply-session-resources :override #'ignore)
(setq frame-title-format "%b - Emacs"
      icon-title-format "%b - Emacs"
      default-frame-alist '((fullscreen . maximized) (vertical-scroll-bars . nil) (horizontal-scroll-bars . nil)))
