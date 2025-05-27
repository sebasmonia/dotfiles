;;; early-init.el --- The name is self-explanatory -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Sebastián Monía
;;
;; Author: Sebastián Monía <sebastian@sebasmonia.com>
;; URL: https://git.sr.ht/~sebasmonia/dotfiles
;; Keywords: convenience

;; This file is not part of GNU Emacs.

;;; Commentary:

;; I am closer to removing this file, than to add more things in it.

;;; Code:

(tool-bar-mode 0)
(menu-bar-mode 0)
(scroll-bar-mode 0)
(tooltip-mode 0)

(setq default-frame-alist '((fullscreen . maximized)
                            (vertical-scroll-bars . nil)
                            (horizontal-scroll-bars . nil)))

;;; early-init.el ends here
