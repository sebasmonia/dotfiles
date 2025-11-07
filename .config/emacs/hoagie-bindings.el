;;; hoagie-bindings.el --- All key bindings in one place -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Sebastián Monía
;;
;; Author: Sebastián Monía <sebastian@sebasmonia.com>
;; URL: https://git.sr.ht/~sebasmonia/dotfiles
;; Keywords: local convenience

;; This file is not part of GNU Emacs.

;;; Commentary:

;; I have the theory that defining all bindings in a single place will help me
;; keep them more consistent.
;; Time will tell...

;;; Code:

(defvar-keymap hoagie-keymap
  :doc "Keybindings for general, non-editing features."
  :prefix 'hoagie-keymap)

(defvar-keymap hoagie-second-keymap
  :doc "Keybindings that deal directly with text manipulation."
  :prefix 'hoagie-second-keymap)

;; compat Linux-Windows
(keymap-set key-translation-map "<apps>" "<menu>")
(keymap-set key-translation-map "<print>" "<menu>") ;; Thinkpad's PrintScr
;; TODO: what can I use the menu key for?
;; (keymap-global-set "<menu>" 'hoagie-keymap)

;; these keys are mapped on particular positions in my Dygma Raise
(keymap-global-set "<f5>" 'hoagie-keymap) ;; Enter
(keymap-global-set "<f6>" 'hoagie-second-keymap) ;; T1 (next to SPC)


;;; hoagie-bindings.el ends here
