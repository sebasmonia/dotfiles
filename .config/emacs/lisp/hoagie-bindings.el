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


(keymap-global-set "<remap> <kill-word>" #'hoagie-delete-word)
(keymap-global-set "<remap> <backward-kill-word>" #'hoagie-backward-delete-word)
(keymap-set 'hoagie-second-keymap "/" #'hoagie-toggle-backslash)
(keymap-set 'hoagie-second-keymap "e" #'hoagie-escape-regexp)
(keymap-set 'hoagie-second-keymap "p" #'hoagie-insert-pair)
(keymap-set 'hoagie-second-keymap "u" #'hoagie-delete-pair)
(keymap-set 'hoagie-second-keymap "t" #'hoagie-insert-datetime)
(keymap-set 'hoagie-second-keymap "s" #'hoagie-split-by-sep)
;; always have a binding for plain old fill-paragraph (it tends
;; to be replaced/shadowed in a lot of modes).
(keymap-set 'hoagie-second-keymap "q" #'fill-paragraph)
(keymap-set 'hoagie-second-keymap "d" #'duplicate-dwim)
(keymap-set 'hoagie-second-keymap "c" #'copy-from-above-command)
;; alt binding - easier on the fingers
(keymap-set 'hoagie-second-keymap "a" #'copy-from-above-command)
(keymap-set 'hoagie-second-keymap "k" #'kill-whole-line)
(keymap-set 'hoagie-second-keymap "m" #'hoagie-flash-mark)

(keymap-global-set "C-c n" hoagie-notes-keymap)

;; I think it is better to have easy access to the listing...
(keymap-set 'ctl-x-map "C-p" #'hoagie-list-pages)
;; ...but keep mark-page around just in case (I might start using it
;; more going forward
(keymap-set 'ctl-x-map "M-p" #'mark-page)

;; Trying to make these more memorable than C-x r m/b/l
;; I associate C-x r with registers, not bookmarks - and I don't even
;; use those bindings
(defvar-keymap hoagie-bookmark-keymap
  :name "Bookmarks"
  :doc "Keymap to global bookmark commands"
  "l" '("list". bookmark-bmenu-list)
  "j" '("jump to..." . bookmark-jump)
  "b" '("add" . bookmark-set))
(keymap-global-set "C-c b" hoagie-bookmark-keymap)

;; suggested in Mastering Emacs: this nicely mirrors M-$ for spellchecking
(keymap-global-set "M-#" #'dictionary-lookup-definition)

;; Mirrors the default binding for dired-jump, C-x C-j
(keymap-set 'ctl-x-map "j" #'dired-jump-other-window)

;; What are the differences between the last two commands?
;; (info "(emacs) Dired and Find")
(defvar-keymap hoagie-find-keymap
  :doc "Keymap for Dired find commands."
  :name "Find variants"
  "g" '("grep dired" . find-grep-dired)
  "n" '("name dired" . find-name-dired)
  "d" '("dired" . find-dired))
(keymap-set hoagie-keymap "f" hoagie-find-keymap)

(keymap-set hoagie-keymap "e" #'ediff-buffers)
(keymap-set hoagie-keymap "M-e" #'ediff-current-file)


;;; hoagie-bindings.el ends here
