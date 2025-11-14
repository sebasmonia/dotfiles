;;; bindings.el --- All key bindings in one place -*- lexical-binding: t; -*-

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

;; compat Linux-Windows
(keymap-set key-translation-map "<apps>" "<menu>")
(keymap-set key-translation-map "<print>" "<menu>") ;; Thinkpad's PrintScr

;; TODO: what can I use the menu key for? Should be something I only use in
;; GUI Emacs, or in my home computer
;; (keymap-global-set "<menu>" 'hoagie-shortcut-keymap)


;; Custom prefix keymaps: editing and shortcuts
(defvar-keymap hoagie-editing-keymap
  :doc "Keybindings that deal directly with text manipulation."
  :prefix 'hoagie-editing-keymap
  "/" #'hoagie-toggle-backslash
  "e" #'hoagie-escape-regexp
  "p" #'hoagie-insert-pair
  "u" #'hoagie-delete-pair
  "t" #'hoagie-insert-datetime
  "s" #'hoagie-split-by-sep
  ;; always have a binding for plain old fill-paragraph (it tends
  ;; to be replaced/shadowed in a lot of modes)
  "q" #'fill-paragraph
  "d" #'duplicate-dwim
  "c" #'copy-from-above-command
  ;; alt binding - easier on the fingers
  "a" #'copy-from-above-command
  "k" #'kill-whole-line
  "m" #'hoagie-flash-mark)
(keymap-global-set "<f6>" hoagie-editing-keymap) ;; T1 (next to SPC)

(defvar-keymap hoagie-shortcut-keymap
  :doc "Keybindings for general, non-editing features."
  :prefix 'hoagie-shortcut-keymap
  "e" #'ediff-buffers
  "M-e" #'ediff-current-file
  ;; "h" for "help"
  "h" #'hoagie-toggle-eldoc-buffer
  "g" #'rgrep
  "o" #'hoagie-occur-symbol-or-region
  "ESC o" #'multi-occur-in-matching-buffers
  ;; good mirror of occur -> occur-dwim, except this built in does
  ;; have a default binding, it is "M-s ."
  "s" #'isearch-forward-thing-at-point

  "n" #'hoagie-kill-buffer-source
  "ESC n" #'hoagie-shr-link-open-or-kill
  ;; Experimental: store window setup manually.
  ;; Use case: I'm reading email in Gnus, and want to look at calendar or world
  ;; clock or diary
  "ESC 1" #'hoagie-store-window-configuration
  "1" #'hoagie-restore-window-configuration
  "|" #'hoagie-toggle-frame-split)
(keymap-global-set "<f5>" hoagie-shortcut-keymap) ;; Enter

;; Other custom keymaps
;; Exception repeat keymaps, at the bottom of the file.

;; What are the differences between the last two commands?
;; (info "(emacs) Dired and Find")
(defvar-keymap hoagie-find-keymap
  :doc "Keymap for Dired find commands."
  :name "Find variants"
  "g" '("grep dired" . find-grep-dired)
  "n" '("name dired"  . find-name-dired)
  "d" '("dired"  . find-dired))
(keymap-set hoagie-shortcut-keymap "f" hoagie-find-keymap)

(defvar-keymap hoagie-eglot-keymap
  :doc "Keymap for Eglot commands."
  :name "Eglot"
  "r" '("rename" . eglot-rename)
  "h" '("help"  . eldoc)
  "c" '("code actions" . eglot-code-actions))
;; "l" for LSP
(keymap-set hoagie-shortcut-keymap "l" hoagie-eglot-keymap)

(keymap-global-set "C-c n" hoagie-notes-keymap)

;; Trying to make these more memorable than C-x r m/b/l
;; I associate C-x r with registers, not bookmarks - and I don't even
;; use those bindings
(defvar-keymap hoagie-bookmark-keymap
  :name "Bookmarks"
  :doc "Keymap to global bookmark commands"
  "l" '("list" . bookmark-bmenu-list)
  "j" '("jump to..." . bookmark-jump)
  "b" '("add" . bookmark-set))
(keymap-global-set "C-c b" hoagie-bookmark-keymap)

(defvar-keymap hoagie-eww-keymap
  :doc "Keymap to global eww commands"
  :name "EWW"
  ;; this keymap has additional bindings setup in my work configuration
  "w" '("EWW (search words)" . eww-search-words)
  ;; mirror "S" in eww-mode-map
  "s" '("buffers" . eww-list-buffers)
  "b" '("bookmarks" . eww-list-bookmarks))
(keymap-global-set "C-c w" hoagie-eww-keymap)

(defvar-keymap hoagie-flymake-keymap
  :doc "Custom bindings for `flymake-mode'."
  :prefix 'hoagie-flymake-keymap
  ;; flymake command alternative bindings
  "l" #'flymake-show-buffer-diagnostics
  "n" #'flymake-goto-next-error
  "p" #'flymake-goto-prev-error
  ;; "t" for toggle
  "t" #'flymake-mode
  ;; whichever I use the most between t and f will stay long term
  "f" #'flymake-mode
  "s" #'flymake-start)
(keymap-global-set "C-c f" hoagie-flymake-keymap)

(defvar-keymap hoagie-register-keymap
  :doc "Keymap for my own register commands."
  :name "Registers"
  "<menu>" '("push-dwim" . hoagie-push-to-register-dwim)
  "C-z" '("push-dwim1" . hoagie-push-to-register-dwim)
  "z" '("push-dwim2" . hoagie-push-to-register-dwim)
  "i" '("insert" . hoagie-insert-register)
  "l" '("list" . list-registers)
  "d" '("delete" . hoagie-clean-registers)
  "j" '("jump" . hoagie-jump-to-register))
; can NOT be repurposed - Menu key doens't work in mintty
(keymap-global-set "C-z" hoagie-register-keymap)
(keymap-global-set "<menu>" hoagie-register-keymap)

(defvar-keymap hoagie-goto-keymap
  :doc "Keymap to go to \"places\": home dir, init file, etc.
It has additional bindings setup in each local configuration."
  :name "Go to"
  "h" '("home directory" . hoagie-go-home)
  "i" '("init file" . hoagie-open-init)
  "m" '("machine config" . hoagie-open-machine))
(keymap-global-set "C-c g" hoagie-goto-keymap)

;; Mode-specific bindings
(keymap-set eww-mode-map "m" #'hoagie-eww-jump)
(keymap-set eww-mode-map "I" #'eww-toggle-images)
(keymap-set narrow-map "i" #'hoagie-narrow-indirect-dwim)

;; Minibuffer (in my config, it is also completion)
(keymap-set minibuffer-mode-map "C-n" #'minibuffer-next-completion)
(keymap-set minibuffer-mode-map "C-p" #'minibuffer-previous-completion)
;; I want a keybinding to "force" the first candidate possible
;; This is useful for buffer switching and file selection: the default
;; of creating a new one is good, but I want a shorcut to (C-i + RET)
;; in one go, for cases where I know the partial input is good enough
(keymap-set minibuffer-mode-map "C-<return>" #'minibuffer-force-complete-and-exit)
(keymap-set completion-in-region-mode-map  "C-n" #'minibuffer-next-completion)
(keymap-set completion-in-region-mode-map "C-p" #'minibuffer-previous-completion)

;; vc-mode and friends
(keymap-set vc-prefix-map "k" #'vc-revert)
;; shadows `vc-dir' (and could be in the ctl-x-map section too)
(keymap-set ctl-x-map "v d" #'vc-dir-root)
(keymap-set vc-dir-mode-map "e" #'vc-ediff)
(keymap-set vc-dir-mode-map "k" #'vc-revert)
(keymap-set vc-dir-mode-map "r" #'stubvex-reset)
(keymap-set vc-dir-mode-map "b b" #'stubvex-list-branches)
;; "l"ist is used for branch-log, use "b"ranches
(keymap-set vc-prefix-map "b b" #'stubvex-list-branches)
(keymap-set vc-prefix-map "e" #'vc-ediff)

;; Remaps
(keymap-global-set "<remap> <capitalize-word>" #'capitalize-dwim)
(keymap-global-set "<remap> <upcase-word>" #'upcase-dwim)
(keymap-global-set "<remap> <downcase-word>" #'downcase-dwim)
(keymap-global-set "<remap> <zap-to-char>" #'zap-up-to-char)
(keymap-global-set "<remap> <list-buffers>" #'ibuffer)
(keymap-global-set "<remap> <kill-word>" #'hoagie-delete-word)
(keymap-global-set "<remap> <backward-kill-word>" #'hoagie-backward-delete-word)
;; from https://emacsredux.com/blog/2020/06/10/comment-commands-redux/
(keymap-global-set "<remap> <comment-dwim>" #'comment-line)
; replace delete-char, as recommended in the docs
(keymap-global-set "<remap> <delete-char>" #'delete-forward-char)
;; THIS ISN'T A REMAP...but I don't have another section to put it in...
;; Suggested in Mastering Emacs: it nicely mirrors M-$ for spellchecking
(keymap-global-set "M-#" #'dictionary-lookup-definition)

;; ctl-x-map
(keymap-set ctl-x-map "k" #'kill-current-buffer)
(keymap-set ctl-x-map "M-k" #'kill-buffer)
;; Mirrors the default binding for dired-jump, C-x C-j
(keymap-set ctl-x-map "j" #'dired-jump-other-window)
(keymap-set ctl-x-map "/" #'vundo)

(keymap-set ctl-x-map "C-p" #'hoagie-list-pages) ;; Shadows mark-page...
(keymap-set ctl-x-map "M-p" #'mark-page)         ;; ...so rebind it

(keymap-set ctl-x-map "i" #'other-frame) ;; right next to other-window
;; add meta to get the original command for C-x i...
;; ...although I never used it. UPDATE: used it a couple times :)
(keymap-set ctl-x-map "ESC i" #'insert-file)

(keymap-set ctl-x-map "|" #'hoagie-toggle-frame-split)
(keymap-set ctl-x-map "1" #'hoagie-delete-other-windows)
;; combines https://stackoverflow.com/a/6465415 with
;; https://www.reddit.com/r/emacs/comments/1juhasp/comment/mm2m4ne/
;; by using prefix-arg UPDATE: improves on the SO answer :D
(keymap-set ctl-x-map "2" #'hoagie-split-window-below)
(keymap-set ctl-x-map "3" #'hoagie-split-window-right)

;; Repeat keymaps
(defvar-keymap hoagie-python-repeat-map
  :doc "Keymap to repeat some `python-mode' commands."
  :repeat t
  "a" #'python-nav-backward-block
  "e" #'python-nav-forward-block
  "u" #'python-nav-backward-up-list
  "d" #'down-list)

(defvar-keymap hoagie-other-window-frame-repeat-map
  :doc "Keymap to repeat window and frame commands."
  :repeat t
  "o" #'other-window
  "i" #'other-frame
  "0" #'delete-window)

(defvar-keymap hoagie-mark-repeat-map
  :doc "Keymap to extend selections after a mark command"
  :repeat t
  "SPC" #'mark-sexp
  "@" #'mark-word
  "h" #'mark-paragraph)

(defvar-keymap hoagie-cycle-repeat-map
    :doc "Keymap to use SPC to repeat cycle-spacing."
    :repeat t
    "SPC" #'cycle-spacing)

(defvar-keymap hoagie-sexp-movement-repeat-map
  :doc "Keymap to repeat a few \"C-M-something\" movement commands."
  :repeat t
  "u" #'backward-up-list
  "d" #'down-list
  "f" #'forward-sexp
  "b" #'backward-sexp
  "a" #'beginning-of-defun
  "e" #'end-of-defun)

(defvar-keymap hoagie-undo-repeat-map
  :doc "Keymap to repeat undo/redo commands."
  :repeat (:exit (vundo))
  "/" #'undo
  "u" #'undo
  "r" #'undo-redo
  "v" #'vundo)

;;; bindings.el ends here
