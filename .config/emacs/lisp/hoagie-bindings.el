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
(keymap-global-set "<f5>" hoagie-keymap) ;; Enter
(keymap-global-set "<f6>" hoagie-second-keymap) ;; T1 (next to SPC)


(keymap-global-set "<remap> <kill-word>" #'hoagie-delete-word)
(keymap-global-set "<remap> <backward-kill-word>" #'hoagie-backward-delete-word)
(keymap-set hoagie-second-keymap "/" #'hoagie-toggle-backslash)
(keymap-set hoagie-second-keymap "e" #'hoagie-escape-regexp)
(keymap-set hoagie-second-keymap "p" #'hoagie-insert-pair)
(keymap-set hoagie-second-keymap "u" #'hoagie-delete-pair)
(keymap-set hoagie-second-keymap "t" #'hoagie-insert-datetime)
(keymap-set hoagie-second-keymap "s" #'hoagie-split-by-sep)
;; always have a binding for plain old fill-paragraph (it tends
;; to be replaced/shadowed in a lot of modes).
(keymap-set hoagie-second-keymap "q" #'fill-paragraph)
(keymap-set hoagie-second-keymap "d" #'duplicate-dwim)
(keymap-set hoagie-second-keymap "c" #'copy-from-above-command)
;; alt binding - easier on the fingers
(keymap-set hoagie-second-keymap "a" #'copy-from-above-command)
(keymap-set hoagie-second-keymap "k" #'kill-whole-line)
(keymap-set hoagie-second-keymap "m" #'hoagie-flash-mark)

(keymap-global-set "C-c n" hoagie-notes-keymap)

;; I think it is better to have easy access to the listing...
(keymap-set ctl-x-map "C-p" #'hoagie-list-pages)
;; ...but keep mark-page around just in case (I might start using it
;; more going forward
(keymap-set ctl-x-map "M-p" #'mark-page)

;; Trying to make these more memorable than C-x r m/b/l
;; I associate C-x r with registers, not bookmarks - and I don't even
;; use those bindings
(defvar-keymap hoagie-bookmark-keymap
  :name "Bookmarks"
  :doc "Keymap to global bookmark commands"
  "l" '("list"#'bookmark-bmenu-list)
  "j" '("jump to..." #'bookmark-jump)
  "b" '("add" #'bookmark-set))
(keymap-global-set "C-c b" hoagie-bookmark-keymap)

;; suggested in Mastering Emacs: this nicely mirrors M-$ for spellchecking
(keymap-global-set "M-#" #'dictionary-lookup-definition)

;; Mirrors the default binding for dired-jump, C-x C-j
(keymap-set ctl-x-map "j" #'dired-jump-other-window)

;; What are the differences between the last two commands?
;; (info "(emacs) Dired and Find")
(defvar-keymap hoagie-find-keymap
  :doc "Keymap for Dired find commands."
  :name "Find variants"
  "g" '("grep dired" #'find-grep-dired)
  "n" '("name dired" #'find-name-dired)
  "d" '("dired" #'find-dired))
(keymap-set hoagie-keymap "f" hoagie-find-keymap)

(keymap-set hoagie-keymap "e" #'ediff-buffers)
(keymap-set hoagie-keymap "M-e" #'ediff-current-file)

;; "l" for LSP
(keymap-set hoagie-keymap "l r" #'eglot-rename)
(keymap-set hoagie-keymap "l f" #'eglot-format)
(keymap-set hoagie-keymap "l h" #'eldoc)
(keymap-set hoagie-keymap "l a" #'eglot-code-actions)
;; "h" for "help"
(keymap-set hoagie-keymap "h" #'hoagie-toggle-eldoc-buffer)

(defvar-keymap hoagie-eww-keymap
  :doc "Keymap to global eww commands"
  :name "EWW"
  ;; this keymap has additional bindings setup in my work configuration
  "w" '("EWW (search words)" #'eww-search-words)
  ;; mirror "S" in eww-mode-map
  "s" '("buffers" #'eww-list-buffers)
  "b" '("bookmarks"#'eww-list-bookmarks))

(keymap-set eww-mode-map "m" #'hoagie-eww-jump)
(keymap-set eww-mode-map "I" #'eww-toggle-images)

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

(keymap-set hoagie-keymap "g" #'rgrep)

(keymap-set minibuffer-mode-map "C-n" #'minibuffer-next-completion)
(keymap-set minibuffer-mode-map "C-p" #'minibuffer-previous-completion)
;; I want a keybinding to "force" the first candidate possible
;; This is useful for buffer switching and file selection: the default
;; of creating a new one is good, but I want a shorcut to (C-i + RET)
;; in one go, for cases where I know the partial input is good enough
(keymap-set minibuffer-mode-map "C-<return>" #'minibuffer-force-complete-and-exit)

(keymap-set completion-in-region-mode-map  "C-n" #'minibuffer-next-completion)
(keymap-set completion-in-region-mode-map "C-p" #'minibuffer-previous-completion)

(defvar-keymap hoagie-python-repeat-map
  :doc "Keymap to repeat some `python-mode' commands."
  :repeat t
  "a" #'python-nav-backward-block
  "e" #'python-nav-forward-block
  "u" #'python-nav-backward-up-list
  "d" #'down-list)

(defvar-keymap hoagie-register-keymap
  :doc "Keymap for my own register commands."
  :name "Registers"
  "<menu>" '("push-dwim" #'hoagie-push-to-register-dwim)
  "C-z" '("push-dwim1" #'hoagie-push-to-register-dwim)
  "z" '("push-dwim2" #'hoagie-push-to-register-dwim)
  "i" '("insert" #'hoagie-insert-register)
  "l" '("list" #'list-registers)
  "d" '("delete" #'hoagie-clean-registers)
  "j" '("jump" #'hoagie-jump-to-register))

; can NOT be repurposed - Menu key doens't work in mintty
(keymap-global-set "C-z" hoagie-register-keymap)
(keymap-global-set "<menu>" hoagie-register-keymap)

(keymap-set hoagie-keymap "o" #'hoagie-occur-symbol-or-region)
(keymap-set hoagie-keymap "ESC o" #'multi-occur-in-matching-buffers)
;; good mirror of occur -> occur-dwim, except this built in does
;; have a default binding, it is "M-s ."
(keymap-set hoagie-keymap "s" #'isearch-forward-thing-at-point)

;; "ESC n" mirrors "n" (hoagie-kill-buffer-source)
(keymap-set hoagie-keymap "ESC n" #'hoagie-shr-link-open-or-kill)

(keymap-set vc-prefix-map "k" #'vc-revert)
;; shadows `vc-dir'
(keymap-set ctl-x-map "v d" #'vc-dir-root)
(keymap-set vc-dir-mode-map "e" #'vc-ediff)
(keymap-set vc-dir-mode-map "k" #'vc-revert)
(keymap-set vc-dir-mode-map "r" #'stubvex-reset)
(keymap-set vc-dir-mode-map "b b" #'stubvex-list-branches)
;; "l"ist is used for branch-log, use "b"ranches
(keymap-set vc-prefix-map "b b" #'stubvex-list-branches)
(keymap-set vc-prefix-map "e" #'vc-ediff)

(keymap-set ctl-x-map "/" #'vundo)

(keymap-set ctl-x-map "1" #'hoagie-delete-other-windows)
(keymap-set ctl-x-map "|" #'hoagie-toggle-frame-split)
;; experimental: store window setup manually, use case: I'm reading email in
;; Gnus, and want to look at the calendar or world clock or diary
(keymap-set hoagie-keymap "ESC 1" #'hoagie-store-window-configuration)
(keymap-set hoagie-keymap "1" #'hoagie-restore-window-configuration)
(keymap-set hoagie-keymap "|" #'hoagie-toggle-frame-split)


(defvar-keymap hoagie-goto-keymap
  :doc "Keymap to go to \"places\": home dir, init file, etc.
It has additional bindings setup in each local configuration."
  :name "Go to"
  "h" '("home directory" #'hoagie-go-home)
  "i" '("init file" #'hoagie-open-init))
(keymap-global-set "C-c g" hoagie-goto-keymap)

;; from https://emacsredux.com/blog/2020/06/10/comment-commands-redux/
(keymap-global-set "<remap> <comment-dwim>" #'comment-line)
; replace delete-char, as recommended in the docs
(keymap-global-set "<remap> <delete-char>" #'delete-forward-char)
(keymap-global-set "<remap> <capitalize-word>" #'capitalize-dwim)
(keymap-global-set "<remap> <upcase-word>" #'upcase-dwim)
(keymap-global-set "<remap> <downcase-word>" #'downcase-dwim)
(keymap-global-set "<remap> <zap-to-char>" #'zap-up-to-char)
(keymap-global-set "<remap> <list-buffers>" #'ibuffer)
(keymap-global-set "C-x k" #'kill-current-buffer)
(keymap-global-set "C-x M-k" #'kill-buffer)

;; right next to other-window
(keymap-set ctl-x-map "i" #'other-frame)
;; add meta to get the original command for C-x i...
;; ...although I never used it#'UPDATE: used it a couple times :)
(keymap-set ctl-x-map "ESC i" #'insert-file)
;; combines https://stackoverflow.com/a/6465415 with
;; https://www.reddit.com/r/emacs/comments/1juhasp/comment/mm2m4ne/
;; by using prefix-arg UPDATE: improves on the SO answer :D
(keymap-set ctl-x-map "3" #'#'hoagie-split-window-right)
(keymap-set ctl-x-map "2" #'#'hoagie-split-window-below)
(keymap-set narrow-map "i" #'hoagie-narrow-indirect-dwim)

(keymap-set hoagie-keymap "n" #'hoagie-kill-buffer-source)

(defvar-keymap hoagie-other-window-frame-repeat-map
  :doc "Keymap to repeat window and frame commands."
  :repeat t
  "o" #'other-window
  "i" #'other-frame
  "0" #'delete-window)

(defvar-keymap hoagie-other-window-frame-repeat-map
  :doc "Keymap to extend selections after a mark command"
  :repeat t
  "SPC" #'mark-sexp
  "@" #'mark-word)

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

;;; hoagie-bindings.el ends here
