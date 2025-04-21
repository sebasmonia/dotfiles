;;; .emacs --- My dot emacs file  -*- lexical-binding: t; -*-

;; Author: Sebastián Monía <code@sebasmonia.com>
;; URL: https://git.sr.ht/~sebasmonia/dotfiles
;; Version: 30.5
;; Keywords: tools maint

;; This file is not part of GNU Emacs.

;;; Commentary:

;; My dot Emacs file
;; Recent changes:
;; 2024-06-26: Remove howm - I only used like, 3 features from it.
;; 2024-07-10: Going back to Gnus with IMAP back end, sending elfeed to
;;             purgatory, and putting note comments in a separate package.
;; 2024-08-29: Move mode-line setup to separate file.
;; 2025-02-24: Update for Emacs 30 (new options, remove things now in core)
;; 2025-02-26: Bring back browse-kill-ring, remove json-mode, dired-narrow,
;;             re-builder. Remove instances of global-set-key.
;; 2025-03-10: Change F2, and add repeat-mode.
;;; Code:

(setf custom-file (locate-user-emacs-file "custom.el"))

(when (eq window-system 'pgtk)
  (pgtk-use-im-context t))

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;; (setf use-package-compute-statistics t)
(setf package-archive-priorities '(("gnu" . 10) ("nongnu" . 5) ("melpa" . 1))
      package-native-compile t)

(require 'use-package)
(setf use-package-verbose t
      use-package-hook-name-suffix nil)

(when (display-graphic-p)
  (custom-set-faces '(default ((t (:family "Berkeley Mono"
                                           :height 120
                                           :foundry "outline"))))))

;; Based on http://www.ergoemacs.org/emacs/emacs_menu_app_keys.html I
;; eventually I moved away from the menu key to F6, and even later that key
;; ended in a very similar place to <menu> but on the other side of the
;; keyboard (Dygma Raise)
(defvar-keymap hoagie-keymap
  :doc "One of my two sets of custom keybindings."
  :prefix 'hoagie-keymap)
(defvar-keymap hoagie-second-keymap
  :doc "The other of my two sets of custom keybindings."
  :prefix 'hoagie-second-keymap)

;; compat Linux-Windows
(keymap-set key-translation-map "<apps>" "<menu>")
(keymap-set key-translation-map "<print>" "<menu>") ;; Thinkpad's PrintScr
;; TODO: can try using something else here, even in the laptop
;; I use F6 and not the menu key!
(keymap-global-set "<menu>" 'hoagie-keymap)

;; these keys are mapped on particular positions in my Dygma Raise
(keymap-global-set "<f6>" 'hoagie-keymap) ;; T1 (next to SPC)
(keymap-global-set "<f5>" 'hoagie-second-keymap) ;; Enter

;; This package declares commands and macros (!) that I
;; use for general editing
(use-package hoagie-editing
  :load-path "~/sourcehut/dotfiles/.config/emacs"
  :demand t
  :bind
  ("C-z" . hoagie-region-to-char)
  ("<remap> <kill-word>" . hoagie-delete-word)
  ("<remap> <backward-kill-word>" . hoagie-backward-delete-word)
  (:map hoagie-keymap
        ("/" . hoagie-toggle-backslash)
        ("p" . hoagie-insert-pair)
        ("u" . hoagie-delete-pair)
        ("q" . hoagie-escape-regexp)
        ("t" . hoagie-insert-datetime))
  (:map hoagie-second-keymap
        ("s" . hoagie-split-by-sep)
        ;; always have a binding for plain old fill-paragraph (it tends
        ;; to be replaced/shadowed in a lot of modes).
        ("q" . fill-paragraph)))

(use-package hoagie-notes
  :load-path "~/sourcehut/dotfiles/.config/emacs"
  :demand t
  :bind
  ("<f2>" . hoagie-notes-keymap))

(use-package ansi-color
  :commands (ansi-color-apply-buffer)
  :init
  (defun ansi-color-apply-buffer ()
    "Colorize the entire buffer using `ansi-color-apply-on-region'."
    (interactive)
    (ansi-color-apply-on-region (point-min) (point-max))))

;; inspired by https://gitlab.com/jabranham/emacs/blob/master/init.el
(use-package appt
  :defer 2 ;; NEW! for me at least. Delay # seconds
  :custom
  (appt-delete-window-function (lambda () t))
  (appt-disp-window-function #'hoagie-appt-notify)
  (appt-audible nil)
  (appt-display-diary nil)
  (appt-display-interval 5)
  (appt-message-warning-time 15)
  :config
  ;; override the built in function, as there is not way to customize this
  (defun appt-mode-line (min-to-app &optional _abbrev)
    "Return an appointment string for the mode-line. Hoagie version.
    MIN-TO-APP is a list of minutes, as strings. _ABBREV is ignored, always
    abbreviate text."
    (let ((smaller-min (car (sort min-to-app #'<))))
      (format "Appt(s): %s"
              (if (equal smaller-min "0") "NOW"
                (format "%sm" smaller-min)))))
  (defun hoagie-appt-notify (minutes-until _current-time appointment-text)
    "Show appointment reminders in the desktop.
    See the documentation of appt.el for details on MINUTES-UNTIL, _CURRENT-TIME
    and APPOINTMENT-TEXT."
    ;; args can be lists if multiple appointments are due at the same time
    (let ((notification-body (if (listp minutes-until)
                                 (format "Multiple appts in %s minutes!!!"
                                         (car minutes-until))
                               (format "In %s minutes: %s"
                                       minutes-until
                                       appointment-text))))
      (notifications-notify :title "Emacs - Appointment"
                            :body notification-body
                            :app-name "Emacs"
                            ;; never expire
                            :timeout 0
                            ;; use 'low to add a notification without toast
                            :urgency 'normal)))
    (appt-activate))

(use-package auth-source
  :custom
  ;; change order so TRAMP defaults to saving passwords in
  ;; the encrypted gpg file
  (auth-sources '("~/.authinfo.gpg" "~/.authinfo" "~/.netrc")))

(use-package bookmark
  :bind
  ;; trying to make these more memorable than C-x r m/b/l
  ;; I associate C-x r with registers, not bookmarks
  (:map hoagie-second-keymap
        ("b b" . bookmark-set)
        ("b j" . bookmark-jump)
        ("b l" . bookmark-bmenu-list)))

;; I can use built in M-y, which offers completion. But for longer text, it
;; isn't nearly as comfortable to use as this package
(use-package browse-kill-ring
  :ensure t
  :demand t
  :config
  (browse-kill-ring-default-keybindings))

(use-package browse-url
  :custom
  (browse-url-secondary-browser-function #'browse-url-firefox)
  (browse-url-browser-function #'eww-browse-url)
  (browse-url-new-window-flag t))

(use-package calendar
  :demand t
  :custom
  (calendar-date-style 'iso)
  (calendar-view-diary-initially-flag nil)
  (calendar-latitude 40.7)
  (calendar-longitude -73.9)
  (calendar-location-name "New York, NY")
  (calendar-setup 'one-frame)
  ;; show events for the next 3 days when the calendar opens
  (diary-number-of-entries 3)
  :hook
  (calendar-today-visible-hook . calendar-mark-today)
  (calendar-mode-hook . diary-mark-entries))

(use-package cambalache
  :load-path "~/sourcehut/cambalache"
  :custom
  (cambalache-root-url "https://myfiles.fastmail.com/")
  :commands
  (cambalache-list-contents cambalache-download-file cambalache-upload-file))

(use-package comint
  :custom
  (comint-prompt-read-only t)
  (comint-scroll-to-bottom-on-input 'this))

(use-package dabbrev
  :custom
  (dabbrev-case-distinction nil)
  (dabbrev-case-fold-search t)
  (dabbrev-case-replace nil))

(use-package diary-lib
  :demand t
  :custom
  (diary-display-function 'diary-fancy-display)
  :hook
  (diary-mark-entries-hook . diary-mark-included-diary-files)
  ;; `diary-sort-entries' should be added last, curiously for
  ;; use-package to do that I have to add it _first_
  (diary-list-entries-hook . diary-sort-entries)
  (diary-list-entries-hook . diary-include-other-diary-files))

(use-package dictionary
  :custom
  ;; unless I run a dict server locally - which I am tempted to, but
  ;; it is definitely overkill
  (dictionary-server "dict.org")
  :commands
  (dictionary-search dictionary-lookup-definition)
  :bind
  ;; suggested in Mastering Emacs: this nicely mirrors M-$ for spellchecking
  ("M-#" . dictionary-lookup-definition))

(use-package cdsync :load-path "~/sourcehut/caldav-sync.el"
  :if (and (eq system-type 'gnu/linux)
           (locate-library "cdsync.el"))
  :demand t
  :custom
  (cdsync-auth-source-host "caldav-fastmail")
  :commands
  (cdsync-open-diary cdsync-track-calendar cdsync-list-calendars)
  :config
  (cdsync-setup-calendar-integration))

(use-package dired
  :demand t
  :custom
  (dired-vc-rename-file t)
  (dired-listing-switches "-labogGhvD")
  (dired-compress-directory-default-suffix ".7z")
  (dired-compress-file-default-suffix ".7z")
  (dired-do-revert-buffer t)
  (dired-movement-style 'cycle)
  :bind
  ;; The default binding for dired-jump is C-x C-j. I used to use
  ;; F6-j for dired-jump-other-window.
  ;; Adding C-x j to jump to other window, freeing j on my personal keymap.
  (:map ctl-x-map
        ("j" . dired-jump-other-window))
  (:map hoagie-keymap
        ;; see definition for F6-f in :config below
        ("n" . hoagie-kill-buffer-filename))
  :hook
  (dired-mode-hook . dired-hide-details-mode)
  ;; Enables "C-c C-m a" (yeah, really :) lol) to attach
  ;; all files marked in dired to the current/a new email
  (dired-mode-hook . turn-on-gnus-dired-mode)
  :config
  ;; What are the differences between the last two commands?
  ;; (info "(emacs) Dired and Find")
  (defvar-keymap hoagie-find-keymap
    :doc "Keymap for Dired find commands."
    :name "Find..."
    "g" '("grep dired" . find-grep-dired)
    "n" '("name dired" . find-name-dired)
    "d" '("dired" . find-dired))
  ;; UPDATE 2024-11-04: I saw this technique in "M-o" for sgml-mode, which in
  ;; turn uses facemenu.el, but it only works correctly if I assign the binding
  ;; "manually" instead through use-package
  (keymap-set hoagie-keymap "ESC f" hoagie-find-keymap)
  (setf dired-compress-file-suffixes
        '(("\\.tar\\.gz\\'" #1="" "7z x -aoa -o%o %i")
          ("\\.tgz\\'" #1# "7z x -aoa -o%o %i")
          ("\\.zip\\'" #1# "7z x -aoa -o%o %i")
          ("\\.7z\\'" #1# "7z x -aoa -o%o %i")
          ("\\.tar\\'" ".tgz" nil)
          (":" ".tar.gz" "tar -cf- %i | gzip -c9 > %o"))
        dired-compress-files-alist
        '(("\\.7z\\'" . "7z a -r %o %i")
          ("\\.zip\\'" . "7z a -r %o  %i")))
  (defun hoagie-kill-buffer-filename ()
    "Sends the current buffer's filename to the kill ring."
    (interactive)
    (if (derived-mode-p 'dired-mode)
        (dired-copy-filename-as-kill 0)
      (if-let* ((name (buffer-file-name)))
        (progn
          (kill-new name)
          (message "Killed filename: %s" name))
        (error "No file for this buffer")))))

(use-package ediff
  :bind
  (:map hoagie-keymap
        ("e" . ediff-buffers)
        ("ESC e" . ediff-current-file))
  :custom
  ;; from https://emacs.stackexchange.com/a/9411/17066
  (ediff-forward-word-function 'forward-char)
  (ediff-highlight-all-diffs t)
  (ediff-keep-variants nil)
  (ediff-window-setup-function 'ediff-setup-windows-plain)
  :hook
  (ediff-keymap-setup-hook . add-d-to-ediff-mode-map)
  (ediff-before-setup-hook . hoagie-ediff-store-windows)
  ;; Welp, don't like using internals but, the regular hook doesn't quite work
  ;; the window config is restored but then _stuff happens_, so:
  (ediff-after-quit-hook-internal . hoagie-ediff-restore-windows)
  :config
  ;; from https://stackoverflow.com/a/29757750
  (defun ediff-copy-both-to-C ()
    "In ediff, copy A and then B to C."
    (interactive)
    (ediff-copy-diff ediff-current-difference nil 'C nil
                     (concat
                      (ediff-get-region-contents ediff-current-difference
                                                 'A
                                                 ediff-control-buffer)
                      (ediff-get-region-contents ediff-current-difference
                                                 'B
                                                 ediff-control-buffer))))
  (defun add-d-to-ediff-mode-map ()
    "Add key 'd' for 'copy both to C' functionality in ediff."
    (keymap-set ediff-mode-map "d" #'ediff-copy-both-to-C))
  ;; One minor annoyance of using ediff with built-in vc was
  ;; the window config being altered, so:
  (defvar hoagie-pre-ediff-windows
    nil
    "Window configuration before starting ediff.")
  (defun hoagie-ediff-store-windows ()
    "Store the pre-ediff window setup"
    (setf hoagie-pre-ediff-windows (current-window-configuration)))
  (defun hoagie-ediff-restore-windows ()
    "Use `hoagie-pre-ediff-windows' to restore the window setup."
    (set-window-configuration hoagie-pre-ediff-windows)))

(use-package eglot
  :commands (eglot)
  :custom
  (eglot-events-buffer-config '(:size 0 :format full))
  (eglot-ignored-server-capabilities '(:codeLensProvider
                                       :documentHighlightProvider
                                       :documentFormattingProvider
                                       :documentRangeFormattingProvider
                                       :documentOnTypeFormattingProvider
                                       :foldingRangeProvider))
  :bind
  (:map hoagie-keymap
        ;; "l" for LSP
        (("l r" . eglot-rename)
         ("l f" . eglot-format)
         ("l h" . eldoc)
         ("l a" . eglot-code-actions)))
  (:map eglot-mode-map
        (("C-c C-e r" . eglot-rename)
         ("C-c C-e f" . eglot-format)
         ("C-c C-e h" . eldoc)
         ("C-c C-e a" . eglot-code-actions)))
  :config
  ;; from https://dawranliou.com/blog/xref-with-eglot-and-project/
  (defun xref-find-references-with-eglot (orig-fun &rest args)
    "An advice function that gives `xref-find-definitions' a unique
buffer name when eglot is enabled."
    (if (bound-and-true-p eglot--managed-mode)
        (let ((xref-buffer-name (format "*xref %s*"
                                        (symbol-at-point))))
          (apply orig-fun args))
      (apply orig-fun args)))
  (advice-add 'xref-find-references :around
              #'xref-find-references-with-eglot))

(use-package eldoc
  :demand t
  :custom
  (eldoc-echo-area-use-multiline-p nil)
  :bind
  (:map hoagie-second-keymap
        ;; "h" for "help"
        ("h" . hoagie-toggle-eldoc-buffer))
  :config
  (global-eldoc-mode)
  (defun hoagie-toggle-eldoc-buffer ()
    "Toggle the eldoc help buffer."
    (interactive)
    (let ((eldoc-window (get-buffer-window eldoc--doc-buffer)))
      (if eldoc-window
          (quit-window nil eldoc-window)
        (eldoc-doc-buffer t)))))

(use-package elisp-mode
  :hook
  (emacs-lisp-mode-hook . hoagie-elisp-mode-setup)
  :config
  (defun hoagie-elisp-mode-setup ()
    "Setup my `emacs-lisp-mode' configuration.
Sets `fill-column'."
    (setf fill-column 79)
    (display-fill-column-indicator-mode)))

(use-package epg-config
  :custom
  (epg-pinentry-mode 'loopback))

(use-package eww
  :demand t
  :custom
  (eww-auto-rename-buffer #'hoagie-eww-rename-buffer)
  (eww-download-directory  "~/eww-downloads/")
  (eww-bookmarks-directory "~/sourcehut/dotfiles/.config/emacs/")
  :hook
  (eww-mode-hook . toggle-word-wrap)
  (eww-mode-hook . visual-line-mode)
  :bind
  (:map hoagie-second-keymap
        ;; "w" for web.
        ("w" . eww)
        ;; the EWW boormarks are the "personal" ones
        ("ESC w" . eww-list-bookmarks)
        ("C-w" . eww-list-buffers))
  (:map eww-mode-map
        ("m" . hoagie-eww-jump)
        ;; default M-I - but I use this often
        ;; using uppercase to mirror F to toggle fonts
        ("I" . eww-toggle-images))
  :config
  (defun hoagie-eww-rename-buffer ()
    "Rename EWW buffers like \"title\", but put title last.
Function based on the same from the docstring for `eww-auto-rename-buffer'."
    (when (eq major-mode 'eww-mode)
      (when-let ((string (or (plist-get eww-data :title)
                             (plist-get eww-data :url))))
        (format "*eww: %s*" string))))
  (defun hoagie-eww-jump ()
    "Similar `elpher-jump', but for EWW.
It is based on the elpher code, but instead of opening the link,
it moves point to it, to take advantage of links'
`eww-follow-link' binding (using prefix or double prefix for
external browser and new eww buffer, respectively)."
    (interactive)
    (let ((all-links (hoagie--collect-shr-links)))
      (goto-char (alist-get (completing-read "Link: "
                                             all-links nil)
                            all-links
                            nil nil #'string=)))))

(use-package flymake
  ;; TODO: config or init? need to test loading flymake
  :init
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
  :bind
  (:map hoagie-second-keymap
        ("f" . hoagie-flymake-keymap)))

(use-package grep
  :custom
  (grep-command "grep --color=always -nHi -r --include=*.* -e \"pattern\" .")
  (grep-use-null-device nil)
  :bind
  (:map hoagie-keymap
        ("ESC g" . rgrep)))

(use-package hl-line
  :hook
  (after-init-hook . global-hl-line-mode))

(use-package mhtml-mode
  :hook
  (mhtml-mode-hook . hoagie-mhtml-mode-setup)
  :config
  (defun hoagie-mhtml-mode-setup ()
    "Setup my `mhtml-mode' configuration.
Set `fill-column' and related modes.'."
    (setf fill-column 100)
    (display-fill-column-indicator-mode)
    (auto-fill-mode)))

(use-package imenu
  :demand t
  :custom
  (imenu-auto-rescan t)
  (imenu-flatten 'annotation)
  :bind
  ("M-i" . imenu))

;; From Steve Yegge's post:
;; Info files out of the docs for just about anything. I have a custom "dir"...
;; I have the Perl 5 info pages and Ruby, and Python, and OCaml, Scheme,
;; Common Lisp, Guile, a whole bunch of language references.
;; All bookmarked heavily — Emacs has a very nice bookmark facility.
;; I also have info files for a bunch of Gnu tools, Emacs utilities, Unix
;; utilities, all kinds of stuff.
(use-package info
  :config
  (add-to-list 'Info-directory-list
               (expand-file-name "~/.local/share/info/")))

(use-package isearch
  :custom
  (search-exit-option 'edit)
  (isearch-lazy-count t)
  (isearch-lazy-highlight 'all-windows)
  (isearch-wrap-pause 'no)
  (isearch-repeat-on-direction-change t)
  (lazy-highlight-initial-delay 0.1)
  (regexp-search-ring-max 64)
  (search-ring-max 64))

(use-package lisp-mode
  :config
  (defun hoagie-lisp-mode-setup ()
    "Setup my `lisp-mode' configuration.
Set `fill-column' and setup indent to CL style."
    (set (make-local-variable lisp-indent-function)
		 'common-lisp-indent-function)
    (setf fill-column 100)
    (display-fill-column-indicator-mode))
  :hook
  (lisp-mode-hook . hoagie-lisp-mode-setup))

(use-package markdown-mode
  :ensure t
  :init
  (setq markdown-command "pandoc")
  :bind
  (:map markdown-mode-map
        ("C-c C-e" . markdown-do))
  :hook
  (markdown-mode-hook . hoagie-markdown-mode-setup)
  :config
  (defun hoagie-markdown-mode-setup ()
    "Setup my `markdown-mode' configuration.
Set `fill-column', `truncate-lines'."
    (setf fill-column 100
          truncate-lines t)
    (display-fill-column-indicator-mode)
    (auto-fill-mode)))

(use-package minibuffer
  :demand t
  :custom
  (completions-format 'one-column)
  (completions-max-height 25)
  (completion-styles '(flex))
  (read-buffer-completion-ignore-case t)
  (read-file-name-completion-ignore-case t)
  (completion-ignore-case t)
  (minibuffer-visible-completions t)
  (minibuffer-message-timeout 0.5)
  (completions-detailed t)
  (completion-auto-help 'always)
  (completion-auto-select 'second-tab)
  ;; not a minibuffer.el setting, but makes sense to it here
  (enable-recursive-minibuffers t)
  :bind
  (:map minibuffer-mode-map
        ("C-n" . minibuffer-next-completion)
        ("C-p" . minibuffer-previous-completion)
        ;; I want a keybinding to "force" the first candidate possible
        ;; This is useful for buffer switching and file selection: the default
        ;; of creating a new one is good, but I want a shorcut to (C-i + RET)
        ;; in one go, for cases where I know the partial input is good enough
        ("C-<return>" . minibuffer-force-complete-and-exit))
  (:map completion-in-region-mode-map
        ("C-n" . minibuffer-next-completion)
        ("C-p" . minibuffer-previous-completion)))

(use-package notifications
  ;; this package is used by appt to display
  ;; notifications in the desktop
  :demand t)

(use-package package-lint
  :ensure t
  :commands package-lint-current-buffer)

(use-package paren
  :custom
  ;; (show-paren-style 'mixed)
  (show-paren-when-point-inside-paren t))

(use-package proced
  :custom
  (proced-auto-update-flag t) ;; default is 5 seconds
  (proced-show-remote-processes t)
  (proced-filter 'all)
  ;; (proced-enable-color-flag t) ;; worth it? then I need to define faces =P
  )

(use-package project
  :bind
  (:map hoagie-keymap
        ;; Since I am using the C-x p prefix more,
        ;; these two bindings can go? maybe?
        ("g" . project-find-regexp)
        ("f" . project-find-file))
  :custom
  (project-vc-extra-root-markers '(".emacs-project"))
  ;; go back to old default of "d" for "dired at root"
  (project-switch-commands
   '((project-find-file "Find file" nil)
     (project-find-regexp "Find regexp" nil)
     (project-dired "Dired" ?d)
     (project-vc-dir "VC-Dir" nil)
     (project-shell "Shell" nil)
     (project-eshell "Eshell" nil)))
  :config
  ;; from https://dawranliou.com/blog/xref-with-eglot-and-project/
  (defun project-find-regexp-with-unique-buffer (orig-fun &rest args)
    "An advice function that gives project-find-regexp a unique buffer name"
    (require 'xref)
    (let ((xref-buffer-name (format "*xref %s*" (car args))))
      (apply orig-fun args)))
  (advice-add 'project-find-regexp :around
              #'project-find-regexp-with-unique-buffer))

(use-package python
  :mode ("\\.py\\'" . python-mode)
  :hook
  (python-mode-hook . hoagie-python-mode-setup)
  :custom
  (python-shell-font-lock-enable nil)
  (python-shell-interpreter "ipython")
  (python-shell-interpreter-args "--pprint --simple-prompt")
  :config
  (defun hoagie-python-mode-setup ()
    "Setup my `python-mode' configuration."
    (setf fill-column 79)
    (display-fill-column-indicator-mode)))

(use-package register
  :demand t
  :custom
  (register-preview-delay 0.1)
  :bind
  (:map hoagie-second-keymap
        ;; hitting F5 twice to store something sounds like a good
        ;; shortcut to push things semi-constantly
        ("<f5>" . hoagie-push-to-register-dwim)
        ("i" . hoagie-insert-register)
        ("l" . list-registers)
        ("d" . hoagie-clean-registers)
        ("j" . hoagie-jump-to-register))
  :config
  ;; BRITTLENESS WARNING: this re-defines a built-in method, there's
  ;; a high risk it breaks when moving Emacs versions
  (cl-defmethod register-val-describe ((val marker) _verbose)
    (let ((buf (marker-buffer val)))
      (if (null buf)
	      (princ "a marker in no buffer")
        (princ (hoagie--text-around-marker val))
        (princ "   -- buffer ")
        (princ (buffer-name buf))
        (princ ", pos ")
        (princ (marker-position val)))))
  (defun hoagie--text-around-marker (marker-val)
    "Get the line around MARKER.
Some inspiration from the package Consult."
    (with-current-buffer (marker-buffer marker-val)
      (save-excursion
        (without-restriction
          (goto-char marker-val)
          (string-trim (thing-at-point 'line))))))
  ;; There are a bunch of packages that do this or similar things,
  ;; but I have an interest on developing my own clunky, bug ridden,
  ;; and, I can only hope, flexible system
  (defun hoagie-push-to-register-dwim ()
    "If the region is active, store it in the next register. Else push point.
See `hoagie-get-next-register' for \"next register\" selection."
    (interactive)
    (if (use-region-p)
        (hoagie-copy-to-next-register)
      (hoagie-point-to-next-register)))
  (defvar hoagie-registers-order
    "asdfgqwertzxcvbyuiophjklnm;',./-=`?\"[](){}"
    "The order in which to walk the registers in `hoagie-next-register'")
  (defun hoagie-next-register ()
    "Return the next char from `hoagie-registers-order' that is empty.
Silently returns nil if none is available."
    (cl-loop for reg-key across hoagie-registers-order
             unless (get-register reg-key)
             return reg-key))
  (defun hoagie-copy-to-next-register ()
    "Copies the region to the register returned by `hoagie-next-register'.
The values passed to `copy-to-register' are based on its
interactive declaration."
    (interactive)
    (let ((register (hoagie-next-register)))
      (copy-to-register register
                        (region-beginning)
                        (region-end)
                        current-prefix-arg
                        t)
      (message "Text to register: %c" register)))
  (defun hoagie-point-to-next-register ()
    "Stores point in the register returned by `hoagie-next-register'.
The values passed to `point-to-register' are based on its
interactive declaration."
    (interactive)
    ;; I never want to store frame configurations...but in a future version
    ;; I could do window configurations? My current setup for windows stores
    ;; ONE window config and that seems to be enough, though.
    (let ((register (hoagie-next-register)))
      (point-to-register (hoagie-next-register) nil)
      (message "Point to register: %c" register)))
  (defun hoagie-jump-to-register (&optional arg)
    "Almost like `jump-to-register' but filters the alist for better preview.
It also deletes the register if called with prefix ARG."
    (interactive "P")
    (let* ((register-alist (cl-loop for reg in register-alist
                                    when (markerp (cdr reg))
                                    collect reg))
           (reg (register-read-with-preview "Jump to: ")))
      (jump-to-register reg)
      (when arg
        (set-register reg nil))))
  (defun hoagie-insert-register (&optional arg)
    "Almost like `insert-register' but filters the alist for better preview.
It also deletes the register when called with prefix ARG."
    (interactive "P")
    (let* ((register-alist (cl-loop for reg in register-alist
                                    when (stringp (cdr reg))
                                    collect reg))
           (reg (register-read-with-preview "Insert text: ")))
      (when (use-region-p)
        ;; when there's an active region, delete it first
        ;; NOTE: maybe I want to consider _killing_ instead of _deleting_?
        (delete-region (region-beginning) (region-end)))
      (insert-register reg)
      (when arg
        (set-register reg nil))))
  (defun hoagie-clean-registers ()
    (interactive)
    (while t
      (set-register (register-read-with-preview
                     "Register to clear (quit to exit): ")
                    nil))))

(use-package repeat
  :custom
  (repeat-exit-key "RET")
  :config
  (repeat-mode))

(use-package replace
  :bind
  (:map hoagie-keymap
        ("o" . hoagie-occur-symbol-or-region)
        ("ESC o" . multi-occur-in-matching-buffers)
        ;; good mirror of occur -> occur-dwim, except this
        ;; built in does have a default binding, it is "M-s ."
        ("s" . isearch-forward-thing-at-point))
  :hook
  (occur-hook . hoagie-rename-and-select-occur-buffer)
  :config
  (defun hoagie-occur-symbol-or-region ()
    "Run occur for the symbol at point, or the active region.
By default, occur _limits the search to the region_ if it is active."
    (interactive)
    (with-region-or-thing 'symbol
      (occur (regexp-quote (buffer-substring-no-properties start end))
             (when current-prefix-arg
	           (prefix-numeric-value current-prefix-arg)))))
  (defun hoagie-rename-and-select-occur-buffer ()
    "Renames the current buffer to *Occur: [term] [buffer]*.
Meant to be added to `occur-hook'."
    (cl-destructuring-bind (search-term _ (buffer-name &rest _))
        occur-revert-arguments
      (pop-to-buffer
       (rename-buffer (format "*Occur: %s %s*" search-term buffer-name) t)))))

(use-package restclient
  :ensure t
  :custom
  (restclient-same-buffer-response nil)
  (restclient-response-body-only nil)
  :mode
  ("\\.http\\'" . restclient-mode)
  :bind
  (:map restclient-mode-map
        ("C-c r" . rename-buffer)
        ("C-c h" . restclient-toggle-headers))
  :hook
  (restclient-mode-hook . hoagie-restclient-imenu-index)
  :config
  (defun restclient-toggle-headers ()
    (interactive)
    (message "restclient-response-body-only=%s"
             (setf restclient-response-body-only
                   (not restclient-response-body-only))))
  (defun hoagie-restclient-imenu-index ()
    "Configure imenu on the convention \"### Title\"."
    (setq-local imenu-generic-expression '((nil "^### \\(.*\\)$" 1)))))

(use-package savehist
  :custom
  (savehist-additional-variables '(kill-ring
                                   search-ring
                                   regexp-search-ring
                                   eww-history))
  (history-delete-duplicates t)
  :config
  (savehist-mode))

(use-package sharper
  :load-path "~/github/sharper"
  :if (locate-library "sharper.el")
  :bind
  (:map hoagie-keymap
        ;; using "n" for another command now, moving
        ;; this to "c" for C#
        ("c" . sharper-main-transient))
  :custom
  (sharper-run-only-one t))

(use-package shell
  :custom
  (shell-get-old-input-include-continuation-lines t)
  :hook
  (shell-mode-hook . hoagie-shell-mode-setup)
  :config
  (defun hoagie-shell-mode-setup ()
    "Configure shell buffers."
    (toggle-truncate-lines t)
    (setq comint-process-echoes t)))

(use-package shr
  :custom
  (shr-use-colors t)
  (shr-bullet "• ")
  (shr-discard-aria-hidden t)
  (shr-max-image-proportion 0.5)
  :bind
  (:map hoagie-keymap
        ;; Tried assigning this in shr-map, but it really needs a
        ;; global binding. "ESC n" mirrors "n" for filenanes
        ("ESC n" . hoagie-shr-link-open-or-kill))
  :config
  (setf shr-indentation 2)
  (defun hoagie-shr-link-open-or-kill (&optional arg)
    "Edit and open the link at point.
With prefig ARG, put it in the kill ring instead."
    (interactive "P")
    (if-let* ((target-url (get-text-property (point) 'shr-url)))
        (progn
          (message "Killed link: %s" target-url)
          (if arg
              (kill-new target-url)
            (eww (read-string "" target-url))))
      (error "No shr link under point")))
  (defun hoagie--collect-shr-links ()
    "Get an alist of all link targets in the current buffer.
The format returned is (link-text . link-position)
Inspired by a similar function in Elpher."
    (save-excursion
      (goto-char (point-min))
      (cl-loop for link = (text-property-search-forward 'shr-url nil nil t)
               while link
               for position = (prop-match-beginning link)
               for text = (buffer-substring position
                                            (prop-match-end link))
               collect
               (cons text position)))))

(use-package sly
  :ensure t
  :commands sly
  :custom
  (inferior-lisp-program "sbcl --dynamic-space-size 10240"))

(use-package sly-quicklisp
  :ensure t
  :after sly)

(use-package sql
  :custom
  (sql-display-sqli-buffer-function t)
  :hook
  (sql-interactive-mode-hook . hoagie-sql-interactive-setup)
  (sql-mode-hook . hoagie-sql-setup)
  :config
  (defun hoagie-sql-setup ()
    "Configure sql-mode."
    (setf truncate-lines t
          fill-column 100)
    (auto-fill-mode)
    (display-fill-column-indicator-mode))
  (defun hoagie-sql-interactive-setup ()
    "Configure SQLi."
    (setf truncate-lines t)
    (setq-local imenu-generic-expression '((nil "^\\(.*\\)" 1)))))

;; connections defined in sc-init.el
(use-package sql-datum
  :load-path "~/github/datum"
  :after sql
  :custom
  (sql-datum-program "datum"))

(use-package time
  :custom
  (world-clock-time-format "%r - %F (%A)")
  (world-clock-list '(("America/Denver" "Denver")
                      ("America/Buenos_Aires" "Buenos Aires")
                      ("Europe/Madrid" "España")
                      ("America/Mexico_City" "CDMX")
                      ("America/Chicago" "Central")
                      ("America/New_York" "Eastern")))
  :commands (world-clock))

(use-package timer
  :commands (run-at-time)
  :config
  ;; inspired by this code
  ;; https://www.reddit.com/r/emacs/comments/7wsnoi/comment/du3h11l/
  (defun hoagie-notify-timer (message when)
    "Show MESSAGE at TIME, in a desktop notification.
Time can be anything accepted by `run-at-time'."
    (interactive
     "sMessage (default: \"Timer Elapsed\"): \nsSeconds (or \"#min\"): ")
    (when (string-empty-p message)
      (setf message "Timer elapsed"))
    (run-at-time when nil #'notifications-notify
                 :title "Emacs - Timer" :body message
                 :app-name "Emacs" :timeout 0 :urgency 'normal)))

(use-package vc
  :demand t
  :custom
  (vc-allow-rewriting-published-history t)
  :bind
  (:map vc-prefix-map
        ;; make it consistent with vc-dir
        ("k" . vc-revert)))

(use-package vc-dir
  :after (vc project vc-git)
  :bind
  ;; shadows `vc-dir'
  ("C-x v d" . vc-dir-root)
  (:map vc-dir-mode-map
        ("e" . vc-ediff)
        ("k" . vc-revert)
        ("r" . hoagie-vc-dir-reset)
        ("b b" . hoagie-vc-git-list-branches))
  :config
  (defun hoagie-vc-dir-reset (&optional arg)
    "Runs \"git reset\" to unstage all changes.
With prefix arg, does a hard reset (thus it asks for confirmation)."
    (interactive "P")
    (if arg
        (when (y-or-n-p "Perform a hard reset? ")
          (vc-git-command nil 0 nil "reset" "--hard")
          (message "Completed. All pending changes are lost."))
      (vc-git-command nil 0 nil "reset")
      (message "All changes are unstaged."))
    (vc-dir-refresh)))

(use-package vc-git
  :after vc
  :custom
  (vc-git-revision-complete-only-branches t)
  (vc-git-log-switches '("--date=iso-local" "--stat"))
  ;; (vc-git-shortlog-switches 'TBD)
  :config
  (defvar hoagie-vc-git-emails
    '("code@sebasmonia.com"
      "some.work@email.com :)")
    "List of email addresses that can be associated with a repository")
  (defun hoagie-vc-git-clone (repository-url local-dir email)
    "Run \"git clone REPOSITORY-URL\" to LOCAL-DIR.
After cloning, EMAIL is set in the repo.
Interactively, the email is read from `hoagie-vc-git-emails', and it
also runs `vc-dir' in the newly cloned directory."
    (interactive
     (let* ((url (read-string "Repository URL: "))
            (dir (read-directory-name "Target directory: " nil nil nil
                                      (file-name-base url)))
            (mail (completing-read "Email for this repo: "
                                   hoagie-vc-git-emails)))
       (list url dir mail)))
    (vc-git-clone repository-url (expand-file-name local-dir) nil)
    (let ((default-directory local-dir))
      (vc-git-command nil 0 nil "config" "user.email" email))
    (when (called-interactively-p 'any)
      (vc-dir local-dir)))
  (defun hoagie-vc-git-amend ()
    "Unconditionally amend the last commit.
Use this for cases where you fumbled the last commit message AND don't
need any other changes (if there are code changes, you can use the
regular vc-mode flow)."
    (interactive)
    (if (and server-process (getenv "GIT_EDITOR"))
        (vc-git-command "*git commit amend*" 'async nil "commit" "--amend")
      (error "Emacs server not running, or GIT_EDITOR not set")))
  (defun hoagie-vc-git-list-branches (&optional arg)
    "Show in a buffer the list of branches in the current repository.
With prefix ARG show the remote branches."
    ;; built in vc-git-branches doesn't show remote branches
    (interactive "P")
    (let* ((root-dir-name (file-name-nondirectory
                           (directory-file-name
                            (vc-git-root default-directory))))
           (buffer-name (format "*%s - %s branches*"
                                root-dir-name
                                (if arg "remote" "local"))))
      (vc-git-command buffer-name 0 nil "branch" (when arg "-r"))
      (pop-to-buffer buffer-name)
      (goto-char (point-min))
      (special-mode)))
  (defun hoagie-vc-git-interactive-rebase ()
    "Do an interactive rebase against another branch.
This command needs the Emacs server running and GIT_EDITOR properly set.
You can override the branch name to something like \"HEAD~2\", for example."
    (interactive "P")
    (if (and server-process (getenv "GIT_EDITOR"))
        (vc-git-command "*git rebase -i*" 'async nil "rebase" "-i"
                        (completing-read "Rebase target (branch or commit): "
                                         (cdr (vc-git-branches))))
      (error "Emacs server not running, or GIT_EDITOR not set"))))

(use-package vc-hooks
  :after (vc vc-git)
  :custom
  (vc-display-status 'no-backend)
  ;; from https://emacs.stackexchange.com/a/37855, which links to TRAMP's manual
  (vc-ignore-dir-regexp (format "\\(%s\\)\\|\\(%s\\)"
                                vc-ignore-dir-regexp
                                tramp-file-name-regexp))
  :bind
  (:map vc-prefix-map
        ;; "l"ist is used for branch-log, use "b"ranches
        ("b b" . hoagie-vc-git-list-branches)
        ("e" . vc-ediff)))

(use-package vundo
  :demand t
  :bind
  ("C-x /" . vundo))

(use-package window
  :config
  ;; Stores the window setup before focusing on a single window, and restore it
  ;; on a "mirror" binding: C-x 1 vs F6 1. Simplified version of the idea at
  ;; https://erick.navarro.io/blog/save-and-restore-window-configuration-in-emacs/
  ;; Update: made it so I can manually push a config too. Maybe the alternative
  ;; command is superflous? or maybe I will need multiple window configuration
  ;; slots.
  (defvar hoagie-window-configuration nil
    "Window configuration saved manually, or before deleting other windows.")
  (defun hoagie-store-window-configuration (&optional silent)
    (interactive)
    (setf hoagie-window-configuration (current-window-configuration))
    (unless silent
      (message "Stored current window configuration")))
  (defun hoagie-restore-window-configuration ()
    "Use `hoagie-window-configuration' to restore the window setup."
    (interactive)
    (when hoagie-window-configuration
      (set-window-configuration hoagie-window-configuration)
      (setf hoagie-window-configuration nil)))
  (defun hoagie-delete-other-windows ()
    "Custom `delete-other-windows' that stores the current setup in
`hoagie-window-configuration'.
Adding an advice to the existing command was finicky."
    (interactive)
    (hoagie-store-window-configuration t)
    (delete-other-windows))
  (defun hoagie-toggle-frame-split ()
    "Toggle orientation, just like ediff's |.
See https://www.emacswiki.org/emacs/ToggleWindowSplit for
sources, this version is my own spin of the first two in the
page."
    (interactive)
    (unless (= (count-windows) 2)
      (error "Can only toggle a frame split in two"))
    (let ((was-split-vertically (window-combined-p))
          (other-buffer (window-buffer (next-window))))
      (delete-other-windows) ; closes the other window
      (if was-split-vertically
          (split-window-horizontally)
        (split-window-vertically))
      (set-window-buffer (next-window) other-buffer)))
  :bind
  (:map ctl-x-map
        ("1" . hoagie-delete-other-windows)
        ("|" . hoagie-toggle-frame-split))
  (:map hoagie-keymap
        ;; experimental: store window setup manually, use case:
        ;; I'm reading email in Gnus, and want to look at the calendar
        ;; or world clock or diary
        ("ESC 1" . hoagie-store-window-configuration)
        ("1" . hoagie-restore-window-configuration)
        ("|" . hoagie-toggle-frame-split)))

(use-package ws-butler
  :ensure t
  :hook
  (prog-mode-hook . ws-butler-mode))

;; Everything that is not part of a particular feature to require
(use-package emacs
  :init
  (defun hoagie-go-home (arg)
    "Open ~. Prefix ARG to open in other window."
    (interactive "P")
    (if arg
        (dired-other-window "~/")
      (dired "~/")))
  ;; Inspired by
  ;; https://demonastery.org/2013/04/emacs-narrow-to-region-indirect/ and
  ;; modified to DWIM. Also use `pop-to-buffer' instead of `switch-to-buffer'
  (defun hoagie-narrow-indirect-dwim (&optional arg)
    "Create an indirect buffer, narrow it to defun or active region.
If ARG, don't prompt for buffer name suffix."
    (interactive "P")
    (with-region-or-thing 'defun
      (let* ((new-name (unless arg
                         (format "%s<%s>"
                                 (buffer-name)
                                 (read-string "<suffix>: "))))
             ;; we'll pop the buffer manually to clear the region
             (buf (clone-indirect-buffer new-name nil)))
        (with-current-buffer buf
          (narrow-to-region start end)
          (deactivate-mark))
        (pop-to-buffer buf))))
  (defun hoagie-split-window-right (&optional arg)
    "Like `split-window-right', but select the new window.
With prefix ARG, use `split-root-window-right' instead"
    (interactive "P")
    (select-window (if arg
                       (split-root-window-right)
                     (split-window-right))))
  (defun hoagie-split-window-below (&optional arg)
    "Like `split-window-below', but select the new window.
With prefix ARG, use `split-root-window-below' instead"
    (interactive "P")
    (select-window (if arg
                       (split-root-window-below)
                     (split-window-below))))
  :bind
  ("<S-f1>" . (lambda () (interactive) (find-file user-init-file)))
  ("<f1>" . hoagie-go-home)
  ;; Window management
  ("S-<left>" . (lambda () (interactive)(shrink-window-horizontally 5)))
  ("S-<right>" . (lambda () (interactive)(enlarge-window-horizontally 5)))
  ("S-<up>" . (lambda () (interactive)(shrink-window 5)))
  ("S-<down>" . (lambda () (interactive)(shrink-window -5)))
  ;; from https://emacsredux.com/blog/2020/06/10/comment-commands-redux/
  ("<remap> <comment-dwim>" . comment-line)
  ;; replace delete-char, as recommended in the docs
  ("C-d" . delete-forward-char)
  ("M-c" . capitalize-dwim)
  ("M-u" . upcase-dwim)
  ("M-l" . downcase-dwim)
  ("M-z" . zap-up-to-char)
  ("C-x k" . kill-current-buffer)
  ("C-x ESC k" . kill-buffer)
  ("<remap> <list-buffers>" . ibuffer)
  ;; it's back...
  ("<remap> <list-buffers>" . ibuffer)
  (:map hoagie-keymap
        ;; this is much easier to type than C-S-backspace
        ;; and mirrors C-k nicely.
        ;; C-k kill rest of the line
        ;; <f6>-k kill the whole thing
        ;; (F6 and C are next to each other in the Raise)
        ("k" . kill-whole-line))
  (:map ctl-x-map
        ;; Back to C-x n i, I simply internalized "C-x n" for all
        ;; narrowing commands. Now "F5 c" is free again...
        ("n i" . hoagie-narrow-indirect-dwim)
        ;; right next to other-window
        ("i" . other-frame)
        ;; add meta to get the original command for C-x i...
        ;; ...although I never used it. UPDATE: used it a couple times :)
        ("ESC i" . insert-file)
        ;; combines https://stackoverflow.com/a/6465415 with
        ;; https://www.reddit.com/r/emacs/comments/1juhasp/comment/mm2m4ne/
        ;; by using prefix-arg UPDATE: improves on the SO answer :D
        ("3" . #'hoagie-split-window-right)
        ("2" . #'hoagie-split-window-below))
  ;; repeat C-x o and C-x i, and even switch between them
  (:repeat-map hoagie-other-window-frame-repeat-map
               ("o" . other-window)
               ("i" . other-frame)
               ("0" . delete-window))
  ;; extend selections with the last key that activates a mark command:
  (:repeat-map hoagie-mark-repeat-map
               ("SPC" . mark-sexp)
               ("@" . mark-word))
  ;; use simple SPC keystroke to repeat the cycling
  (:repeat-map hoagie-cycle-repeat-map
               ("SPC" . cycle-spacing))
  ;; repeat a few "C-M-something" movemeent commands
  (:repeat-map hoagie-sexp-movement-repeat-map
               ("u" . backward-up-list)
               ("d" . down-list)
               ("f" . forward-sexp)
               ("b" . backward-sexp))
  (:repeat-map hoagie-undo-repeat-map
               ("/" . undo)
               ("u" . undo)
               ("r" . undo-redo)
               :exit
               ("v" . vundo))
  :custom
  (create-lockfiles nil)
  ;; from TRAMP's FAQ
  (remote-file-name-inhibit-locks t)
  (sentence-end-double-space nil)
  (tab-always-indent 'complete)
  (read-minibuffer-restore-windows nil) ;; finally...
  (tab-width 4) ;; make golang code nicer to read
  (delete-pair-blink-delay 0.1)
  ;; This feature was the culprit of the messages "No matching parenthesis
  ;; found" in the minibuffer, an annoyance in `zap-up-to-char' - and probably
  ;; other commands. With `show-paren-mode', it feels superfluous.
  (blink-matching-paren nil)
  (recenter-positions '(1 middle -2)) ;; behaviour for C-l
  (read-file-name-completion-ignore-case t) ;; useful in Linux
  ;; via https://github.com/jacmoe/emacs.d/blob/master/jacmoe.org
  (help-window-select t)
  ;; tired of this question. Sorry not sorry
  (custom-safe-themes t)
  (indent-tabs-mode nil)
  ;; from https://karthinks.com/software/batteries-included-with-emacs/
  ;; use view-mode for all read only files automatically
  (view-read-only t)
  (delete-by-moving-to-trash t)
  (inhibit-startup-screen t)
  (initial-buffer-choice t)
  (initial-scratch-message
   ";; Il semble que la perfection soit atteinte non quand il n’y a\n;; plus rien à ajouter, mais quand il n’y a plus à retrancher.\n;;                                   - Antoine de Saint Exupéry\n\n;; C-x C-k e edit kmacro             ;; (shell) C-c C-o clear last output\n;; C-x / vundo                       ;; C-x C-t transpose-lines (0 arg!)\n;; C-x ESC ESC repeat command        ;; C-o / C-M-o   open / split line\n\n;; During isearch                    ;; Less common search/replace\n;; C-w add word at point, can repeat ;; M-s . isearch symbol at point\n;; M-r toggle regex                  ;; C-u M-% to replace words\n\n;; M-x...\n;; copy-matching-lines (also kill-)  ;; (un)highlight-regexp\n;; align-current (or align-regexp)\n\n;; Calendar & Diary\n;; . - go to today                    ;; u/m/x - unmark/mark events/holidays\n\n;; Replace in many files:\n;; 1. multi-occur (if buffers visiting)\n;; 2. in Dired, Q -> regexp replace in marked files\n;; 3. F6-f (find-name-dired, find-grep-dired), then #2\n\n;; Notes prefix <f2> => 2 inbox / n new / g grep / f find by name\n\n;; Source functions (familiarize): help-find-source, find-variable,\n;;                                 find-function, find-function-on-key\n\n;; New commands (replace?): replace-regexp-as-diff,\n;;                          multi-file-replace-regexp-as-diff,\n;;                          dired-do-replace-regexp-as-diff\n\n\n")
  (save-interprogram-paste-before-kill t)
  (visible-bell nil)
  ;; from https://gitlab.com/jessieh/dot-emacs
  (backup-by-copying t)   ; Don't delink hardlinks
  (version-control t)     ; Use version numbers on backups
  (delete-old-versions t) ; Do not keep old backups
  (kept-new-versions 5)   ; Keep 5 new versions
  (kept-old-versions 5)   ; Keep 3 old versions
  ;; from https://depp.brause.cc/dotemacs/
  (echo-keystrokes 0.25)
  (use-short-answers t)
  ;; this works for compile but also occur, grep etc
  (next-error-message-highlight t)
  :config
  ;; see https://emacs.stackexchange.com/a/28746/17066 and
  ;; https://blog.danielgempesaw.com/post/129841682030/fixing-a-laggy-compilation-buffer
  (setf disabled-command-function nil
        w32-use-native-image-API t
        inhibit-compacting-font-caches t
        auto-window-vscroll nil
        compilation-error-regexp-alist (delete 'maven compilation-error-regexp-alist))
  ;; https://200ok.ch/posts/2020-09-29_comprehensive_guide_on_handling_long_lines_in_emacs.html
  (setq-default bidi-paragraph-direction 'left-to-right
  ;; from https://github.com/SystemCrafters/rational-emacs/blob/master/modules/rational-defaults.el
                bidi-inhibit-bpa t
  ;; from https://blogs.dgplug.org/sandeepk/no-newline-at-the-end-of-file-_-tsu-_
                require-final-newline t)
  (delete-selection-mode)
  (blink-cursor-mode -1)
  (column-number-mode 1)
  (horizontal-scroll-bar-mode -1)
  (global-so-long-mode 1)
  ;; from https://stackoverflow.com/a/22176971, move auto saves and
  ;; back up files to a different folder so git or dotnet core won't
  ;; pick them up as changes or new files in the project
  (let ((auto-save-dir (expand-file-name (concat
                                          user-emacs-directory
                                          "auto-save/")))
        (backup-dir (expand-file-name (concat
                                       user-emacs-directory
                                       "backups/"))))
    (make-directory auto-save-dir t)
    (make-directory backup-dir t)
    (setf auto-save-list-file-prefix auto-save-dir
          auto-save-file-name-transforms
          `((".*" ,auto-save-dir t)))
    (make-directory backup-dir t)
    (setf backup-directory-alist
          `((".*" . ,backup-dir)))))


(when (and (eq system-type 'gnu/linux) (display-graphic-p))
  (set-fontset-font t 'emoji (font-spec :family "Noto Emoji"))

  (defun hoagie-adjust-font-size (frame)
    "Inspired by https://emacs.stackexchange.com/a/44930/17066.
FRAME is ignored."
    ;; 2021-05-22: now I use the pgtk branch everywhere, and the monitor name
    ;; has a meaningul value in all cases, so:
    (let* ((monitor-name (alist-get 'name (frame-monitor-attributes)))
           (monitor-font '(("0x0536" . 143) ;; laptop -- maybe 151?
                           ("LG Ultra HD" . 181))) ;; 173?
           (size (alist-get monitor-name monitor-font
                            180 ;; default size, "big just in case"
                            nil
                            'equal)))
      (set-face-attribute 'default (selected-frame) :height size)))
  (add-hook 'window-size-change-functions #'hoagie-adjust-font-size))

(load-file "~/sourcehut/dotfiles/.config/emacs/hoagie-theme.el")
(load-theme 'hoagie t)

(load-file "~/sourcehut/dotfiles/.config/emacs/hoagie-mode-line.el")

;; let's do our best to keep Gnus files/dir outside of ~
(load-file "~/sourcehut/dotfiles/.config/gnus/.gnus.el")

(use-package site
  :load-path "~/sourcehut/site.sebasmonia"
  :if (locate-library "site.el")
  :demand t)

;; is it a work computer...?
(when (file-exists-p "~/sourcehut/simcorp-files/.emacs.d/sc-init.el")
  (defvar sc-init-file
    "~/sourcehut/simcorp-files/.emacs.d/sc-init.el"
    "Location of the SimCorp init file.")
  (load sc-init-file)
  (keymap-global-set "ESC S-<f1>"
                     (lambda () (interactive) (find-file sc-init-file))))

;;; init.el ends here
