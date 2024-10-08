;;; .emacs --- My dot emacs file  -*- lexical-binding: t; -*-

;; Author: Sebastián Monía <code@sebasmonia.com>
;; URL: https://git.sr.ht/~sebasmonia/dotfiles
;; Version: 30.4
;; Keywords: tools maint

;; This file is not part of GNU Emacs.

;;; Commentary:

;; My dot Emacs file
;; Recent changes:
;; 2023-08-31: Notifications and appointments setup
;; 2023-10-09: Move unused languages/tools to purgatory.el
;; 2023-11-07: Move general editing commands to hoagie-editing.el
;; 2023-11-24: Remove some bindings, add auth-source customization, add some
;;             initialization to sql-mode and markdown-mode.
;; 2023-12-06: Use my own mode-line configuration. Require Emacs 30.
;; 2024-02-25: Move from Gnus to mu4e (and elfeed)
;; 2024-06-26: Remove howm - I only used like, 3 features from it.
;; 2024-07-10: Going back to Gnus with IMAP back end, sending elfeed to
;;             purgatory, and putting note comments in a separate package.
;; 2024-08-29: Move mode-line setup to separate file.
;;; Code:

(setf custom-file (locate-user-emacs-file "custom.el"))

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(setf package-install-upgrade-built-in t)

;; (setf use-package-compute-statistics t)

(when (eq window-system 'pgtk)
  (pgtk-use-im-context t))

(setf comp-deferred-compilation t
      warning-minimum-level :error)

(require 'use-package)
(setf use-package-verbose t)
(setf use-package-hook-name-suffix nil)
(setf package-native-compile t)

(when (display-graphic-p)
  (custom-set-faces
   '(default ((t (:family "Berkeley Mono"
                          :slant normal
                          :weight regular
                          :height 120
                          :width normal
                          :foundry "outline"))))))

;; Based on http://www.ergoemacs.org/emacs/emacs_menu_app_keys.html I
;; eventually I moved away from the menu key to F6, and even later that key
;; ended in a very similar place to <menu> but on the other side of the
;; keyboard (Dygma Raise)
(defvar hoagie-keymap
  (define-prefix-command 'hoagie-keymap)
  "My custom bindings.")
(defvar hoagie-second-keymap
  (define-prefix-command 'hoagie-second-keymap)
  "Originally a register keymap, now used for other stuff too.")
;; compat Linux-Windows
(define-key key-translation-map (kbd "<apps>") (kbd "<menu>"))
(define-key key-translation-map (kbd "<print>") (kbd "<menu>"))
(global-set-key (kbd "<menu>") 'hoagie-keymap)

;; In case the config is not running on Silverblue, or if I ever layer Emacs on
;; the base system...
(defvar hoagie-toolbox-name
  (if (file-exists-p "/run/.containerenv")
      ;; from http://ergoemacs.org/emacs/elisp_read_file_content.html
      ;; insert content in temp buffer rather than open a file
      (with-temp-buffer
        (insert-file-contents "/run/.containerenv")
        (search-forward "name=") ;; move point to the line with the name
        (setf hoagie-container-name
              (cl-subseq (thing-at-point 'line) 6 -2)))
    "host")
  "Stores the name of the current container, if present.")

;; these keys are mapped on particular positions in my Dygma Raise
(global-set-key (kbd "<f6>") 'hoagie-keymap) ;; T1 (next to SPC/Control)
(global-set-key (kbd "<f5>") 'hoagie-second-keymap) ;; Enter

;; This package declares commands and macros (!) that I use for general
;; editing
(use-package hoagie-editing
  :load-path "~/sourcehut/dotfiles/.config/emacs"
  :demand t
  :bind
  ("C-<backspace>" . hoagie-backward-delete-word)
  ("C-z" . hoagie-region-to-char)
  (:map hoagie-keymap
        ("/" . hoagie-toggle-backslash)
        ("p" . hoagie-insert-pair) ;; and "u" for built-in unpair
        ("q" . hoagie-escape-regexp)
        ("t" . hoagie-insert-datetime))
  (:map hoagie-second-keymap
        ("q" . hoagie-split-by-newline)
        ;; always have a binding for plain old fill-paragraph (it tends to be
        ;; replaced/shadowed in a lot of modes).
        ("ESC q" . fill-paragraph)))

(use-package hoagie-notes
  :load-path "~/sourcehut/dotfiles/.config/emacs"
  :demand t
  :bind
  ("<f3>" . hoagie-notes-keymap))

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

(use-package browse-url
  :custom
  (browse-url-secondary-browser-function #'hoagie-browse-url)
  (browse-url-browser-function #'eww-browse-url)
  :config
  (defun hoagie-browse-url (url &rest args)
    "Open Firefox, changing invocation if running in a toolbox.
Running in a toolbox is actually the \"common\" case. :)"
    (if (string= hoagie-toolbox-name "host")
        (browse-url-firefox url args)
      (call-process "flatpak-spawn"
                    nil
                    0
                    nil
                    "--host"
                    "firefox"
                    url))))

(use-package calendar
  :demand t
  :custom
  (calendar-date-style 'iso)
  (calendar-view-diary-initially-flag nil)
  (calendar-latitude 40.7)
  (calendar-longitude -73.9)
  (calendar-location-name "New York, NY")
  (calendar-setup 'one-frame)
  ;; show events for the next 7 days when the calendar opens
  (diary-number-of-entries 7)
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
  :bind
  (:map hoagie-keymap
        (("ESC f" . find-name-dired)
         ;; for some cases where I used prefix args or shifted keys
         ;; I am now trying this alternative of adding ESC to the binding
         ("ESC j" . dired-jump)
         ("j" . dired-jump-other-window)
         ;; n for "name"
         ("n" . hoagie-kill-buffer-filename)))
  (:map dired-mode-map
        ("C-<return>" . hoagie-dired-os-open-file))
  :hook
  (dired-mode-hook . dired-hide-details-mode)
  ;; Enables "C-c C-m a" (yeah, really :) lol) to attach
  ;; all files marked in dired to the current/a new email
  (dired-mode-hook . turn-on-gnus-dired-mode)
  :config
  (setf dired-compress-file-suffixes
        '(("\\.tar\\.gz\\'" #1="" "7z x -aoa -o%o %i")
          ("\\.tgz\\'" #1# "7z x -aoa -o%o %i")
          ("\\.zip\\'" #1# "7z x -aoa -o%o %i")
          ("\\.7z\\'" #1# "7z x -aoa -o%o %i")
          ("\\.tar\\'" ".tgz" nil)
          (":" ".tar.gz" "tar -cf- %i | gzip -c9 > %o"))
        dired-compress-files-alist
        '(("\\.7z\\'" . "7za a -r %o %i")
          ("\\.zip\\'" . "7za a -r %o  %i")))
  (defun hoagie-dired-os-open-file ()
    "Open a file with the default OS program.
Initial version from EmacsWiki, added macOS & Silverblue toolbox
support. This could also use `w32-shell-execute' on Windows.
Also, the binding W `browse-url-of-dired-file' is a valid
replacement, but not sure about toolboxes..."
    (interactive)
    (let ((program-name (cond ((eq system-type 'darwin) "open")
                              ;; Used to use start "" {path}, but this
                              ;; one works too
                              ((eq system-type 'windows-nt) "explorer")
                              ;; For Linux, change based on toolbox
                              ;; vs non-toolbox
                              (t (if (string= hoagie-toolbox-name "host")
                                     "xdg-open"
                                   "flatpak-spawn"))))
          (target-filename (dired-get-filename nil t)))
      ;; for Windows, replace the slashes in the name for "explorer" to work
      (when (eq system-type 'windows-nt)
        ;; see https://stackoverflow.com/a/9910097
        (setf target-filename (subst-char-in-string ?/ ?\\ target-filename)))
      (apply #'call-process
             program-name
             ;; arguments to `call-process' + args for toolbox when required +
             ;; target filename
             `(nil 0 nil
                   ,@(unless (string= hoagie-toolbox-name "host")
                       '("--host" "xdg-open"))
                   ,target-filename))))
  (defun hoagie-kill-buffer-filename ()
    "Sends the current buffer's filename to the kill ring."
    (interactive)
    (if (derived-mode-p 'dired-mode)
        (dired-copy-filename-as-kill 0)
      (let ((name (buffer-file-name)))
        (when name
          (kill-new name))
        (message (format "Filename: %s"
                         (or name "-No file for this buffer-")))))))

(use-package dired-narrow
  :ensure t
  :after dired
  :bind
  (:map dired-mode-map
        ("/" . dired-narrow)))

(use-package ecomplete
  :config
  (ecomplete-setup)
  (defun ecomplete-add-email ()
    "Add email address to ecomplete's database.
Inspired by oantolin's ecomplete-extras."
    (interactive)
    (let ((email (read-string "Email: "))
          (name (read-string "Name: ")))
      (ecomplete-add-item 'mail
                          email
                          (concat name " <" email ">"))
      (ecomplete-save)))
  (defun ecomplete-list-emails (&optional regexp)
    "Pretty print the ecomplete emails that match REGEXP.
If REGEXP is not provided, then all emails are printed."
    (interactive "sMatch text (empty for no filter): ")
    ;; since splitting the name isn't exactly precise,
    ;; and we already have the keys available, let's extract
    ;; only the name from the display text
    (with-current-buffer (get-buffer-create "*ecomplete emails*")
      (require 'vtable)
      (erase-buffer)
      (make-vtable
       :columns '("Name" "Email")
       :objects (cl-loop for (email _count _time display) in
                         (alist-get 'mail ecomplete-database)
		                 when (or (null regexp)
                                  (string-match regexp email)
                                  (string-match regexp display))
		                 collect (list
                                  (car (split-string display
                                                     " <"))
                                  email))
       ;; force fixed width face
       :face 'default
       ;; sort by display text (or, name)
       :sort-by '((0 . ascend))))
    (pop-to-buffer "*ecomplete emails*")))

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
    (define-key ediff-mode-map "d" #'ediff-copy-both-to-C))
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
  :commands (eglot eglot-ensure)
  :custom
  (eglot-events-buffer-config '(:size 0 :format full))
  :hook
  ((python-mode-hook . eglot-ensure)
   (go-mode-hook . eglot-ensure))
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
        ("ESC w" . eww-list-bookmarks))
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

(defvar hoagie-flymake-keymap
  (define-prefix-command 'hoagie-flymake-keymap)
  "Custom bindings for `flymake-mode'.")
(use-package flymake
  :config
  :bind
  (:map hoagie-flymake-keymap
        ;; flymake command alternative bindings
        ("l" . flymake-show-buffer-diagnostics)
        ("n" . flymake-goto-next-error)
        ("p" . flymake-goto-prev-error)
        ("s" . flymake-start))
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

(use-package json-mode
  :ensure t
  :mode "\\.json$")

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
  (completions-max-height 20)
  (completion-styles '(flex))
  (read-buffer-completion-ignore-case t)
  (read-file-name-completion-ignore-case t)
  (completion-ignore-case t)
  (completions-detailed t)
  ;; experimental, was using 'visible
  (completion-auto-help 'always)
  (completion-auto-select 'second-tab)
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
        ("C-p" . minibuffer-previous-completion))
  (:map hoagie-keymap
        ("<f6>" . execute-extended-command)))

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
  (proced-show-remote-processes t)
  (proced-filter 'all))

(use-package project
  :bind
  (:map hoagie-keymap
        ("g" . project-find-regexp)
        ("f" . project-find-file))
  :custom
  (project-vc-extra-root-markers '(".emacs-project"))
  ;; TODO: tried it briefly, not that useful.
  ;; reconsider in the future
  (project-mode-line nil)
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

(use-package re-builder
  :custom
  ;; let's try this thing...
  (reb-re-syntax 'rx))

(use-package register
  :demand t
  :custom
  (register-preview-delay 0.1)
  :bind
  (:map hoagie-second-keymap
        ;; hitting F5 twice to store something sounds like a good
        ;; shortcut to push things semi-constantly
        ("<f5>" . hoagie-push-to-register-dwim)
        ("s" . copy-to-register)
        ("i" . hoagie-insert-register)
        ("l" . list-registers)
        ("SPC" . point-to-register)
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

(use-package replace
  :bind
  (:map hoagie-keymap
        ("o" . hoagie-occur-symbol-or-region)
        ("ESC o" . multi-occur-in-matching-buffers)
        ;; good mirror of occur -> occur-dwim, except
        ;; this built in does have a default binding
        ("s" . isearch-forward-thing-at-point))
  :hook
  (occur-hook . hoagie-rename-and-select-occur-buffer)
  :config
  (defun hoagie-occur-symbol-or-region ()
    "Run occur for the symbol at point, or the active region.
By default, occur _limits the search to the region_ if it is active."
    (interactive)
    (with-region-or-thing 'symbol
      (occur (buffer-substring-no-properties start
                                             end)
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
  ("<f4>" . hoagie-open-restclient)
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
  (defun hoagie-open-restclient (&optional arg)
    "Open a file from the restclient \"collection\".
Use prefix ARG to open the file in another window."
    (interactive "P")
    (let ((restclient-file (read-file-name "Open restclient file:"
                                           "~/restclient/"
                                           nil
                                           nil
                                           nil
                                           (lambda (name)
                                             (string-equal
                                              (file-name-extension name)
                                              "http")))))
      (if arg
          (find-file-other-window restclient-file)
        (find-file restclient-file))))
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
  :bind
  (:map hoagie-keymap
        ;; using "n" for another command now, moving
        ;; this to "c" for C#
        ("c" . sharper-main-transient))
  :custom
  (sharper-run-only-one t))

(use-package shell
  :hook
  (shell-mode-hook . hoagie-shell-mode-setup)
  :config
  (defun hoagie-shell-mode-setup ()
    "Configure shell buffers."
    (toggle-truncate-lines t)
    (setq comint-process-echoes t)))

(use-package shr
  :custom
  ;; (shr-use-fonts nil)
  (shr-use-colors t)
  (shr-bullet "• ")
  (shr-discard-aria-hidden t)
  (shr-max-image-proportion 0.5)
  :bind
  (:map hoagie-keymap
        ("ESC b" . hoagie-shr-link-open-or-kill))
  :config
  (setf shr-indentation 2)
  (defun hoagie-shr-link-open-or-kill (&optional arg)
    "Edit and open the link at point in EWW.
With prefig ARG, put it in the kill ring instead."
    (interactive "P")
    (when-let ((target-url (get-text-property (point) 'shr-url)))
      (message "Link: %s" target-url)
      (if arg
          (kill-new target-url)
        (eww (read-string "" target-url)))))
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

;; connections defined in qontigo-init.el
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
        ("f" . hoagie-vc-git-fetch-all)
        ("e" . vc-ediff)
        ("k" . vc-revert)
        ("r" . hoagie-vc-dir-reset)
        ("b b" . hoagie-vc-git-show-branches))
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
  :config
  (defvar hoagie-vc-git-emails
    '("code@sebasmonia.com"
      "some.work@email.com :)")
    "List of email addresses that can be associated with a repository")
  (defun hoagie-vc-git-fetch-all ()
    "Run \"git fetch --all\" in the current repo.
No validations, so better be in a git repo when calling this :)."
    (interactive)
    (vc-git-command nil 0 nil "fetch" "--all")
    (message "Completed \"fetch --all\" for current repo."))
  (defun hoagie-vc-git-clone (repository-url local-dir)
    "Run \"git clone REPOSITORY-URL\" to LOCAL-DIR.
It also prompts what email to use in the directory, from the
values in `hoagie-vc-git-emails'.
Executes `vc-dir' in the newly cloned directory."
    (interactive
     (let* ((url (read-string "Repository URL: "))
            (dir (file-name-base url)))
       ;; docs say not to use "initial-input", but it does what I want...
       ;; and the other "default-value" doesn't.
       (list url (read-string "Target directory: " dir))))
    (vc-git-command nil 0 nil "clone" repository-url local-dir)
    (let ((default-directory (file-name-concat default-directory local-dir)))
      (vc-git-command nil 0 nil "config" "user.email"
                      (completing-read "Email for this repo: "
                                       hoagie-vc-git-emails))
      (vc-dir default-directory)))
  (defun hoagie-vc-git-show-branches (&optional arg)
    "Show in a buffer the list of branches in the current repository.
With prefix ARG show the remote branches."
    (interactive "P")
    ;; TODO: this is a mix of vc-git stuff and project.el stuff...
    (let* ((default-directory (project-root (project-current t)))
           (buffer-name (project-prefixed-buffer-name (if arg
                                                          "git remote branches"
                                                        "git local branches"))))
      (vc-git-command buffer-name
                      0
                      nil
                      "branch"
                      (when arg "-r"))
      (pop-to-buffer buffer-name)
      (goto-char (point-min))
      (special-mode))))

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
        ;; vc-dir-find-file, but I use project-find-file instead
        ("f" . hoagie-vc-git-fetch-all)
        ;; "l"ist is used for branch-log, use "b"ranches
        ("b b" . hoagie-vc-git-show-branches)
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
  ;; Inspired by https://demonastery.org/2013/04/emacs-narrow-to-region-indirect/
  ;; and modified to DWIM. Also use `pop-to-buffer' instead of `switch-to-buffer'
  (defun hoagie-clone-indirect-dwim (&optional arg)
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
  (defun hoagie-kill-buffer (&optional prefix-arg)
    "Kill the current buffer, with PREFIX-ARG defer to `kill-buffer'."
    (interactive "P")
    (if current-prefix-arg
        (call-interactively #'kill-buffer)
      (kill-buffer (current-buffer))))
  :bind
  ("<S-f1>" . (lambda () (interactive) (find-file user-init-file)))
  ("<f1>" . hoagie-go-home)
  ("<f2>" . project-switch-project)
  ;; from https://stackoverflow.com/a/6465415
  ("C-x 3" . (lambda () (interactive)(split-window-right) (other-window 1)))
  ("C-x 2" . (lambda () (interactive)(split-window-below) (other-window 1)))
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
  ("C-x k" . hoagie-kill-buffer)
  ;; it's back...
  ("<remap> <list-buffers>" . ibuffer)
  (:map hoagie-keymap
        ;; this is much easier to type than C-S-backspace
        ;; and mirrors C-k nicely.
        ;; C-k kill rest of the line
        ;; <f6>-k kill the whole thing
        ;; (F6 and C are next to each other in the Raise)
        ("k" . kill-whole-line)
        ("u" . delete-pair))
  (:map hoagie-second-keymap
        ;; Used to be C-x n i (narrow indirect) with the enhancement
        ;; to narrow to defun, it gets a new and shorter binding
        ("c" . hoagie-clone-indirect-dwim))
  (:map ctl-x-map
        ;; right next to other-window
        ("i" . other-frame)
        ("ESC i" . make-frame)
        ;; add shift to get the original command for C-x i...
        ;; ...although I never used it
        ("I" . insert-file))
  :custom
  ;; experimental, I don't think I have a need for lockfiles...
  (create-lockfiles nil)
  ;; from TRAMP's FAQ
  (remote-file-name-inhibit-locks t)
  (sentence-end-double-space nil)
  (tab-always-indent 'complete)
  (minibuffer-restore-windows nil) ;; finally...
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
   ";; Il semble que la perfection soit atteinte non quand il n’y a\n;; plus rien à ajouter, mais quand il n’y a plus à retrancher.\n;;                                   - Antoine de Saint Exupéry\n\n;; Misc:\n;; C-x C-k e edit kmacro    ;; (shell) C-c C-o clear last output\n;; C-x / vundo              ;; C-x C-t transpose-lines (0 arg!)\n\n;; During isearch           ;; Query replace\n;; C-w add watp, can repeat ;; C-u M-% to replace words\n;; M-r toggle regex\n\n;; Newlines:\n;; C-o open-line            ;; C-M-o split-line\n;; M-^ join with prev line\n\n;; M-x...\n;; copy-matching-lines (also kill-) ;; (un)highlight-regexp\n;; align-current (or align-regexp)\n\n;; Calendar & Diary\n;; . - go to today          ;; u/m/x - unmark/mark events/holidays\n;; M-= count days region\n\n;; Notes prefix <f3> => 3 inbox / n new / g grep / f find by name\n\n")
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
        compilation-error-regexp-alist
        (delete 'maven compilation-error-regexp-alist))
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
          `((".*" . ,backup-dir))))
  ;; Identify the toolbox container for this Emacs instance in the frame title
  (setf frame-title-format '(" %b @ " (:eval hoagie-toolbox-name))
        icon-title-format '(" %b @ " (:eval hoagie-toolbox-name))))

;; Per-OS configuration

(when (eq system-type 'windows-nt)
  (require 'ls-lisp)
  (customize-set-value 'ls-lisp-use-insert-directory-program nil)

  (set-fontset-font t 'emoji (font-spec :family "Segoe UI Emoji"))

  (defun hoagie-manually-adjust-font-size ()
    "Something fishy is going on with font sizes...set them manually for now."
    (interactive)
    ;; nil for "all frames"
    (set-face-attribute 'default nil
                        :height (if (= (face-attribute 'default :height) 120)
                                    181
                                  120)))
  (global-set-key (kbd "<f7>") #'hoagie-manually-adjust-font-size))

(when (eq system-type 'gnu/linux)
  (when (display-graphic-p)
    (set-fontset-font t 'emoji (font-spec :family "Noto Emoji")))

  (defun hoagie-adjust-font-size (frame)
    "Inspired by https://emacs.stackexchange.com/a/44930/17066.
FRAME is ignored."
    ;; 2021-05-22: now I use the pgtk branch everywhere, and the monitor name
    ;; has a meaningul value in all cases, so:
    (let* ((monitor-name (alist-get 'name (frame-monitor-attributes)))
           (monitor-font '(("0x0536" . 128) ;; laptop -- maybe 143
                           ("LG Ultra HD" . 151))) ;; monitor -- maybe 158
           (size (alist-get monitor-name monitor-font
                            180 ;; default size, "big just in case"
                            nil
                            'equal)))
      (set-face-attribute 'default (selected-frame) :height size)))
  (add-hook 'window-size-change-functions #'hoagie-adjust-font-size))

(use-package modus-themes
  :demand t
  :config
  (load-theme 'modus-operandi t)
  :custom
  (modus-themes-completions (quote ((matches . (underline))
                                    (selection . (bold intense))))))

;; Almost tempted to make it a package. But given that I _always_ load this,
;; in a normal init, a simple `load-file' will suffice.
(load-file "~/sourcehut/dotfiles/.config/emacs/hoagie-modeline.el")

;; let's do our best to keep Gnus files/dir outside of ~
(load-file "~/sourcehut/dotfiles/.config/gnus/.gnus.el")

;;; init.el ends here
