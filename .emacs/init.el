;;; .emacs --- My dot emacs file  -*- lexical-binding: t; -*-

;; Author: Sebastian Monia <code@sebasmonia.com>
;; URL: https://git.sr.ht/~sebasmonia/dotfiles
;; Version: 29.9
;; Keywords: tools maint

;; This file is not part of GNU Emacs.

;;; Commentary:

;; My dot Emacs file
;; Recent changes:
;; 2023-08-09: Update config to use PGP for authinfo and sending emails, minor
;;             improvements on Gnus and message modes, add emoji font (for some
;;             Gemini sites, this is a must!)
;; 2023-08-25: Manage email contacts with ecomplete
;; 2023-08-31: Notifications and appointments setup
;; 2023-10-09: Moved unused languages/tools to purgatory.el

;;; Code:

(setf custom-file (locate-user-emacs-file "custom.el"))

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
;; (setf use-package-compute-statistics t)

(when (eq window-system 'pgtk)
  (pgtk-use-im-context t))

(setf comp-deferred-compilation t
      warning-minimum-level :error)

(require 'use-package)
(setf use-package-verbose t)
;; turns out I use more built ins than externals...
;; let's see how this change goes :)
;; (setf use-package-always-ensure t)
(setf use-package-hook-name-suffix nil)
(setf package-native-compile t)
(custom-set-faces
 '(default ((t (:family "Iosevka Comfy Wide Fixed"
                        :slant
                        normal
                        :weight
                        regular
                        :height
                        160
                        :width
                        normal)))))
(set-fontset-font t 'emoji (font-spec :family "Noto Emoji"))

;; based on http://www.ergoemacs.org/emacs/emacs_menu_app_keys.html
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
(define-key key-translation-map (kbd "<f7>") (kbd "C-x")) ;; CapsLock
;; This one is for the benefit of the laptop keyboard, which
;; I'm barely using nowadays, TBH
(define-key key-translation-map (kbd "<f8>") (kbd "C-c"))

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
  ;; (appt-display-mode-line nil)
  ;; next two are default values - might not need to customize
  (appt-display-interval 5)
  (appt-message-warning-time 15)
  :config
  (appt-activate)
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
                            :urgency 'normal))))

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

(use-package csharp-mode
  :ensure t
  :mode "\\.cs$"
  :hook
  (csharp-mode-hook . subword-mode))

(use-package dabbrev
  :custom
  (dabbrev-case-distinction nil)
  (dabbrev-case-fold-search t)
  (dabbrev-case-replace nil)
  :bind
  ("C-;" . dabbrev-expand))

(use-package calendar
  :demand t
  :custom
  (calendar-date-style 'iso)
  (calendar-view-diary-initially-flag t)
  ;; show events for the next 7 days when the calendar opens
  (diary-number-of-entries 7)
  :hook
  (calendar-today-visible-hook . calendar-mark-today)
  (calendar-mode-hook . diary-mark-entries))

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

(use-package cdsync :load-path "~/sourcehut/caldav-sync.el"
  :demand t
  :custom
  (cdsync-auth-source-host "caldav-fastmail")
  :commands
  (cdsync-open-diary cdsync-track-calendar cdsync-list-calendars)
  :config
  (cdsync-setup-calendar-integration))

(use-package dired
  :custom
  (dired-listing-switches "-laogGhvD")
  (dired-compress-file-suffixes
        '(("\\.tar\\.gz\\'" #1="" "7za x -aoa -o%o %i")
          ("\\.tgz\\'" #1# "7za x -aoa -o%o %i")
          ("\\.zip\\'" #1# "7za x -aoa -o%o %i")
          ("\\.7z\\'" #1# "7za x -aoa -o%o %i")
          ("\\.tar\\'" ".tgz" nil)
          (":" ".tar.gz" "tar -cf- %i | gzip -c9 > %o")))
  (dired-compress-directory-default-suffix ".7z")
  (dired-compress-file-default-suffix ".7z")
  (dired-compress-files-alist
        '(("\\.7z\\'" . "7za a -r %o %i")
          ("\\.zip\\'" . "7za a -r %o  %i")))
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
        ("C-<return>" . hoagie-dired-open-file))
  :hook
  (dired-mode-hook . dired-hide-details-mode)
  ;; Enables "C-c C-m a" (yeah, really :) lol) to attach
  ;; all files marked in dired to the current/a new email
  (dired-mode-hook . turn-on-gnus-dired-mode)
  :config
  (defun hoagie-dired-open-file ()
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
    "An advice function that gives xref-find-definitions a unique
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
  (eldoc-documentation-function 'eldoc-documentation-compose-eagerly)
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
  (defun hoagie-elisp-eldoc-value (callback &rest _)
    "Show the value variable at point by calling CALLBACK.
One portion of `elisp-eldoc-var-docstring-with-value', it only prints
the value, docstrings are handled by `hoagie-elisp-eldoc-fulldoc'."
    (when-let ((cs (elisp--current-symbol)))
      (when (and (boundp cs)
                 (not (null cs))
                 (not (eq cs t)))
        (funcall callback
                 (format "Variable value:\n%s"
		                 (prin1-to-string (symbol-value cs))))
        :thing cs
        :face 'font-lock-variable-name-face)))
  (defun hoagie-elisp-eldoc-fulldoc (callback &rest _)
    "Show the complete docs for the symbol at point by calling CALLBACK.
Based on `elisp-eldoc-var-docstring-with-value' and the SO post at
https://emacs.stackexchange.com/a/55914."
    (when-let ((cs (elisp--current-symbol)))
      (funcall callback
               (format "(full doc)\n%s"
                       (if (and (boundp cs)
                                (not (null cs))
                                (not (eq cs t)))
                           (or (documentation-property cs
                                                       'variable-documentation
                                                       t)
                               "Undocumented")
                         (condition-case nil
                             (documentation cs t)
                           ((error "Possibly undefined?")))))
               :thing cs
               :face 'font-lock-variable-name-face)))
  (defun hoagie-elisp-mode-setup ()
    "Setup my `emacs-lisp-mode' configuration.
Add hooks for `eldoc' customizations and set `fill-column'."
    ;; add last to the hook, the value for variables and the complete docstring
    ;; for variables and functions. This is displayed when invoking
    ;; `hoagie-toggle-eldoc-buffer', and good to have samples of how to add
    ;; more eldoc sources :D
    (add-hook 'eldoc-documentation-functions #'hoagie-elisp-eldoc-value 90 t)
    (add-hook 'eldoc-documentation-functions #'hoagie-elisp-eldoc-fulldoc 91 t)
    (setf fill-column 79)
    (display-fill-column-indicator-mode)))

(use-package elpher
  :ensure t
  :custom
  ;; from gemini://sporiff.dev/2023/08/08/elpher/
  ;; so Station works with Elpher :)
  (elpher-certificate-map '(("gemini://station.martinrue.com/" "sebhoagie")))
  :bind
  (:map hoagie-second-keymap
        ;; "g" for Gemini
        ("g" . elpher))
  ;; match eww/help bindings
  (:map elpher-mode-map
        ;; go to previous page
        ("l" . elpher-back)
        ;; TODO: no match for "next"?
        ;; rebind O to u, which makes
        ;; more sense to me :shrug:
        ("u" . elpher-root-dir)))

(use-package epg-config
  :custom
  (epg-pinentry-mode 'loopback))

(use-package eshell
  :custom
  ;; TODO: check all customizations
  (eshell-prefer-lisp-functions nil)
  (eshell-prefer-lisp-variables nil)
  :bind
  (:map hoagie-keymap
        ("`" . hoagie-eshell-here))
  ;; experimental - better on separate halves of the keyboard
  (:map hoagie-second-keymap
        ("`" . hoagie-eshell-here))
  :config
  (defun hoagie-eshell-here (&optional arg)
    "Pop eshell and switch to `default-directory' when the command was invoked.
With prefix arg, create a new instance even if there was one running."
    (interactive "P")
    ;; pass-through of the argument, will return an existing eshell or
    ;; create a new one
    (let ((new-dir default-directory))
      (with-current-buffer (eshell arg)
        (goto-char (point-max))
        (eshell/cd new-dir)
        (eshell-send-input "")))))

(use-package eww
  :demand t
  :custom
  (eww-auto-rename-buffer #'hoagie-eww-rename-buffer)
  :hook
  (eww-mode-hook . toggle-word-wrap)
  (eww-mode-hook . visual-line-mode)
  :bind
  (:map hoagie-second-keymap
        ;; already have "g" for Gemini on this keymap
        ;; might as well use "w" for web.
        ("w" . eww))
  (:map eww-mode-map
        ("m" . hoagie-eww-jump)
        ("o" . eww)
        ("O" . eww-browse-with-external-browser))
  :config
  (defun hoagie-eww-rename-buffer ()
    "Rename EWW buffers like \"title\", but put title last.
Function based on the same from the docstring for `eww-auto-rename-buffer'."
    (when (eq major-mode 'eww-mode)
      (when-let ((string (or (plist-get eww-data :title)
                             (plist-get eww-data :url))))
        (format "*eww: %s*" string))))
  ;; from https://emacs.stackexchange.com/a/36287 I want this for a
  ;; function to open the gcloud docs but I think it is useful as a
  ;; general tool to have around
  (defun hoagie-eww-readable (url &optional new-buffer)
    "Open URL, after the page loads, call `eww-readable'.
Optional argument NEW-BUFFER is passed to `eww' as prefix arg."
    ;;TIL letrec, too
    (letrec ((nonce (lambda ()
                      (unwind-protect
                          (eww-readable)
                        (remove-hook 'eww-after-render-hook nonce)))))
      (add-hook 'eww-after-render-hook nonce))
    (eww url new-buffer))
  (defun hoagie-eww-jump ()
    "Like `elpher-jump', but for EWW.
Based on the elpher code."
    (interactive)
    (let ((all-links (hoagie--collect-shr-links)))
      (eww (alist-get (completing-read "Link: "
                                       all-links nil)
                      all-links
                      nil nil #'string=)))))


;; TODO: as I write less C#, should I stil keep this around?
;; haven't used it much lately
(use-package fill-function-arguments
  :ensure t
  :commands (fill-function-arguments-dwim)
  :custom
  (fill-function-arguments-indent-after-fill t)
  :hook
  (prog-mode-hook . (lambda () (local-set-key (kbd "M-q") #'fill-function-arguments-dwim)))
  (sgml-mode-hook . (lambda ()
                      (setq-local fill-function-arguments-first-argument-same-line t)
                      (setq-local fill-function-arguments-argument-sep " ")
                      (local-set-key (kbd "M-q") #'fill-function-arguments-dwim)))
  ;; both emacs-lisp-mode and lisp-mode inherit from lisp-data-mode, and I want the same
  ;; rules for both
  (lisp-data-mode-hook . (lambda ()
                           (setq-local fill-function-arguments-first-argument-same-line t)
                           (setq-local fill-function-arguments-second-argument-same-line t)
                           (setq-local fill-function-arguments-last-argument-same-line t)
                           (setq-local fill-function-arguments-argument-separator " ")
                           (local-set-key (kbd "M-q") #'fill-function-arguments-dwim)))
  (csharp-mode-hook . (lambda ()
                        (setq-local fill-function-arguments-first-argument-same-line t)
                        (setq-local fill-function-arguments-second-argument-same-line nil)
                        (setq-local fill-function-arguments-last-argument-same-line t)
                        ;; override LSP indentation for this particular command
                        (setq-local fill-function-arguments-indent-after-fill
                                    (lambda (start end)
                                      (let ((indent-region-function #'c-indent-region))
                                        (indent-region start end)))))))

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

(use-package gnus
  :bind
  (:map hoagie-second-keymap
        ;; for "email"
        ("e" . gnus))
  (:map gnus-summary-mode-map
        ("v t" . hoagie-summary-trash)
        ("v a" . hoagie-summary-archive)
        ;; "b" for Basura. Using "s" is risky, as it is
        ;; next to "a"rchive :)
        ("v b" . hoagie-summary-spam)
        ("v g" . hoagie-summary-show-all))
  :init
  ;; let's do our best to keep Gnus files/dir outside of ~
  ;; some of these are not really Gnus vars, but declared in
  ;; nndraft, message, etc.
  (setq gnus-home-directory "~/.gnus.d/"
        gnus-directory "~/.gnus.d/News/"
        nndraft-directory "~/.gnus.d/News/drafts/"
        nnmh-directory "~/.gnus.d/News/drafts/"
        nnfolder-directory "~/.gnus.d/Mail/archive"
        nnml-directory "~/.gnus.d/Mail/"
        message-directory "~/.gnus.d/Mail/"
        gnus-article-save-directory "~/.gnus.d/News/"
        gnus-read-newsrc-file nil
        gnus-save-newsrc-file nil
        ;; the two values above mean I don't need this.
        ;; But, just in case:
        gnus-startup-file "~/.gnus.d/.newsrc"
        gnus-dribble-directory "~/.gnus.d/"
        gnus-always-read-dribble-file t)
  :custom
  ;; these two are not really Gnus values, but a sensible place to set them
  (user-full-name "Sebastián Monía")
  (user-mail-address "sebastian@sebasmonia.com")
  (mml-secure-openpgp-signers '("A65927B22A60F72A53D77CD7EF7DAC84163D7A83"))
  (mml-secure-openpgp-encrypt-to-self t)
  (gnus-select-method '(nnnil ""))
  (gnus-secondary-select-methods '((nnimap "fastmail"
                                     (nnimap-address "imap.fastmail.com")
                                     (nnimap-server-port 993)
                                     (nnimap-stream ssl)
                                     (nnir-search-engine imap))
                                   (nntp "feedbase"
                                     ;; feedbase does not do STARTTLS (yet?)
                                     (nntp-open-connection-function nntp-open-tls-stream)
                                     (nntp-port-number 563) ; nntps
                                     (nntp-address "feedbase.org"))
                                   (nntp "gmane"
                                     (nntp-open-connection-function nntp-open-plain-stream)
                                     (nntp-address "news.gmane.io"))))
   ;; Archive outgoing email in Sent folder, mark it as read
  (gnus-message-archive-method '(nnimap "imap.fastmail.com"))
  (gnus-message-archive-group "nnimap+fastmail:Sent")
  (gnus-gcc-mark-as-read t)
  (gnus-search-use-parsed-queries t)
  (gnus-auto-select-next nil)
  (gnus-paging-select-next nil)
  (gnus-summary-stop-at-end-of-message t)
  ;; http://groups.google.com/group/gnu.emacs.gnus/browse_thread/thread/a673a74356e7141f
  (gnus-summary-line-format (concat
                             "%0{%U%R%z%}"
                             "%3{│%}%1{%&user-date;%}%3{│%}" ;; date
                             " "
                             "%4{%-25,25f%}" ;; name
                             " "
                             "%3{│%}"
                             " "
                             "%1{%B%}"
                             "%s\n"))
  (gnus-user-date-format-alist '((t . "%Y-%m-%d (%a) %H:%M")
                                 gnus-thread-sort-functions '(gnus-thread-sort-by-date)
                                 ))
  (gnus-user-date-format-alist '((t . "%Y-%m-%d %I:%M%p")))
  (gnus-thread-sort-functions '(gnus-thread-sort-by-date))
  ;; let's experiment disabling threads...
  (gnus-show-threads nil)
  (gnus-sum-thread-tree-false-root "")
  (gnus-sum-thread-tree-root "")
  (gnus-sum-thread-tree-indent " ")
  (gnus-sum-thread-tree-vertical        "│")
  (gnus-sum-thread-tree-leaf-with-other "├─► ")
  (gnus-sum-thread-tree-single-leaf     "╰─► ")
  :config
  ;; Inspired by https://github.com/kensanata/ggg, add shortcuts to
  ;; Archive, Trash and Spam, but bind to keymap directly
  (defun hoagie-summary-spam ()
    "Move the current or marked mails to Spam in Fastmail."
    (interactive)
    (gnus-summary-move-article nil "nnimap+fastmail:Spam"))
  (defun hoagie-summary-archive ()
    "Move the current or marked mails to the Archive in Fastmail."
    (interactive)
    (gnus-summary-move-article nil "nnimap+fastmail:Archive"))
  (defun hoagie-summary-trash ()
    "Move the current or marked mails to Trash in Fastmail."
    (interactive)
    (gnus-summary-move-article nil "nnimap+fastmail:Trash"))
  (defun hoagie-summary-show-all ()
    "Show all messages, even the ones read.
This is C-u M-g but I figured I would put it in a simpler binding."
    (interactive)
    (gnus-summary-rescan-group t))
  )

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

(defvar hoagie-howm-keymap
  (define-prefix-command 'hoagie-howm-keymap)
  "Custom bindings for `howm-mode'.")
(use-package howm
  :demand t
  :bind-keymap
  ("<f3>" . hoagie-howm-keymap)
  :bind
  (:map hoagie-keymap
        ("3" . hoagie-howm-inbox)
        ("C-3" . howm-list-todo)
        ("ESC 3" . howm-list-schedule))
  (:map hoagie-howm-keymap
        ("c" . howm-create)
        ("3" . howm-menu)
        ("s" . howm-list-grep-fixed)
        ("t" . howm-insert-date))
  :config
  (setf howm-file-name-format "%Y/%m/%Y-%m-%d-%H%M%S.md")
  ;; https://leahneukirchen.org/blog/archive/2022/03/note-taking-in-emacs-with-howm.html
  ;; keep C-h for help in howm modes
  (define-key howm-menu-mode-map "\C-h" nil)
  (define-key riffle-summary-mode-map "\C-h" nil)
  (define-key howm-view-contents-mode-map "\C-h" nil)
  (defun hoagie-howm-inbox ()
    (interactive)
    (find-file "~/howm/tasks.md")
    (howm-set-mode)
    (goto-char (point-max))))

(use-package icomplete
  :demand t
  :custom
  (icomplete-hide-common-prefix nil)
  (icomplete-show-matches-on-no-input t)
  (icomplete-prospects-height 15)
  :config
  (fido-vertical-mode t)
  (defun hoagie-icomplete-styles ()
    (setq-local completion-styles '(substring partial-completion flex)))
  ;; experimental, override style so there isn't only flex, see
  ;; https://www.reddit.com/r/emacs/comments/16f2t3u/comment/k013bk8/
  :hook
  (icomplete-minibuffer-setup-hook . hoagie-icomplete-styles)
  :bind
  (:map icomplete-minibuffer-map
        ;; when there's no exact match, accept the first one
        ;; under cursor with RET
        ("RET" . icomplete-force-complete-and-exit)
        ;; C-j to force-accept current input even if it's not
        ;; in the candidate list
        ("C-j" . icomplete-fido-exit)))

(use-package imenu
  :demand t
  :bind
  ("M-i" . imenu))

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
  :hook
  (lisp-mode-hook . (lambda ()
                      (set (make-local-variable lisp-indent-function)
		                   'common-lisp-indent-function)
                      (setf fill-column 100)
                      (display-fill-column-indicator-mode))))

(use-package markdown-mode
  :ensure t
  :init
  (setq markdown-command "pandoc")
  :bind
  (:map markdown-mode-map
        ("C-c C-e" . markdown-do)))

(use-package message
  :bind
  (:map message-mode-map
        ;; shadows `message-elide-region' :shrug:
        ("C-c C-e" . hoagie-confirm-encrypt))
  :hook
  (message-setup-hook . hoagie-message-change-from)
  :custom
  ;; actually part of simple.el, but putting it here because
  ;; it is relevant to message.el behaviour for C-x m
  (mail-user-agent 'gnus-user-agent)
  ;; integrate with ecomplete. Press TAB to get email completion
  (message-mail-alias-type 'ecomplete)
  (message-self-insert-commands nil)
  (message-expand-name-standard-ui t)
  ;; This causes problems when Cc: is already present.
  ;; Need to either add a func to add a header, or internalize the
  ;; existing commands to "go to header" which add them
  ;; (message-default-mail-headers "Cc: \nBcc: \n")
  :config
  ;; From https://www.emacswiki.org/emacs/GnusTutorial#h5o-40
  (defvar hoagie-email-addresses '("sebastian@sebasmonia.com"
                                   "capsule@sebasmonia.com"
                                   "code@sebasmonia.com"
                                   "mailing@sebasmonia.com"
                                   "subscriptions@sebasmonia.com"
                                   "thingstopay@sebasmonia.com"
                                   "work@sebasmonia.com")
    "The list of aliases in my email setup.")
  (defun hoagie-message-change-from ()
    "Select the \"From:\" address when composing a new email."
    (interactive)
    (let* ((selected-address (completing-read "From: " hoagie-email-addresses))
           (address (concat user-full-name " <" selected-address ">"))
           (from-text (concat "From: " address)))
      (setq gnus-article-current-point (point))
      (goto-char (point-min))
      (while (re-search-forward "^From:.*$" nil t)
        (replace-match from-text))
      (goto-char gnus-article-current-point)))
  (defun hoagie-confirm-encrypt ()
    "Answer y/n to whether to send the message encrypted."
    (interactive)
    (when (y-or-n-p "Encrypt message?")
      (mml-secure-message-encrypt))))

(use-package minibuffer
  :demand t
  :custom
  (completions-format 'one-column)
  (completions-max-height 20)
  (completion-styles '(substring partial-completion flex))
  ;; default: (basic partial-completion emacs22)
  ;; and it is overriden by fido-mode anyway? :shrug:
  (read-buffer-completion-ignore-case t)
  (read-file-name-completion-ignore-case t)
  (completion-ignore-case t)
  (completions-detailed t)
  (completion-auto-help 'visible)
  (completion-auto-select 'second-tab)
  :bind
  (:map minibuffer-mode-map
        ("C-n" . minibuffer-next-completion)
        ("C-p" . minibuffer-previous-completion))
  (:map completion-in-region-mode-map
        ("RET" . minibuffer-choose-completion)
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
  (show-paren-style 'mixed)
  (show-paren-when-point-inside-paren t))

(use-package proced
  :custom
  (proced-filter 'all))

(use-package project
  :bind
  (:map hoagie-keymap
        ("g" . project-find-regexp)
        ("f" . project-find-file))
  :custom
  (project-vc-extra-root-markers '(".subproject"))
  ;; go back to old default of "d" for "dired at root"
  (project-switch-commands
   '((project-find-file "Find file" nil)
     (project-find-regexp "Find regexp" nil)
     (project-dired "Find directory" ?d)
     (project-vc-dir "VC-Dir" nil)
     (project-eshell "Eshell" nil)))
  :config
  (defun hoagie-project-multi-occur (regexp &optional nlines)
    "Run `multi-occur' in all the files in the current project."
    ;; very much inspired by https://github.com/NicolasPetton/noccur.el
    ;; By using `project-files' instead of "git ls", it works in subprojects
    (interactive (occur-read-primary-args))
    (let* ((the-project (project-current t))
           (default-directory (project-root the-project ))
           (files (mapcar #'find-file-noselect (project-files the-project))))
      (multi-occur files regexp nlines)))
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
        ("j" . hoagie-jump-to-register)
        ("4 j" . hoagie-jump-to-register-other-window))
  :config
  ;; BRITTLENESS WARNING: this re-defines a built-in method, there's
  ;; a high risk it breaks when moving Emacs versions
  (cl-defmethod register-val-describe ((val marker) _verbose)
    (let ((buf (marker-buffer val)))
      (if (null buf)
	      (princ "a marker in no buffer")
        (princ (hoagie--text-around-marker val))
        (princ " -- buffer ")
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
  (defun hoagie-jump-to-register-other-window (&optional arg)
    (interactive)
    "Variant of `hoagie-jump-to-register' that jumps in other window.
It also deletes the register if called with prefix ARG."
    (interactive "P")
    (let* ((register-alist (cl-loop for reg in register-alist
                                    when (markerp (cdr reg))
                                    collect reg))
           (reg (register-read-with-preview "Jump to: ")))
      (pop-to-buffer (current-buffer) t t)
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
        ("o" . hoagie-occur-sexp-or-region)
        ("ESC o" . multi-occur-in-matching-buffers))
  :hook
  (occur-hook . hoagie-rename-and-select-occur-buffer)
  :config
  (defun hoagie-occur-sexp-or-region ()
    "Run occur for the sexp at point, or the active region.
By default, occur _limits the search to the region_ if it is active."
    (interactive)
    (occur (if (use-region-p)
               (buffer-substring-no-properties (region-beginning)
                                               (region-end))
             (thing-at-point 'sexp t))
           (when current-prefix-arg
	         (prefix-numeric-value current-prefix-arg))))
  (defun hoagie-rename-and-select-occur-buffer ()
    "Renames the current buffer to *Occur: [term] [buffer]*.
Meant to be added to `occur-hook'."
    (cl-destructuring-bind (search-term _ (buffer-name &rest _))
        occur-revert-arguments
      (pop-to-buffer
       (rename-buffer (format "*Occur: %s %s*" search-term buffer-name) t)))))

(use-package savehist
  :custom
  (savehist-additional-variables '(kill-ring
                                   search-ring
                                   regexp-search-ring))
  (history-delete-duplicates t)
  :config
  (savehist-mode))

(use-package sharper :load-path "~/github/sharper"
  :bind
  (:map hoagie-keymap
        ;; using "n" for another command now, moving
        ;; this to "c" for C#
        ("c" . sharper-main-transient))
  :custom
  (sharper-run-only-one t))

(use-package shr
  :custom
  (shr-use-fonts t)
  (shr-use-colors t)
  (shr-bullet "• ")
  (shr-indentation 2)
  (shr-discard-aria-hidden t)
  :bind
  (:map hoagie-keymap
        ("ESC b" . hoagie-shr-kill-link-target))
  :config
  (defun hoagie-shr-kill-link-target ()
    "Kill (and show in echo area) the target of the link at point."
    (interactive)
    (let ((target-url (get-text-property (point) 'shr-url)))
      (when target-url
        (kill-new target-url)
        (message "URL: %s" target-url))))
  (defun hoagie--collect-shr-links ()
    "Get an alist of all link targets in the current buffer.
Inspired by a similar function in Elpher."
    (let ((link-template "%s (%s)"))
      (save-excursion
        (goto-char (point-min))
        (cl-loop for link = (text-property-search-forward 'shr-url nil nil t)
                 while link
                 for text = (buffer-substring (prop-match-beginning link)
                                              (prop-match-end link))
                 for target = (prop-match-value link)
                 collect
                 (cons (format link-template text target)
                       target))))))

(use-package sly
  :ensure t
  :commands sly
  :custom
  (inferior-lisp-program "sbcl --dynamic-space-size 10240"))

(use-package sly-quicklisp
  :ensure t
  :after sly)

;; Send email via Fastmail's SMTP:
(use-package smtpmail
  :custom
  (send-mail-function 'smtpmail-send-it)
  (smtpmail-default-smtp-server "smtp.fastmail.com")
  (smtpmail-stream-type  'starttls)
  (smtpmail-smtp-service 587))

(use-package sql
  :hook
  (sql-interactive-mode-hook . (lambda () (setf truncate-lines t))))

(use-package sql-datum :load-path "~/github/datum"
  :after sql
  :custom
  (sql-datum-program "/var/home/hoagie/.local/bin/datum.sh")
  :config
  (setf sql-connection-alist nil)
  (add-to-list 'sql-connection-alist
               '("Container - user SA"
                 (sql-product 'datum)
                 (sql-server "127.0.0.1,1433")
                 (sql-user "sa")
                 (sql-password "LeSecurity!1")
                 (sql-datum-options '("--driver" "FreeTDS" ))
                 (sql-database "tp11")))
  (add-to-list 'sql-connection-alist
               '("Container - user TP11"
                 (sql-product 'datum)
                 (sql-server "127.0.0.1,1433")
                 (sql-user "tp_ProgAvanzada")
                 (sql-password "H0l4Mund0")
                 (sql-datum-options '("--driver" "FreeTDS" ))
                 (sql-database "tp11")))
  (add-to-list 'sql-connection-alist
               '("Chinook"
                 (sql-product 'datum)
                 (sql-server "")
                 (sql-user "")
                 (sql-password "")
                 (sql-datum-options '("--driver" "SQLITE3" ))
                 (sql-database "/var/home/hoagie/github/datum/chinook.db"))))

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
  (defun hoagie-notify-timer (message minutes)
    "Show MESSAGE after MINUTES, in a desktop notification."
    (interactive "sMessage (default: \"Timer Elapsed\"): \nnMinutes: ")
    (when (string-empty-p message)
      (setf message "Timer elapsed"))
    (run-at-time (* 60 minutes) nil #'notifications-notify
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
        ("d" . hoagie-vc-dir-delete)
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
    (vc-dir-refresh))
  (defun hoagie-vc-dir-delete ()
    "Delete files directly in the vc-dir buffer."
    (interactive)
    ;; idea from https://stackoverflow.com/a/29504426/91877 but much
    ;; simplified (many cases I really don't need) and getting the
    ;; list of files using `vc-deduce-fileset'
    (let ((files (cl-second (vc-deduce-fileset))))
      (when (and files
                 (yes-or-no-p (format "Delete %s? "
                                      (string-join files ", " ))))
        (unwind-protect
            (mapcar (lambda (path) (delete-file path t)) files)
          (revert-buffer))))))

(use-package vc-git
  :after vc
  :custom
  (vc-git-revision-complete-only-branches t)
  :config
  (defun hoagie-vc-git-fetch-all ()
    "Run \"git fetch --all\" in the current repo.
No validations, so better be in a git repo when calling this :)."
    (interactive)
    (vc-git-command nil 0 nil "fetch" "--all")
    (message "Completed \"fetch --all\" for current repo."))
  (defun hoagie-vc-git-clone (repository-url directory)
    "Run \"git clone REPOSITORY-URL\" into DIRECTORY."
    (interactive "sRepository URL: \nsTarget directory (empty for default): ")
    (when (string= directory "")
      ;; "clone" needs nil to use the default directory
      (setf directory nil))
    (vc-git-command nil 0 nil "clone" repository-url directory)
    (message "Repository cloned!"))
  (defun hoagie-vc-git-show-branches (&optional arg)
    "Show in a buffer the list of branches in the current repository.
With prefix ARG show the remote branches."
    (interactive "P")
    ;; TODO: this is a mix of vc-git stuff and project.el stuff...
    (let* ((default-directory (project-root (project-current t)))
           (buffer-name (project-prefixed-buffer-name (if arg
                                                          "git remote branches"
                                                        "git branches"))))
      (vc-git-command buffer-name
                      0
                      nil
                      "branch"
                      (when arg "-r"))
      (pop-to-buffer buffer-name)
      (goto-char (point-min)))))

(use-package vc-hooks
  :after (vc vc-git)
  :custom
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
  (defun hoagie-store-window-configuration ()
    (interactive)
    (setf hoagie-window-configuration (current-window-configuration)))
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
    (hoagie-store-window-configuration)
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
    (interactive "P")
    (if arg
        (dired-other-window "~/")
      (dired "~/")))
  ;; from https://www.emacswiki.org/emacs/BackwardDeleteWord because I
  ;; agree C-backspace shouldn't kill the word! It litters my kill ring
  (defun delete-word (arg)
    "Delete characters forward until encountering the end of a word.
With ARG, do this that many times."
    (interactive "p")
    (if (use-region-p)
        (delete-region (region-beginning) (region-end))
      (delete-region (point) (progn
                               (forward-word arg)
                               (point)))))
  (defun backward-delete-word (arg)
    "Delete characters backward until encountering the end of a word.
With ARG, do this that many times."
    (interactive "p")
    (delete-word (- arg)))
  ;; Inspired by https://demonastery.org/2013/04/emacs-narrow-to-region-indirect/
  ;; and modified to DWIM. Also use `pop-to-buffer' instead of `switch-to-buffer'
  (defun hoagie-clone-indirect-dwim (&optional arg)
    "Create an indirect buffer, narrow it to defun or active region.
If ARG, don't narrow."
    (interactive "P")
    (unless (or (use-region-p)
                arg)
      (mark-defun))
    (let ((start (use-region-beginning))
          (end (use-region-end))
          ;; we'll pop the buffer manually
          ;; to clear the region
          (buf (clone-indirect-buffer nil nil)))
      (deactivate-mark)
      (with-current-buffer buf
        (when (and start end)
          (narrow-to-region start end)
          (deactivate-mark))
        (pop-to-buffer buf))))
  (defun hoagie-jump-to-char (arg char &optional interactive)
    "A copy of `zap-to-char' that doesn't kill the text."
    (interactive (list (prefix-numeric-value current-prefix-arg)
		               (read-char-from-minibuffer "Jump to char: "
						                          nil 'read-char-history)
                       t))
    (let ((direction (if (>= arg 0) 1 -1))
          (case-fold-search (if (and interactive (char-uppercase-p char))
                                nil
                              case-fold-search)))
      (goto-char
	   (progn
		 (forward-char direction)
		 (unwind-protect
		     (search-forward (char-to-string char) nil nil arg)
		   (backward-char direction))
		 (point)))))
  (defun hoagie-kill-buffer (&optional prefix-arg)
    "Kill the current buffer, with PREFIX-ARG defer to `kill-buffer'."
    (interactive "P")
    (if current-prefix-arg
        (call-interactively #'kill-buffer)
      (kill-buffer (current-buffer))))
  (defun hoagie-region-escape-quotes (start end)
    "Escape quotes in the region.
It simply adds a \\ to each \" found."
    (interactive "r")
    (unless (use-region-p)
      (error "No active region"))
    ;; using replace in string vs replace in region because I need to
    ;; insert the literal replacement
    (let ((new-text
           (replace-regexp-in-string "\\(\"\\)"
                                     "\\\""
                                     (buffer-substring-no-properties start end)
                                     nil
                                     t)))
      (delete-region start end)
      (insert new-text)))
  (defvar hoagie-pair-chars
    '((?\" . ?\")
      (?\' . ?\')
      (?\` . ?\')
      (?\( . ?\))
      (?\[ . ?\])
      (?\{ . ?\}))
    "Alist of pairs to insert for `hoagie-insert-pair'.")
  (defun hoagie-insert-pair (start end)
    "Wrap the region or symbol at point in a pair from `hoagie-pair-chars'.
This is my own counterpart to `delete-pair' (which see). Emacs
has a built in mode for this, `electric-pair-mode', but it does
more than I want, it is more intrusive, and I couldn't get around
some of it's behaviours."
    (interactive "r")
    (unless (use-region-p)
      ;; if there's no active region, pick the bounds of the current symbol
      (cl-destructuring-bind (a . b)
          (bounds-of-thing-at-point 'symbol)
        (setf start a
              end b)))
    (let* ((opener (read-char "Opening char: "))
           (closer (alist-get opener hoagie-pair-chars)))
      ;; if the opener isn't from our list of chars, message and do nothing
      (if (not closer)
          (message "\"%c\" is not in the pair list" opener)
        (goto-char start)
        (insert opener)
        (goto-char (+ 1 end))
        (insert closer))))
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
  ("M-o" . other-window)
  ("M-O" . other-frame)
  ("M-`" . other-frame) ;; for Windows - behave like Gnome
  ;; from https://emacsredux.com/blog/2020/06/10/comment-commands-redux/
  ("<remap> <comment-dwim>" . comment-line)
  ;; replace delete-char, as recommended in the docs
  ("C-d" . delete-forward-char)
  ("C-<backspace>" . backward-delete-word)
  ("M-c" . capitalize-dwim)
  ("M-u" . upcase-dwim)
  ("M-l" . downcase-dwim)
  ("C-z" . hoagie-jump-to-char)
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
        ;; need to keep this one more present...
        ("u" . delete-pair)
        ("p" . hoagie-insert-pair))
  (:map hoagie-second-keymap
        ;; Used to be C-x n i (narrow indirect) with the enhancement
        ;; to narrow to defun, it gets a new and shorter binding
        ("c" . hoagie-clone-indirect-dwim)
        ("q" . hoagie-region-escape-quotes))
  :custom
  ;; experimental, I don't think I have a need for lockfiles...
  (create-lockfiles nil)
  (sentence-end-double-space nil)
  (tab-width 4) ;; make golang code nicer to read
  ;; TODO: so far I'm liking being explicit when I request completion
  ;; vs triggering when trying to indent. But it is an extra key... :shrug:
  ;; (tab-always-indent 'complete)
  (delete-pair-blink-delay 0.1)
  (recenter-positions '(1 middle -2)) ;; behaviour for C-l
  (comint-prompt-read-only t)
  (read-file-name-completion-ignore-case t) ;; useful in Linux
  ;; via https://github.com/jacmoe/emacs.d/blob/master/jacmoe.org
  (help-window-select t)
  ;; From https://github.com/wasamasa/dotemacs/blob/master/init.org
  (line-number-display-limit-width 10000)
  ;; tired of this question. Sorry not sorry
  (custom-safe-themes t)
  (indent-tabs-mode nil)
  (delete-by-moving-to-trash t)
  (global-mark-ring-max 64)
  (mark-ring-max 64)
  (inhibit-startup-screen t)
  (initial-buffer-choice t)
  (initial-scratch-message
   ";; Il semble que la perfection soit atteinte non quand il n’y a plus rien à ajouter, mais quand il n’y a plus à retrancher. - Antoine de Saint Exupéry\n;; It seems that perfection is attained not when there is nothing more to add, but when there is nothing more to remove.\n\n;; Marking:                             ;; Killing\n;; M-@ Next word C-M-<SPC> Next sexp    ;; C-M-k next sexp\n;; M-h Paragraph C-x h Buffer           ;; C-k rest of line\n;; C-M-h Next defun                     ;; <f6> k whole line\n\n;; Misc:                                   ;; (e)SHELL C-c then...\n;; <f6> i imenu C-x C-k e edit kmacro      ;; C-[p|n] prev/next input\n;; M-t Transpose word C-M-t Tranpose sexp  ;; C-o clear last output\n;; C-x / vundo\n\n;; During search\n;; C-w add word at point, repeat to add more\n;; M-r toggle regex\n\n;; Replace (regexp + elisp):\n;; \"movie\" -> \"film\" and \"movies\" -> \"films\":\n;; ‘movie(s)?‘ -> ‘,(if \\1 \"films\" \"film\")‘\n\n;; howm:\n;; <f6> 3 - inbox\n;; <f6> C-3 - show TODO\n;; <f6> ESC 3 - show scheduled\n;; <f3> howm keymap (hit twice for menu)\n\n;; REMEMBER: REGEXPS - INFO\n\n")
  (save-interprogram-paste-before-kill t)
  (visible-bell nil) ;; macOS change
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
                bidi-inhibit-bpa t)
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

;; Convenient to work with AWS timestamps
(defun hoagie-convert-timestamp (&optional timestamp)
"Convert a Unix TIMESTAMP (as string) to date.
If the parameter is not provided use word at point."
  (interactive (list (thing-at-point 'word t)))
  (let ((to-convert (if (< 10 (length timestamp))
                        (substring timestamp 0 10)
                      timestamp))
        (millis (if (< 10 (length timestamp))
                  "000")))
    (message "%s.%s"
             (format-time-string "%Y-%m-%d %H:%M:%S"
                                 (seconds-to-time
                                  (string-to-number to-convert)))
             millis)))
(define-key hoagie-keymap (kbd "t") #'hoagie-convert-timestamp)

;; Per-OS configuration

(when (string= system-type "windows-nt")
  (require 'ls-lisp)
  (customize-set-value 'ls-lisp-use-insert-directory-program nil))

(when (string= system-type "darwin")
  ;; use `ls` from coreutils, installed with homebrew
  (customize-set-value 'insert-directory-program "gls"))

(when (string= system-type "gnu/linux")
  (defun find-alternative-file-with-sudo ()
    (interactive)
    (let ((fname (or buffer-file-name
		     dired-directory)))
      (when fname
        (if (string-match "^/sudo:root@localhost:" fname)
	    (setf fname (replace-regexp-in-string
		         "^/sudo:root@localhost:" ""
		         fname))
	  (setf fname (concat "/sudo:root@localhost:" fname)))
        (find-alternate-file fname))))
  (global-set-key (kbd "C-x F") 'find-alternative-file-with-sudo)

  (require 'cl-lib)
  (defun hoagie-adjust-font-size (frame)
    "Inspired by https://emacs.stackexchange.com/a/44930/17066. FRAME is ignored."
    ;; 2021-05-22: now I use the pgtk branch everywhere, and the monitor name
    ;; has a meaningul value in all cases, so:
    (let* ((monitor-name (alist-get 'name (frame-monitor-attributes)))
           (monitor-font '(("0x0536" . 143) ;; laptop -- was 151
                           ("2757" . 120))) ;; external monitor
                         ;;("2757" . 128))) ;; external monitor
           (size (alist-get monitor-name monitor-font
                            180 ;; default size, "big just in case"
                            nil
                            'equal)))
      (set-face-attribute 'default (selected-frame) :height size)))
  (add-hook 'window-size-change-functions #'hoagie-adjust-font-size))

(use-package modus-themes
  :demand t
  :config
  (load-theme 'modus-operandi t))

(use-package mood-line
  :demand t
  :custom
  (mood-line-glyph-alist '((:checker-info . ?i)
                           (:checker-issues . ?!)
                           (:checker-good . ?+)
                           (:checker-checking . ?-)
                           (:checker-errored . ?x)
                           (:checker-interrupted . ?=)
                           (:vc-added . ?+)
                           (:vc-needs-merge . ?m)
                           (:vc-needs-update . ?u)
                           (:vc-conflict . ?c)
                           (:vc-good . ?-)
                           (:buffer-narrowed . ?n)
                           (:buffer-modified . ?!)
                           (:buffer-read-only . ?-)
                           (:count-separator . ?*)))
  :custom-face
  ;; same as the original one, but **make it bold**!
  (mood-line-buffer-status-modified ((t (:inherit (error) :weight bold))))
  :init
  (mood-line-mode)
  (defun mood-line-segment-cursor-position ()
    "Display the current cursor position.
This modified version shows the region size if applicable."
    (let ((region-size (when (use-region-p)
                         (propertize (format " (%sL:%sC)"
                                             (count-lines (region-beginning)
                                                          (region-end))
                                             (- (region-end) (region-beginning)))
                                     'face 'mood-line-unimportant)))
          (position (propertize " %p%% " 'face 'mood-line-unimportant)))
      (list "%l:%c" position region-size)))
  (defun mood-line-segment-buffer-status ()
    "Return an indicator for buffer status.
This version makes the narrowing indicator independent, and shows
modified only when the buffer isn't read-only.
The whole segment is decorated with `mood-line-buffer-status-modified'.
See https://gitlab.com/jessieh/mood-line/-/issues/20."
    (propertize (concat (if buffer-read-only
                            (mood-line--get-glyph :buffer-read-only)
                          ;; since it's not read-only, show the
                          ;; modified flag
                          (if (buffer-modified-p)
                            (mood-line--get-glyph :buffer-modified)
                            " "))
                        (if (buffer-narrowed-p)
                            (mood-line--get-glyph :buffer-narrowed)
                          " ")
                        " ")
                'face 'mood-line-buffer-status-modified))
  (defun mood-line-segment-misc-info ()
    "Display the current value of `mode-line-misc-info'.
This modified version adds a keyboard macro recording status."
    (let ((misc-info (concat (format-mode-line mode-line-misc-info 'mood-line-unimportant)
                             (when defining-kbd-macro
                               (format-mode-line mode-line-defining-kbd-macro
                                                 'mood-line-major-mode)))))
      (unless (string-blank-p (string-trim misc-info))
          (concat (string-trim misc-info) "  ")))))

;;; init.el ends here
