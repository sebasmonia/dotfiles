;; .emacs --- My dot emacs file  -*- lexical-binding: t; -*-

;; Author: Sebastian Monia <code@sebasmonia.com>
;; URL: https://git.sr.ht/~sebasmonia/dotfiles
;; Version: 29.5
;; Keywords: .emacs dotemacs

;; This file is not part of GNU Emacs.

;;; Commentary:

;; My dot Emacs file
;; In theory I should be able to just drop the file in any computer and have
;; the config synced without merging/adapting anything
;; 2019-05-06: V3 means I moved to use-package
;; 2020-06-14: Arbitrarily bumping the version number
;; 2021-09-02: Used https://www.manueluberti.eu/emacs/2021/09/01/package-report/
;;             to remove some dead packages and things I didn't use that much.
;; 2022-01-01: Make init file use lexical binding, update mark and point bindings.
;;             Bumping minor version (!) so 4.1 it is :)
;; 2022-04-06: Starting today, the major version of this file will match the minimum
;;             Emacs version targeted.  Since yesterday I added variables and settings
;;             that are new in Emacs 28, the new init version is 28.1 (I plan to start
;;             bumping the minor version more often, too).
;; 2022-06-09: Finished reading Mastering Emacs, added some notes and bindings
;;             Cleaned up some functions, removed some values.
;; 2022-12-01: Moved to depend on Emacs 29 (some customizations are 29-only)
;;             After a brief experiment with default "bare" completion, revisit
;;             my icomplete/minibuffer setup.
;; 2022-12-22: Hosting some of my personal repos in Source Hut, mirroring setting
;;             multiple remotes in "origin", see https://stackoverflow.com/a/58465641
;; 2023-02-18: Adding register bindings, jump-to-char, occur integration to project.el,
;;             new window size bindings, move back to Eglot again (better for
;;             remote/slow environments like VDIs...)
;; 2023-03-15: Rework my old push mark + visible-mark system into something based on
;;             registers and maybe advices. Revisit some bindings to take advantage
;;             of the Dygma Raise configuration.
;; 2023-05-18: Minor binding adjustments, occur w/region and sexp, remove unused or
;;             superseeded packages.
;; 2023-03-25: Adjust more bindings for the Raise, try _yet again_ switching away from
;;             company and icomplete/fido to "pure" built-in completion
;; 2023-05-18: Minor binding adjustments, occur w/region and sexp.
;; 2023-06-14: No company...yet again...
;; 2023-08-02: Some minor clean up. Add Gnus to handle my email
;;; Code:

(setf custom-file (expand-file-name (concat user-emacs-directory "custom.el")))

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
;; (setf use-package-compute-statistics t)

(when (eq window-system 'pgtk)
  (pgtk-use-im-context t))

;; Remember: emacs -Q then eval (native-compile-async "~/.emacs.d" 3 t)
;; Solves problem "(setf seq-elt) is already defined as something else than a generic function"
(setf comp-deferred-compilation t
      warning-minimum-level :error)

(require 'use-package)
(setf use-package-verbose t)
(setf use-package-always-ensure t)
(setf use-package-hook-name-suffix nil)
(setf package-native-compile t)
(custom-set-faces
 '(default ((t (:family "Iosevka Comfy Wide Fixed" :slant normal :weight regular :height 160 :width normal)))))
;; based on http://www.ergoemacs.org/emacs/emacs_menu_app_keys.html
(defvar hoagie-keymap (define-prefix-command 'hoagie-keymap) "My custom bindings.")
(defvar hoagie-second-keymap (define-prefix-command 'hoagie-second-keymap) "Originally a register keymap, now used for other stuff too.")
(define-key key-translation-map (kbd "<apps>") (kbd "<menu>")) ;; compat Linux-Windows
(define-key key-translation-map (kbd "<print>") (kbd "<menu>")) ;; curse you, thinkpad keyboard!!!
(global-set-key (kbd "<menu>") 'hoagie-keymap)

;; In case the config is not running on Silverblue, or if I ever layer Emacs on the base system...
(defvar hoagie-toolbox-name (if (file-exists-p "/run/.containerenv")
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
  :ensure nil
  :commands (ansi-color-apply-buffer)
  :init
  (defun ansi-color-apply-buffer ()
    "Colorize the entire buffer using `ansi-color-apply-on-region'."
    (interactive)
    (ansi-color-apply-on-region (point-min) (point-max))))

(use-package browse-url
  :ensure nil
  :custom
  (browse-url-secondary-browser-function #'hoagie-browse-url)
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
  :mode "\\.cs$"
  :hook
  (csharp-mode-hook . subword-mode))

(use-package dabbrev
  :ensure nil
  :custom
  (dabbrev-case-distinction nil)
  (dabbrev-case-fold-search t)
  (dabbrev-case-replace nil)
  :bind
  ("C-;" . dabbrev-expand))

(use-package dired
  :ensure nil
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
Initial version from EmacsWiki, added macOS & Silverblue toolbox support.
This could also use `w32-shell-execute' on Windows.
Also, the binding W `browse-url-of-dired-file' is a valid replacement, but not sure
about toolboxes..."
    (interactive)
    (let ((program-name (cond ((eq system-type 'darwin) "open")
                              ;; Used to use start "" {path}, but this one works too
                              ((eq system-type 'windows-nt) "explorer")
                              ;; For Linux, change based on toolbox vs non-toolbox
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
             ;; arguments to `call-process' + args for toolbox when required + target filename
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
        (message (format "Filename: %s" (or name "-No file for this buffer-")))))))

(use-package dired-narrow
  :after dired
  :bind
  (:map dired-mode-map
        ("/" . dired-narrow)))

(use-package docker
  :bind
  ("C-c d" . docker))

(use-package dockerfile-mode
  :mode "Dockerfile\\'")

(use-package ediff
  :ensure nil
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
                      (ediff-get-region-contents ediff-current-difference 'A ediff-control-buffer)
                      (ediff-get-region-contents ediff-current-difference 'B ediff-control-buffer))))
  (defun add-d-to-ediff-mode-map ()
    "Add key 'd' for 'copy both to C' functionality in ediff."
    (define-key ediff-mode-map "d" #'ediff-copy-both-to-C))
  ;; One minor annoyance of using ediff with built-in vc was the window config being altered, so:
  (defvar hoagie-pre-ediff-windows nil "Window configuration before starting ediff.")
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
  :ensure nil
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

(use-package elpher
  :ensure t
  :bind
  (:map hoagie-second-keymap
        ;; "g" for Gemini
        ("g" . elpher))
  ;; match eww/help bindings
  (:map elpher-mode-map
        ("l" . elpher-back)
        ;; no match for "next" yet?
        ))

(use-package eshell
  :ensure nil
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
    "Pops eshell and switches to `default-directory' when the command was invoked.
With prefix arg, create a new instance even if there was one running."
    (interactive "P")
    ;; pass-through of the argument, will return an existing eshell or create a new one
    (let ((new-dir default-directory))
      (with-current-buffer (eshell arg)
        (goto-char (point-max))
        (eshell/cd new-dir)
        (eshell-send-input "")))))

(use-package eww
  :ensure nil
  :demand t
  :custom
  (eww-auto-rename-buffer 'title)
  :hook
  (eww-mode-hook . toggle-word-wrap)
  (eww-mode-hook . visual-line-mode)
  :bind
  (:map eww-mode-map
        ("m" . hoagie-eww-jump)
        ("o" . eww)
        ("O" . eww-browse-with-external-browser))
  :config
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

(defvar hoagie-flymake-keymap (define-prefix-command 'hoagie-flymake-keymap) "Custom bindings for `flymake-mode'.")
(use-package flymake
  :ensure nil
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
  :ensure nil
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
        gnus-dribble-directory "~/.gnus.d/")
  :custom
  ;; these two are not really Gnus values, but a sensible place to set them
  (user-full-name "Sebastián Monía")
  (user-mail-address "sebastian@sebasmonia.com")
  (gnus-select-method '(nnnil ""))
  (gnus-secondary-select-methods '((nnimap "fastmail"
                                     (nnimap-address "imap.fastmail.com")
                                     (nnimap-server-port 993)
                                     (nnimap-stream ssl)
                                     (nnir-search-engine imap))
                                   (nntp "feedbase"
                                     (nntp-open-connection-function nntp-open-tls-stream) ; feedbase does not do STARTTLS (yet?)
                                     (nntp-port-number 563) ; nntps
                                     (nntp-address "feedbase.org"))
                                   (nntp "gmane"
                                     (nntp-open-connection-function nntp-open-plain-stream)
                                     (nntp-address "news.gmane.io"))))
  ;; see below for address listing
  (message-alternative-emails (regexp-opt hoagie-gnus-from-emails))
  ;; Archive outgoing email in Sent folder:
  (gnus-message-archive-method '(nnimap "fastmail"
                                (nnimap-address "imap.fastmail.com")
                                (nnimap-server-port 993)
                                (nnimap-stream ssl)
                                (nnir-search-engine imap)))
  (gnus-message-archive-group "Sent")
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
  (gnus-sum-thread-tree-false-root "")
  (gnus-sum-thread-tree-root "")
  (gnus-sum-thread-tree-indent " ")
  (gnus-sum-thread-tree-vertical        "│")
  (gnus-sum-thread-tree-leaf-with-other "├─► ")
  (gnus-sum-thread-tree-single-leaf     "╰─► ")
  :hook
  (message-setup-hook . hoagie-gnus-change-from)
  :config
  ;; From https://www.emacswiki.org/emacs/GnusTutorial#h5o-40
  (defvar hoagie-gnus-from-emails '("sebastian@sebasmonia.com"
                                    "capsule@sebasmonia.com"
                                    "code@sebasmonia.com"
                                    "mailing@sebasmonia.com"
                                    "subscriptions@sebasmonia.com"
                                    "thingstopay@sebasmonia.com"
                                    "work@sebasmonia.com")
    "The list of aliases in my email setup.")
  (defun hoagie-gnus-change-from ()
    (interactive)
    (let* ((selected-address (completing-read "From: " hoagie-gnus-from-emails))
           (from-text (concat "From: " user-full-name " <" selected-address ">")))
      (setq gnus-article-current-point (point))
      (goto-char (point-min))
      (while (re-search-forward "^From:.*$" nil t)
        (replace-match from-text))
      (goto-char gnus-article-current-point))))

(use-package go-mode
  :hook
  (go-mode-hook . subword-mode)
  :config
  (defun hoagie-golang-stdlib-reference ()
    "Open the Go stdlib reference in EWW."
    (interactive)
    (hoagie-eww-readable (format "https://pkg.go.dev/%s" (read-string "Package: ")))))

(use-package grep
  :ensure nil
  :custom
  (grep-command "grep --color=always -nHi -r --include=*.* -e \"pattern\" .")
  (grep-use-null-device nil)
  :bind
  (:map hoagie-keymap
        ("ESC g" . rgrep)))

(use-package hl-line
  :ensure nil
  :hook
  (after-init-hook . global-hl-line-mode))

(defvar hoagie-howm-keymap (define-prefix-command 'hoagie-howm-keymap) "Custom bindings for `howm-mode'.")
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
  :ensure nil
  :demand t
  :custom
  (icomplete-hide-common-prefix nil)
  (icomplete-show-matches-on-no-input t)
  (icomplete-prospects-height 15)
  :config
  ;; Non-custom configuration. Temporarily disabled, but could replace company...
  ;; (setf icomplete-in-buffer nil)
  ;; NOTE to future self: fido uses flex by default, and it is exactly what I want
  (fido-vertical-mode t)
  :bind
  (:map icomplete-minibuffer-map
        ;; when there's no exact match, accept the first one under cursor with RET
        ("RET" . icomplete-force-complete-and-exit)
        ;; C-j to force-accept current input even if it's not in the candidate list
        ("C-j" . icomplete-fido-exit)))

(use-package imenu
  :ensure nil
  :demand t
  :bind
  (:map hoagie-keymap
        ("i" . imenu)))

(use-package isearch
  :ensure nil
  :custom
  (search-exit-option 'edit)
  (isearch-lazy-count t)
  (isearch-lazy-highlight 'all-windows)
  (isearch-wrap-pause 'no)
  (isearch-repeat-on-direction-change t)
  (lazy-highlight-initial-delay 0.1)
  (regexp-search-ring-max 64)
  (search-ring-max 64))

(use-package java-mode
  :ensure nil
  :hook
  (java-mode-hook . subword-mode))

(use-package json-mode
  :mode "\\.json$")

;; (use-package kubernetes
;;   :ensure t
;;   :commands (kubernetes-overview)
;;   :bind
;;   ("C-c K" . kubernetes-overview)
;;   ;; experimental alternative
;;   (:map hoagie-keymap
;;         ("K" . kubernetes-overview))
;;   :custom
;;   ;; setting these means I have to manually
;;   ;; refresh the "main" screen
;;   (kubernetes-poll-frequency 3600)
;;   (kubernetes-redraw-frequency 3600)
;;   (kubernetes-pods-display-completed t))

(use-package lisp-mode
  :ensure nil
  :hook
  (lisp-mode-hook . (lambda ()
                      (set (make-local-variable lisp-indent-function)
		                   'common-lisp-indent-function)
                      (setf fill-column 100)
                      (display-fill-column-indicator-mode))))

(use-package markdown-mode
  :init
  (setq markdown-command "pandoc")
  :bind
  (:map markdown-mode-map
        ("C-c C-e" . markdown-do)))

(use-package minibuffer
  :ensure nil
  :demand t
  :custom
  (completions-format 'one-column)
  (completions-max-height 20)
  (completion-styles '(substring partial-completion flex))
  (read-buffer-completion-ignore-case t)
  (read-file-name-completion-ignore-case t)
  (completion-ignore-case t)
  (completions-detailed t)
  (completion-auto-help t)
  (completion-auto-select 'second-tab)
  :bind
  ;; Default is M-v, but that doesn't work when completing text in a buffer and
  ;; M-i has a nice symmetry with C-M-i (used to trigger completion)
  ;; UPDATE 2023-08-07: maybe I don't need this binding...at all?
  ;; can switch with "second tab" - not 100% sure since now I trigger
  ;; with C-M-i
  ;; ("M-i" . switch-to-completions)
  (:map minibuffer-mode-map
        ("C-n" . minibuffer-next-completion)
        ("C-p" . minibuffer-previous-completion))
  (:map completion-in-region-mode-map
        ("RET" . minibuffer-choose-completion)
        ("C-n" . minibuffer-next-completion)
        ("C-p" . minibuffer-previous-completion))
  (:map hoagie-keymap
        ("<f6>" . execute-extended-command)))

(use-package package-lint
  :commands package-lint-current-buffer)

(use-package paren
  :ensure nil
  :custom
  (show-paren-style 'mixed)
  (show-paren-when-point-inside-paren t))

(use-package php-mode
  :ensure t
  )

(use-package plantuml-mode
  :commands plantuml-mode
  :mode
  (("\\.puml$" . plantuml-mode)
   ("\\.plantuml$" . plantuml-mode))
  :custom
  (plantuml-jar-path "~/plantuml/plantuml.jar")
  (plantuml-default-exec-mode 'jar)
  :bind
  (:map plantuml-mode-map
        ("C-c C-p" . hoagie-plantuml-generate-png))
  :config
  (defun hoagie-plantuml-generate-png ()
    (interactive)
    (when (buffer-modified-p)
      (error "There are unsaved changes..."))
    (let* ((input (expand-file-name (buffer-file-name)))
           (output (concat (file-name-sans-extension input) ".png"))
           (output-buffer (get-file-buffer output)))
    (call-process "java" nil t nil
                  ;; the jar file...
                  "-jar"
                  (expand-file-name plantuml-jar-path)
                  input
                  "-tpng")
    (if output-buffer
        (with-current-buffer output-buffer
          (revert-buffer-quick)
          (pop-to-buffer output-buffer))
      (find-file-other-window output)))))

(use-package proced
  :ensure nil
  :custom
  (proced-filter 'all))

(use-package project
  :ensure nil
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
  :ensure nil
  :mode ("\\.py\\'" . python-mode)
  :hook
  (python-mode-hook . (lambda ()
                        (setf fill-column 79)
                        (display-fill-column-indicator-mode)))
  :custom
  (python-shell-font-lock-enable nil)
  (python-shell-interpreter "ipython")
  (python-shell-interpreter-args "--pprint --simple-prompt"))

(use-package re-builder
  :ensure nil
  :custom
  (reb-re-syntax 'string))

(use-package register
  :ensure nil
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
;;   ;; BRITTLENESS WARNING: this re-defines a built-in method, there's
;;   ;; a high risk it breaks when moving Emacs versions
;;   (cl-defmethod register-val-describe ((val marker) _verbose)
;;     (let ((buf (marker-buffer val)))
;;       (if (null buf)
;; 	      (princ "a marker in no buffer")
;;         (princ (hoagie--text-around-marker val))
;;         (princ " -- buffer ")
;;         (princ (buffer-name buf))
;;         (princ ", pos")
;;         (princ (marker-position val)))))
;;   (defun hoagie--text-around-marker (marker)
;;     "Get the line around MARKER.
;; Some inspiration from the package Consult."
;;   (with-current-buffer (marker-buffer marker)
;;     (save-excursion
;;       (save-restriction
;;         (widen)
;;         (goto-char marker)
;;         (beginning-of-line)
;;         (string-trim (thing-at-point 'line))))))
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
The values passed to `copy-to-register' are based on its interactive declaration."
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
The values passed to `point-to-register' are based on its interactive declaration."
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
      (set-register (register-read-with-preview "Register to clear (quit to exit): ")
                    nil))))

(use-package restclient
  :custom
  (restclient-same-buffer-response t)
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
             (setf restclient-response-body-only (not restclient-response-body-only))))
  (defun hoagie-open-restclient (arg)
    "Open a file from the restclient \"collection\"."
    (interactive "P")
    (let ((restclient-file (read-file-name "Open restclient file:"
                                           "~/restclient/"
                                           nil
                                           nil
                                           nil
                                           (lambda (name)
                                             (string-equal (file-name-extension name) "http")))))
      (if arg
          (find-file-other-window restclient-file)
        (find-file restclient-file))))
  (defun hoagie-restclient-imenu-index ()
    "Configure imenu on the convention \"### Title ###\"."
    (setq-local imenu-generic-expression '((nil "^### \\(.*\\) ###$" 1)))))

(use-package replace
  :ensure nil
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
    (if (use-region-p)
        (occur (buffer-substring-no-properties (region-beginning) (region-end)))
      (occur (thing-at-point 'sexp t))))
  (defun hoagie-rename-and-select-occur-buffer ()
    "Renames the current buffer to *Occur: [term] [buffer]*.
Meant to be added to `occur-hook'."
    (cl-destructuring-bind (search-term _ (buffer-name &rest _)) occur-revert-arguments
      (pop-to-buffer
       (rename-buffer (format "*Occur: %s %s*" search-term buffer-name) t)))))

(use-package rcirc
  :ensure nil
  :commands rcirc
  :custom
  (rcirc-server-alist '(("irc.libera.chat"
                         :port 7000
                         :encryption tls
                         :server-alias "libera"
                         :nick "hoagie"
                         :full-name "Sebastián Monía"
                         :user-name "seb.hoagie@outlook.com"
                         :channels ("#emacs" "#emacs-es" "#argentina"))))
  :hook
  (rcirc-mode-hook . (lambda () (rcirc-track-minor-mode 1))))

(use-package savehist
  :ensure nil
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
  :ensure nil
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

(use-package shell
  :ensure nil
  :config
  (defun hoagie-shell-mode-setup ()
    "My personal shell mode setup."
    (toggle-truncate-lines t)
    (setf comint-process-echoes t))
  :hook
  (shell-mode-hook . hoagie-shell-mode-setup))

(use-package sly
  :commands sly
  :custom
  (inferior-lisp-program "sbcl --dynamic-space-size 10240"))

(use-package sly-quicklisp
  :after sly)

;; Send email via Fastmail's SMTP:
(use-package smtpmail
  :ensure nil
  :custom
  (send-mail-function 'smtpmail-send-it)
  (smtpmail-default-smtp-server "smtp.fastmail.com")
  (smtpmail-stream-type  'starttls)
  (smtpmail-smtp-service 587))

(use-package sql
  :ensure nil
  :custom
  (sql-ms-options '("--driver" "ODBC Driver 17 for SQL Server"))
  (sql-ms-program "/var/home/hoagie/github/sqlcmdline/sqlcmdline.py")
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

(use-package terraform-mode
  :mode "\\.tf$")

(use-package time
  :ensure nil
  :custom
  (world-clock-time-format "%r - %F (%A)")
  (world-clock-list '(("America/Denver" "Denver")
                      ("America/Buenos_Aires" "Buenos Aires")
                      ("America/Mexico_City" "CDMX")
                      ("America/Chicago" "Central")
                      ("America/New_York" "Eastern")))
  :commands (world-clock))

(use-package tramp
  :ensure nil
  :custom
  (tramp-default-method "sshx"))

(use-package vc
  :ensure nil
  :demand t
  :bind
  (:map vc-prefix-map
        ;; make it consistent with vc-dir
        ("k" . vc-revert)))

(use-package vc-dir
  :ensure nil
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
  :ensure nil
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
  :ensure nil
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
  :ensure nil
  :config
  ;; Stores the window setup before focusing on a single window, and restore it
  ;; on a "mirror" binding: C-x 1 vs F6 1. Simplified version of the
  ;; idea at https://erick.navarro.io/blog/save-and-restore-window-configuration-in-emacs/
  (defvar hoagie-window-configuration nil "Window configuration saved before deleting other windows.")
  (defun hoagie-restore-window-configuration ()
    "Use `hoagie-window-configuration' to restore the window setup."
    (interactive)
    (when hoagie-window-configuration
      (set-window-configuration hoagie-window-configuration)))
  (defun hoagie-delete-other-windows ()
    "Custom `delete-other-windows' that stores the current setup in `hoagie-window-configuration'.
Adding an advice to the existing command was finicky."
    (interactive)
    (setf hoagie-window-configuration (current-window-configuration))
    (delete-other-windows))
  (defun hoagie-toggle-frame-split ()
    "Toggle orientation, just like ediff's |.
See https://www.emacswiki.org/emacs/ToggleWindowSplit for sources, this version is my own
spin of the first two in the page."
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
        ("1" . hoagie-restore-window-configuration)
        ("|" . hoagie-toggle-frame-split)))

(use-package web-mode
  :mode
  (("\\.html$" . web-mode)
   ("\\.phtml\\'" . web-mode)
   ("\\.tpl\\.php\\'" . web-mode)
   ("\\.[agj]sp\\'" . web-mode)
   ("\\.as[cp]x\\'" . web-mode)
   ("\\.cshtml\\'" . web-mode)
   ("\\.erb\\'" . web-mode)
   ("\\.mustache\\'" . web-mode)
   ("\\.djhtml\\'" . web-mode)
   ("\\.html?\\'" . web-mode)
   ("\\.css\\'" . web-mode)
   ("\\.xml?\\'" . web-mode))
  :custom
  (web-mode-enable-css-colorization t)
  (web-mode-enable-sql-detection t)
  (web-mode-enable-current-element-highlight t)
  (web-mode-markup-indent-offset 2))

(use-package ws-butler
  :hook
  (prog-mode-hook . ws-butler-mode))

(use-package yaml-mode
  :mode "\\.yml$")

;; Everything that is not part of a particular feature to require
(use-package emacs
  :ensure nil
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
  ;; from https://demonastery.org/2013/04/emacs-narrow-to-region-indirect/
  ;; modified to DWIM: if there's no active region, just clone the entire
  ;; buffer. Also use `pop-to-buffer' instead of `switch-to-buffer'
  (defun hoagie-clone-indirect-dwim ()
    "Create an indirect buffer, narrow it to the current region if active."
    (interactive)
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
  (defun jump-to-char (arg char &optional interactive)
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
  ;; TODO: should I remove this binding? it is sometimes shadowed by other
  ;; modes, and it clashes with some new Emacs 29 commands
  ;; ("M-N" . next-buffer)
  ;; ("M-P" . previous-buffer)
  ;; from https://karthinks.com/software/batteries-included-with-emacs/#cycle-spacing--m-spc
  ;; ("M-SPC" . cycle-spacing) default in 29 apparently? confirm
  ;; from https://emacsredux.com/blog/2020/06/10/comment-commands-redux/
  ("<remap> <comment-dwim>" . comment-line)
  ;; replace delete-char, as recommended in the docs
  ("C-d" . delete-forward-char)
  ("C-<backspace>" . backward-delete-word)
  ("M-c" . capitalize-dwim)
  ("M-u" . upcase-dwim)
  ("M-l" . downcase-dwim)
  ("C-z" . jump-to-char)
  ("M-z" . zap-up-to-char)
  ("C-x n i" . hoagie-clone-indirect-dwim)
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
        ("u" . delete-pair))
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
   ";; Il semble que la perfection soit atteinte non quand il n’y a plus rien à ajouter, mais quand il n’y a plus à retrancher. - Antoine de Saint Exupéry\n;; It seems that perfection is attained not when there is nothing more to add, but when there is nothing more to remove.\n\n;; Marking:                             ;; Killing\n;; M-@ Next word C-M-<SPC> Next sexp    ;; C-M-k next sexp\n;; M-h Paragraph C-x h Buffer           ;; C-k rest of line\n;; C-M-h Next defun                     ;; <f6> k whole line\n\n;; Misc:                                   ;; (e)SHELL C-c then...\n;; <f6> i imenu C-x C-k e edit kmacro      ;; C-[p|n] prev/next input\n;; M-t Transpose word C-M-t Tranpose sexp  ;; C-o clear last output\n;; C-x / vundo\n\n;; During search\n;; C-w add word at point, repeat to add more\n;; M-r toggle regex\n\n;; Replace (regexp + elisp):\n;; \"movie\" -> \"film\" and \"movies\" -> \"films\":\n;; ‘movie(s)?‘ -> ‘,(if \\1 \"films\" \"film\")‘\n\n;; howm:\n;; <f6> 3 - inbox\n;; <f6> C-3 - show TODO\n;; <f6> ESC 3 - show scheduled\n;; <f3> howm keymap (hit twice for menu)\n\n;; REMEMBER YOUR REGEXPS\n")
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
        compilation-error-regexp-alist (delete 'maven compilation-error-regexp-alist))
  ;; from http://www.jurta.org/en/emacs/dotemacs, set the major mode
  ;; of buffers that are not visiting a file
  (setq-default major-mode (lambda ()
                             (if buffer-file-name
                                 (fundamental-mode)
                               (let ((buffer-file-name (buffer-name)))
                                 (set-auto-mode)))))
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
  "Convert a Unix TIMESTAMP (as string) to date.  If the parameter is not provided use word at point."
  (interactive (list (thing-at-point 'word t)))
  (let ((to-convert (if (< 10 (length timestamp)) (substring timestamp 0 10) timestamp))
        (millis (if (< 10 (length timestamp)) (substring timestamp 10 (length timestamp)) "000")))
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
    ;; 2021-05-22: now I use the pgtk branch everywhere, and the monitor name has
    ;; a meaningul value in all cases, so:
    (let* ((monitor-name (alist-get 'name (frame-monitor-attributes)))
           (monitor-font '(("0x0536" . 151) ;; laptop
                           ("2757" . 128))) ;; external monitor
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
