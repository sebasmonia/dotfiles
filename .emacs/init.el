;; .emacs --- My dot emacs file  -*- lexical-binding: t; -*-

;; Author: Sebastian Monia <code@sebasmonia.com>
;; URL: https://git.sr.ht/~sebasmonia/dotfiles
;; Version: 29.4
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
;;             remote/slow environments like VDIs...
;; 2023-03-15: Rework my old push mark + visible-mark system into something based on
;;             registers and maybe advices. Revisit some bindings to take advantage
;;             of the Dygma Raise configuration
;;
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

(global-set-key (kbd "<f6>") 'hoagie-keymap)
(define-key key-translation-map (kbd "<f7>") (kbd "C-x"))
(define-key key-translation-map (kbd "<f8>") (kbd "C-c"))

(use-package ansi-color
  :ensure nil
  :commands (ansi-color-apply-buffer)
  :init
  (defun ansi-color-apply-buffer ()
    "Colorize the entire buffer using `ansi-color-apply-on-region'."
    (interactive)
    (ansi-color-apply-on-region (point-min) (point-max))))

(use-package browse-kill-ring
  :config
  (browse-kill-ring-default-keybindings))

(use-package company
  :hook
  (after-init-hook . global-company-mode)
  :bind
  ("C-<tab>" . company-indent-or-complete-common)
  (:map company-active-map
        ("C-<RET>" . company-abort)
        ("<tab>" . company-complete-selection))
  :custom
  (company-idle-delay 0.2)
  (company-minimum-prefix-length 3)
  (company-selection-wrap-around t))

(use-package company-dabbrev
  :after company
  :ensure nil
  :custom
  (company-dabbrev-ignore-case nil)
  (company-dabbrev-downcase nil))

(use-package company-dabbrev-code
  :after company
  :ensure nil
  :custom
  (company-dabbrev-code-modes t)
  (company-dabbrev-code-ignore-case nil))

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
  ;; use external 'ls' even on Windows
  (ls-lisp-use-insert-directory-program t)
  :bind
  ("<C-f1>" . 'hoagie-kill-buffer-filename)
  (:map hoagie-keymap
        (("F" . find-name-dired)
         ("J" . dired-jump)
         ("j" . dired-jump-other-window)))
  (:map dired-mode-map
        ("C-<return>" . dired-open-file)
        ("<C-f1>" . (lambda () (interactive) (dired-copy-filename-as-kill 0))))
  :hook
  (dired-mode-hook . dired-hide-details-mode)
  :config
  (defun dired-open-file ()
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
    (let ((name (buffer-file-name)))
      (when name
        (kill-new name))
      (message (format "Filename: %s" (or name "-No file for this buffer-"))))))

;; use ls-dired in VDI?
(setq ls-lisp-use-insert-directory-program nil)
(require 'ls-lisp)

(use-package dired-narrow
  :after dired
  :bind
  (:map dired-mode-map
        ("/" . dired-narrow)))

(use-package dired-git-info
  :after dired
  :bind
  (:map dired-mode-map
        (")" . dired-git-info-mode)))

(use-package display-line-numbers
  :ensure nil
  :custom
  (display-line-numbers-major-tick 10)
  (display-line-numbers-type 'relative)
  :hook
  (after-init-hook . global-display-line-numbers-mode))

(use-package docker
  :bind
  ("C-c d" . docker))

(use-package dockerfile-mode
  :mode "Dockerfile\\'")

(use-package ediff
  :ensure nil
  :bind
  (:map hoagie-keymap
        ("e" . ediff-buffers))
  :custom
  (ediff-forward-word-function 'forward-char) ;; from https://emacs.stackexchange.com/a/9411/17066
  (ediff-highlight-all-diffs t)
  (ediff-keep-variants nil)
  (ediff-window-setup-function 'ediff-setup-windows-plain)
  :hook
  (ediff-keymap-setup-hook . add-d-to-ediff-mode-map)
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
    (set-window-configuration hoagie-pre-ediff-windows))
  (add-hook 'ediff-before-setup-hook #'hoagie-ediff-store-windows)
  ;; Welp, don't like using internals but, the regular hook doesn't quite work
  ;; the window config is restored but then _stuff happens_, so:
  (add-hook 'ediff-after-quit-hook-internal #'hoagie-ediff-restore-windows))

(use-package eglot
  :commands (eglot eglot-ensure)
  :hook
  ((python-mode-hook . eglot-ensure)
   (go-mode-hook . eglot-ensure))
  :bind
  (:map hoagie-keymap
        (("l r" . eglot-rename)
         ("l f" . eglot-format)
         ("l h" . eldoc)
         ("l a" . eglot-code-actions)))
  (:map eglot-mode-map
        (("C-c C-e r" . eglot-rename)
         ("C-c C-e f" . eglot-format)
         ("C-c C-e h" . eldoc))))

(use-package eldoc
  :ensure nil
  :demand t
  :config
  (global-eldoc-mode))

(use-package elec-pair
  :ensure nil
  :custom
  (electric-pair-inhibit-predicate 'electric-pair-inhibit-if-helps-balance)
  :config
  (electric-pair-mode))

(use-package eshell
  :ensure nil
  :custom
  ;; TODO: check all customizations
  (eshell-prefer-lisp-functions nil)
  (eshell-prefer-lisp-variables nil)
  :bind
  (:map hoagie-keymap
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
        ("o" . eww)
        ("O" . eww-browse-with-external-browser))
  (:map hoagie-keymap
        ("b" . hoagie-browse-url-at-point))
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
  (defun hoagie-browse-url-at-point (&optional arg)
    "A mix of `browse-url-button-open-url' and `browse-url-at-point'.
Open the URL at point in EWW, use external browser with prefix arg."
    (interactive "P")
    (funcall (if arg
                 'browse-url
               'eww-browse-url)
             (browse-url-url-at-point))))

(use-package eww-lnum
  :after eww
  :bind
  (:map eww-mode-map
        ("C-c SPC" . eww-lnum-follow)))

(use-package expand-region
  :bind
  ;; I want to keep cycle-spacing in its default binding, so
  ;; moving expand-region to C-c SPC which in turns moves it from
  ;; F7 SPC to F8 SPC
  ("C-c <SPC>" . er/expand-region)
  ;; replacing mark commands with registers frees F6 spc
  (:map hoagie-keymap
        ("SPC" . er/expand-region))
  :config
  (er/enable-mode-expansions 'csharp-mode 'er/add-cc-mode-expansions))

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

(use-package git-timemachine
  :bind
  ("C-x M-G" . git-timemachine))

(use-package go-mode
  :custom
  (godoc-at-point-function 'godoc-gogetdoc)
  :bind
  ;; need a better binding for this, but haven't used it yet
  ("C-c G d" . godoc-at-point)
  :hook
  (go-mode-hook . subword-mode))

(use-package grep
  :ensure nil
  :custom
  (grep-command "grep --color=always -nHi -r --include=*.* -e \"pattern\" .")
  (grep-use-null-device nil)
  ;; :config
  ;; ;; from https://www.emacswiki.org/emacs/NTEmacsWithCygwin
  ;; (defadvice grep-compute-defaults (around grep-compute-defaults-advice-null-device)
  ;;   "Use cygwin's /dev/null as the null-device."
  ;;   (let ((null-device "/dev/null"))
  ;;     ad-do-it))
  ;; (ad-activate 'grep-compute-defaults)
  :bind
  (:map hoagie-keymap
        ("G" . rgrep)))

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
  ("C-<f3>" . hoagie-howm-inbox)
  ("S-<f3>" . howm-list-todo)
  ("C-S-<f3>" . howm-list-schedule)
  (:map hoagie-howm-keymap
        ("c" . howm-create)
        ("<f3>" . howm-menu)
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
  (setf icomplete-in-buffer nil)
  (icomplete-vertical-mode)
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

;; Trying to use more integrated vc-mode, but leave Magit for the "power stuff"
(use-package magit
  :init
  :bind
  ("C-x G" . magit-status)
  :custom
  ;; this option solves the problems with Magit commits & diffs with
  ;; my current `display-buffer-alist' configuration
  (magit-commit-diff-inhibit-same-window t)
  (magit-display-buffer-function 'display-buffer))

(use-package minibuffer
  :ensure nil
  :demand t
  :custom
  (completions-format 'one-column)
  ;; (completions-header-format nil)
  (completions-max-height 15)
  (completion-auto-select 'second-tab)
  (completion-styles '(flex basic partial-completion))
  (read-buffer-completion-ignore-case t)
  (read-file-name-completion-ignore-case t)
  (completion-ignore-case t)
  (completions-detailed t)
  (completion-auto-help 'lazy)
  :bind
  ;; Default is M-v, but that doesn't work when completing text in a buffer and
  ;; M-i has a nice symmetry with C-i (TAB) that is used to trigger completion
  ("M-i" . switch-to-completions)
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
  :config
  ;; apparently it is now enabled by default?
  (show-paren-mode)
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
  :config
  (defun hoagie-project-multi-occur (regexp &optional nlines)
    "Run `multi-occur' in all the files in the current project."
    ;; very much inspired by https://github.com/NicolasPetton/noccur.el
    ;; By using `project-files' instead of "git ls", it works in subprojects
    (interactive (occur-read-primary-args))
    (let* ((the-project (project-current t))
           (default-directory (project-root the-project ))
           (files (mapcar #'find-file-noselect (project-files the-project))))
      (multi-occur files regexp nlines))))

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

(defvar hoagie-register-keymap (define-prefix-command 'hoagie-register-keymap) "Sligthly more accessible register keybindings.")
(use-package register
  :ensure nil
  :demand t
  :custom
  (register-preview-delay 0.1)
  :bind
  ;; in the keyboard, F5 is now in Enter
  ("<f5>" . hoagie-register-keymap)
  (:map hoagie-register-keymap
        ;; hitting F5 twice to jump sounds like a good shortcut to
        ;; push things semi-constantly
        ("<f5>" . hoagie-push-to-register-dwim)
        ("s" . copy-to-register)
        ("i" . hoagie-insert-register)
        ("f" . hoagie-current-file-to-register)
        ("l" . list-registers)
        ("SPC" . point-to-register)
        ;; this won't jump to files, remains to be seen if I need that
        ("j" . hoagie-jump-to-register))
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
        (princ ", pos")
        (princ (marker-position val)))))
  (defun hoagie--text-around-marker (marker)
    "Get the line around MARKER.
Some inspiration from the package Consult."
  (with-current-buffer (marker-buffer marker)
    (save-excursion
      (save-restriction
        (widen)
        (goto-char marker)
        (beginning-of-line)
        (string-trim (thing-at-point 'line))))))

  (defun hoagie-current-file-to-register (register &optional _arg)
    "Stored the currently visited file in REGISTER."
    ;; https://www.gnu.org/software/emacs/manual/html_node/emacs/File-Registers.html
    (interactive (list (register-read-with-preview
		                "Current file to register: ")
		               current-prefix-arg))
    (set-register register (cons 'file (buffer-file-name))))
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
    "qwertasdfgzxcvbyuiophjklnm[];',./-=`?\"[](){}"
    "The order in which to walk the registers in `hoagie-next-register'")
  (defvar hoagie-last-register-index -1)
  (defun hoagie-next-register ()
    "Return the next char from `hoagie-registers-order'.
Silently starts over if needed."
    (cl-incf hoagie-last-register-index)
    (setf hoagie-last-register-index (mod hoagie-last-register-index
                                          (length hoagie-registers-order)))
    (elt hoagie-registers-order hoagie-last-register-index))
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
It also deletes the register unless called with prefix ARG."
    (interactive "P")
    (let* ((register-alist (cl-loop for reg in register-alist
                                    when (markerp (cdr reg))
                                    collect reg))
           (reg (register-read-with-preview "Jump to: ")))
      (jump-to-register reg)
      (unless arg
        (set-register reg nil))))
  (defun hoagie-insert-register (&optional arg)
    "Almost like `insert-register' but filters the alist for better preview.
It also deletes the register unless called with prefix ARG."
    (interactive "P")
    (let* ((register-alist (cl-loop for reg in register-alist
                                    when (stringp (cdr reg))
                                    collect reg))
           (reg (register-read-with-preview "Insert text: ")))
      (insert-register reg)
      (unless arg
        (set-register reg nil)))))

(use-package repeat
  :ensure nil
  :demand t
  :custom
  (repeat-exit-key (kbd "RET"))
  :config
  (repeat-mode))

(use-package restclient
  :custom
  (restclient-same-buffer-response . nil)
  (restclient-response-body-only t)
  :mode
  ("\\.http\\'" . restclient-mode)
  :bind
  ("<f4>" . hoagie-open-restclient)
  :hook
  (restclient-mode-hook . hoagie-restclient-imenu-index)
  :config
  (defun hoagie-open-restclient (arg)
    "Open a file from the restclient \"collection\"."
    (interactive "P")
    (let ((restclient-file (read-file-name "Open restclient file:" "~/restclient/")))
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
        ("o" . hoagie-occur-dwim)
        ("O" . multi-occur-in-matching-buffers))
  :config
  (defun hoagie-occur-dwim ()
    "Run occur, if there's a region selected use that as input.
By default, occur _limits the search to the region_ if it is active."
    (interactive)
    (if (use-region-p)
        (occur (buffer-substring-no-properties (region-beginning) (region-end)))
      (command-execute 'occur)))
  (defun hoagie-rename-and-select-occur-buffer ()
    "Renames the current buffer to *Occur: [term] [buffer]*.
Meant to be added to `occur-hook'."
    (cl-destructuring-bind (search-term _ (buffer-name &rest _)) occur-revert-arguments
      (pop-to-buffer
       (rename-buffer (format "*Occur: %s %s*" search-term buffer-name) t))))
  (add-hook 'occur-hook #'hoagie-rename-and-select-occur-buffer))

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
        ("n" . sharper-main-transient))
  :custom
  (sharper-run-only-one t))

(use-package shr
  :ensure nil
  :custom
  (shr-use-fonts t)
  (shr-use-colors t)
  (shr-bullet "• ")
  (shr-indentation 2)
  (shr-discard-aria-hidden t))

(use-package shell
  :ensure nil
  :config
  (defun hoagie-shell-mode-setup ()
    "My personal shell mode setup."
    (toggle-truncate-lines t)
    (setf comint-process-echoes t)
    ;; From https://emacs.stackexchange.com/a/62420
    (when (and (fboundp 'company-mode)
               (file-remote-p default-directory))
      (company-mode -1)))
  :hook
  (shell-mode-hook . hoagie-shell-mode-setup))

(use-package sly
  :commands sly
  :custom
  (inferior-lisp-program "sbcl --dynamic-space-size 10240"))

(use-package sly-quicklisp
  :after sly)

(use-package sql
  :ensure nil
  :custom
  (sql-ms-options '("--driver" "ODBC Driver 17 for SQL Server"))
  (sql-ms-program "/var/home/hoagie/github/sqlcmdline/sqlcmdline.py")
  :hook
  (sql-interactive-mode-hook . (lambda () (setf truncate-lines t))))

(use-package sql-datum :load-path "~/github/datum")

(use-package terraform-mode
  :mode "\\.tf$")

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
        ;; vc-dir-find-file-other-window, but I use project-find-file instead
        ("o" . hoagie-vc-git-current-branch-upstream-origin)
        ("e" . vc-ediff)
        ("k" . vc-revert)
        ("r" . hoagie-vc-dir-reset)
        ("d" . hoagie-vc-dir-delete)
        ("b L" . hoagie-vc-git-show-branches))
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
      ;; "clone" needs nil to
      (setf directory nil))
    (vc-git-command nil 0 nil "clone" repository-url directory)
    (message "Repository cloned!"))
  (defun hoagie-vc-git-current-branch-upstream-origin ()
    "Set the upstream of the current git branch to \"origin\".
This is meant to be called after creating a new branch with `vc-create-tag' (do
not forget the prefix arg, else you get a new TAG not BRANCH). Else the new
branch remains local-only."
    (interactive)
    (let ((current-branch (car (vc-git-branches))))
      (when (y-or-n-p (format "Set the upstream of %s to origin?" current-branch))
        (vc-git-command nil 0 nil "push" "--set-upstream" "origin" current-branch)
        (message "Upstream of %s is now ORIGIN." current-branch))))
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
        ("f" . hoagie-vc-git-fetch-all) ;; vc-dir-find-file, but I use project-find-file instead
        ("o" . hoagie-vc-git-current-branch-upstream-origin)
        ("b L" . hoagie-vc-git-show-branches) ;; l is used for branch-log
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
    "A copy of `zap-up-to-char' that doesn't kill the text."
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
  ("M-n" . next-buffer)
  ("M-p" . previous-buffer)
  ("C-S-k" . kill-whole-line) ;; more convenient than default C-S-<backspace>
  ;; from https://karthinks.com/software/batteries-included-with-emacs/#cycle-spacing--m-spc
  ("M-SPC" . cycle-spacing)
  ;; from https://emacsredux.com/blog/2020/06/10/comment-commands-redux/
  ("<remap> <comment-dwim>" . comment-line)
  ("C-d" . delete-forward-char) ;; replace delete-char, as recommended in the docs
  ("C-<backspace>" . backward-delete-word)
  ("M-c" . capitalize-dwim)
  ("M-u" . upcase-dwim)
  ("M-l" . downcase-dwim)
  ("C-z" . jump-to-char)
  ("M-z" . zap-up-to-char)
  ;; like flycheck's C-c ! l
  ("C-c !" . flymake-show-buffer-diagnostics)
  ("C-x n i" . hoagie-clone-indirect-dwim)
  ;; it's back...
  ("<remap> <list-buffers>" . ibuffer)
  (:map hoagie-keymap
        ;; need to keep this one more present...
        ("u" . delete-pair))
  :custom
  ;; experimental, I don't think I have a need for this...
  (create-lockfiles nil)
  (sentence-end-double-space nil)
  (tab-width 4) ;; make golang code nicer to read
  (tab-always-indent 'complete)
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
   ";; Il semble que la perfection soit atteinte non quand il n’y a plus rien à ajouter, mais quand il n’y a plus à retrancher. - Antoine de Saint Exupéry\n;; It seems that perfection is attained not when there is nothing more to add, but when there is nothing more to remove.\n\n;; Setting the region:\n;; M-h - Paragraph  C-x h - Buffer\n;; C-M-h - Next defun ;; M-@ - Next word\n;; C-M-<SPC> or C-M-@ - Mark next sexp --- C-M-k kill it\n\n;; imenu: M-i\n;; Transpose: word M-t sexp C-M-t\n;; C-x C-k e edit kmacro\n;; C-z zap-up-to-char\n\n;; (e)SHELL:\n;; C-c C-[p|n] prev/next input\n;; C-c C-o clear last output\n\n;; Search\n;; C-s C-w search word at point, each C-w adds next word\n;; Replace \"movie\" with \"film\" and \"movies\" with \"films\": `movie\(s\)?` -> `\,(if \\1 \"films\" \"film\")`\n;; Another common use case is to transform numbers in the matches using the format function.\n\n;; C-; dabbrev\n\n;; howm:\n;; C-<f3> - inbox\n;; S-<f3> - show TODO\n;; C-S-<f3> - show scheduled\n\n;; REMEMBER YOUR REGEXPS")
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
(setf user-full-name "Sebastián Monía"
      user-mail-address "sebastian@sebasmonia.com")

(when (string= system-type "darwin")
  ;; use `ls` from coreutils, installed with homebrew
  (customize-set-value 'insert-directory-program "gls")
  )

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
           (monitor-font '(("S240HL" . 143) ;; 24"
                           ("2757" . 128) ;; 27"
                           ("LG HDR 4K" . 188))) ;; 27" office - was 181
           (size (alist-get monitor-name monitor-font
                            180 ;; default size, "big just in case"
                            nil
                            'equal)))
      ;; override for "laptop screen only"
      (when (eq (length (display-monitor-attributes-list)) 1)
        (setf size 143))
      (set-face-attribute 'default (selected-frame) :height size)
    ))
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

;;; Experimental features - from reading Mastering Emacs

;; TODO: rely on C-M-SPC/C-M-@ for mark-sexp and change M-h to mark-defun

;; Follow up to previous: C-M-SPC to select, C-M-k to kill by sexp.
;; I should be using these two a lot more

;; See M-i for imenu

;; Transpose word M-t sexp C-M-t

;;; init.el ends here
 
