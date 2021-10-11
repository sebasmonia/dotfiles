;; .emacs --- My dot emacs file

;; Author: Sebastian Monia <smonia@outlook.com>
;; URL: https://github.com/sebasmonia/.emacs
;; Version: 4
;; Keywords: .emacs dotemacs

;; This file is not part of GNU Emacs.

;;; Commentary:

;; My dot Emacs file
;; In theory I should be able to just drop the file in any computer and have
;; the config synced without merging/adapting anything
;; Update 2019-05-06: V3 means I moved to use-package
;; Update 2020-06-14: Arbitrarily bumping the version number
;; Update 2021-09-02: Used https://www.manueluberti.eu/emacs/2021/09/01/package-report/
;;                    to remove some dead packages and things I didn't use that much.

;;; Code:

(setf custom-file (expand-file-name (concat user-emacs-directory "custom.el")))

(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)

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

(custom-set-faces
 '(default ((t (:family "IBM Plex Mono" :foundry "IBM " :slant normal :weight normal :height 128 :width normal)))))

;; based on http://www.ergoemacs.org/emacs/emacs_menu_app_keys.html
(defvar hoagie-keymap (define-prefix-command 'hoagie-keymap) "My custom bindings.")
(define-key key-translation-map (kbd "<apps>") (kbd "<menu>")) ;; compat Linux-Windows
(define-key key-translation-map (kbd "<print>") (kbd "<menu>")) ;; curse you, thinkpad keyboard!!!
(global-set-key (kbd "<menu>") 'hoagie-keymap)
(global-set-key (kbd "C-'") 'hoagie-keymap)
(define-key hoagie-keymap (kbd "k") #'kill-this-buffer)

;; experimenting with new types of keybindings/entry keys for keymaps
(global-set-key (kbd "<f6>") 'hoagie-keymap)
(global-set-key (kbd "<f7>") ctl-x-map)
(global-set-key (kbd "<f8>") mode-specific-map)

(use-package company
  :bind
  ("M-S-<SPC>" . company-complete-common)
  (:map hoagie-keymap
        ("<SPC>" . company-complete-common))
  (:map company-active-map
        ("C-<RET>" . company-abort)
        ("[?\t]" . company-complete-selection)
        ("C-n" . company-select-next)
        ("C-p" . company-select-previous))
  :hook
  (after-init-hook . global-company-mode)
  :custom
  (company-idle-delay 0.01)
  (company-minimum-prefix-length 2)
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

;; (use-package grep
;;   :ensure nil
;;   :bind
;;   (:map hoagie-keymap
;;         ("g" . rgrep)))

;; (use-package deadgrep
;;   :bind
;;   (:map hoagie-keymap
;;         ("g" . deadgrep))
;;   (:map deadgrep-mode-map
;;         ("r" . (lambda () (interactive) (setf deadgrep--search-type 'regexp) (deadgrep-restart)))
;;         ("s" . (lambda () (interactive) (setf deadgrep--search-type 'string) (deadgrep-restart))))
;;   :config
;;   (defun deadgrep--format-command-patch (rg-command)
;;     "Add --hidden to rg-command."
;;     (replace-regexp-in-string "^rg " "rg --hidden " rg-command))
;;   (advice-add 'deadgrep--format-command :filter-return #'deadgrep--format-command-patch))

(use-package dired
  :ensure nil
  :custom
  (dired-listing-switches "-laogGhvD")
  (dired-compress-file-suffixes
        '(("\\.tar\\.gz\\'" #1="" "7z x -aoa -o%o %i")
          ("\\.tgz\\'" #1# "7z x -aoa -o%o %i")
          ("\\.zip\\'" #1# "7z x -aoa -o%o %i")
          ("\\.7z\\'" #1# "7z x -aoa -o%o %i")
          ("\\.tar\\'" ".tgz" nil)
          (":" ".tar.gz" "tar -cf- %i | gzip -c9 > %o")))
  (dired-compress-files-alist
        '(("\\.7z\\'" . "7z a -r %o %i")
          ("\\.zip\\'" . "7z a -r %o  %i")))
  :bind
  ("<C-f1>" . 'hoagie-kill-buffer-filename)
  (:map hoagie-keymap
        (("F" . find-name-dired)
         ("j" . hoagie-dired-jump-other-window)
         ("J" . dired-jump)))
  (:map dired-mode-map
        ("C-<return>" . dired-open-file))
  :config
  ;; from Emacs Wiki
  (defun dired-open-file ()
    "Call xdg-open on the file at point."
    (interactive)
    (call-process "xdg-open" nil 0 nil (dired-get-filename nil t)))
  (defun hoagie-dired-jump-other-window ()
    (interactive)
    (dired-jump t))
  (defun hoagie-kill-buffer-filename ()
    "Sends the current buffer's filename to the kill ring."
    (interactive)
    (let ((name (buffer-file-name)))
      (when name
        (kill-new name))
      (message (format "Filename: %s" (or name "-No file for this buffer-")))))
  (define-key dired-mode-map (kbd "<C-f1>") (lambda () (interactive) (dired-copy-filename-as-kill 0))))

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

(use-package docker
  :bind
  ("C-c d" . docker))

(use-package dockerfile-mode
  :mode "Dockerfile\\'")

(use-package ediff
  :ensure nil
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
    (define-key ediff-mode-map "d" 'ediff-copy-both-to-C))
  ;; TODO: after moving 100% to Linux, the will/need to use VC is gone. The code below is candidate
  ;; for deletion
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
  ;; the window config is restore but them _stuff happens_, so:
  (add-hook 'ediff-after-quit-hook-internal #'hoagie-ediff-restore-windows))

(use-package eww
  :ensure nil
  :hook
  (eww-mode-hook . toggle-word-wrap)
  (eww-mode-hook . visual-line-mode)
  :custom
  (browse-url-browser-function #'eww-browse-url)
  :bind
  (:map eww-mode-map
        ("o" . eww)
        ("O" . eww-browse-with-external-browser)))

(use-package eww-lnum
  :after eww
  :bind
  (:map eww-mode-map
        ("C-c SPC" . eww-lnum-follow)))

(use-package eglot
  :commands (eglot eglot-ensure)
  :hook
  ((python-mode-hook . eglot-ensure)
   (csharp-mode-hook . eglot-ensure)
   (java-mode-hook . eglot-ensure))
  :bind
  (:map hoagie-keymap
        (("e r" . eglot-rename)
         ("e f" . eglot-format)
         ("e h" . eldoc)))
  (:map eglot-mode-map
        (("C-c C-e r" . eglot-rename)
         ("C-c C-e f" . eglot-format)
         ("C-c C-e h" . eldoc)))
  :config
  ;; for Java LSP to work
  (setenv "CLASSPATH" "$CLASSPATH:/var/home/hoagie/java_lsp_server/plugins/org.eclipse.equinox.launcher_1.6.300.v20210813-1054.jar" t)
  (add-to-list 'eglot-server-programs
               `(csharp-mode . ("/home/hoagie/.omnisharp/1.37.15/run" "-lsp")))
  ;; patch the argument. when nil, use "" instead.
  (defun eglot--format-markup-patch (args)
    (list (or (car args) "")))
  (advice-add 'eglot--format-markup :filter-args #'eglot--format-markup-patch))

(use-package eldoc-box
  :hook
  (prog-mode-hook . eldoc-box-hover-mode)
  (comint-mode-hook . eldoc-box-hover-mode)
  :custom
  (eldoc-box-max-pixel-width 1024)
  (eldoc-box-max-pixel-height 768)
  (eldoc-idle-delay 0.1)
  :config
  ;; set the child frame face as 1.0 relative to the default font
  (set-face-attribute 'eldoc-box-body nil :inherit 'default :height 1.0))

(use-package elec-pair
  :ensure nil
  :custom
  (electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit)
  :config
  (electric-pair-mode))

(use-package expand-region
  :bind
  ("M-<SPC>" . er/expand-region)
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
  (emacs-lisp-mode-hook . (lambda ()
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

(use-package hl-line
  :ensure nil
  :hook
  (after-init-hook . global-hl-line-mode))

(use-package icomplete
  :ensure nil
  :demand t
  :custom
  (icomplete-hide-common-prefix nil)
  (icomplete-show-matches-on-no-input t)
  (icomplete-prospects-height 10)
  (icomplete-delay-completions-threshold 1000)
  (icomplete-max-delay-chars 1)
  ;; The following are minibuffer/C customizations
  ;; but it makes sense to have them here:
  (completion-styles '(substring partial-completion))
  (read-buffer-completion-ignore-case t)
  (read-file-name-completion-ignore-case t)
  (completion-ignore-case t)
  (completion-cycle-threshold t)
  :init
  ;; Not the best place for this, but since icomplete displaced amx/smex...
  (define-key hoagie-keymap (kbd "<f6>") #'execute-extended-command)
  (define-key hoagie-keymap (kbd "<menu>") #'execute-extended-command)
  (define-key hoagie-keymap (kbd "C-'") #'execute-extended-command)
  :config
  (fido-mode t)
  (icomplete-vertical-mode t) ;; new in Emacs 28
  ;; Non-custom configuration:
  (setf icomplete-in-buffer t)
  :bind
  (:map icomplete-minibuffer-map
        ("C-<return>" . icomplete-fido-exit) ;; when there's no exact match
        ("C-j" . icomplete-fido-exit) ;; from the IDO days...
        ("<down>" . icomplete-forward-completions)
        ("C-n" . icomplete-forward-completions)
        ("<up>" . icomplete-backward-completions)
        ("C-p" . icomplete-backward-completions)))

(use-package isearch
  :ensure nil
  :custom
  (isearch-lazy-count t)
  (isearch-lazy-highlight 'all-windows)
  (lazy-highlight-initial-delay 0.1)
  (regexp-search-ring-max 64)
  (search-ring-max 64))

(use-package java-mode
  :ensure nil
  :hook
  (java-mode-hook . subword-mode))

(use-package json-mode
  :mode "\\.json$")

(with-eval-after-load "vc-hooks"
  (define-key vc-prefix-map "=" 'vc-ediff))
(with-eval-after-load "vc-dir"
  (define-key vc-dir-mode-map "=" 'vc-ediff)
  (define-key vc-dir-mode-map "k" 'vc-revert))
(defun hoagie-try-vc-here-and-there ()
  "Open `vc-dir' for the root of the current project."
  (interactive)
  (vc-dir (project-root (project-current))))
(global-set-key (kbd "C-x t") #'hoagie-try-vc-here-and-there)
(use-package magit
  :init
  :bind
  ("C-x g" . magit-status)
  :custom
  (magit-display-buffer-function 'display-buffer))

(use-package git-timemachine
  :bind ("C-x M-G" . git-timemachine))

(use-package minions
  :config
  (minions-mode 1)
  :custom
  (minions-mode-line-lighter "^"))

(use-package org
  :ensure nil
  :mode (("\\.org$" . org-mode))
  :config
  ;; I don't use this feature and it clashes with
  ;; my mode map binding
  (define-key org-mode-map (kbd "C-'") nil))

(use-package package-lint
  :commands package-lint-current-buffer)

(use-package proced
  :ensure nil
  :custom
  (proced-filter 'all))

(use-package project
  :ensure nil
  :bind
  (:map hoagie-keymap
        ("G" . project-find-regexp)
        ("f" . project-find-file))
  :config
  (add-to-list 'project-switch-commands '(?m "Magit status" magit-status))
  (add-to-list 'project-switch-commands '(?s "Shell" project-shell)))

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

(use-package replace
  :ensure nil
  :config
  (defun hoagie-occur-dwim ()
    "Run occur, if there's a region selected use that as input.
By default, occur _limits the search to the region_ if it is active."
    (interactive)
    (if (use-region-p)
        (occur (buffer-substring-no-properties (region-beginning) (region-end)))
      (command-execute 'occur)))
  (define-key hoagie-keymap (kbd "o") 'hoagie-occur-dwim)
  (defun hoagie-rename-occur-buffer ()
    "Renames the current buffer to *Occur: [term] [buffer]*.
Meant to be added to `occur-hook'."
    (cl-destructuring-bind (search-term _ (buffer-name &rest _)) occur-revert-arguments
      (rename-buffer (format "*Occur: %s %s*" search-term buffer-name) t)))
  (add-hook 'occur-hook #'hoagie-rename-occur-buffer))

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
  :config
  (savehist-mode))

(use-package sharper
  :bind
  (:map hoagie-keymap
        ("n" . sharper-main-transient))
  :custom
  (sharper-run-only-one t))

(use-package shr
  :ensure nil
  :custom
  (shr-use-fonts nil)
  (shr-use-colors nil)
  (shr-bullet "• ")
  (shr-indentation 2)
  (shr-discard-aria-hidden t))

(use-package paren
  :ensure nil
  :config
  (show-paren-mode)
  :custom
  (show-paren-style 'mixed))

(use-package shell
  :ensure nil
  :hook
  (shell-mode-hook . (lambda ()
                       (toggle-truncate-lines t)
                       (setf comint-process-echoes t))))

(use-package better-shell
  :after shell
  :bind (:map hoagie-keymap
              ("`" . better-shell-for-current-dir)))

(use-package lisp-mode
  :ensure nil
  :hook
  (lisp-mode-hook . (lambda ()
                      (setf fill-column 100)
                      (display-fill-column-indicator-mode))))

(use-package sly
  :commands sly
  :custom
  (inferior-lisp-program "sbcl --dynamic-space-size 4096"))

(use-package sly-quicklisp
  :after sly)

(use-package sql
  :ensure nil
  :hook
  (sql-interactive-mode-hook . (lambda () (setf truncate-lines t))))

(use-package sql-datum :load-path "/home/hoagie/github/datum")

(use-package terraform-mode
  :mode "\\.tf$")

(use-package visible-mark
  :demand t ;; has to be loaded, no command
  :config
  (global-visible-mark-mode t)
  :custom-face
  (visible-mark-face1 ((t (:background "tomato"))))
  (visible-mark-face2 ((t (:background "gold"))))
  (visible-mark-forward-face1 ((t (:background "sea green"))))
  (visible-mark-forward-face2 ((t (:background "pale green"))))
  :custom
  (visible-mark-max 2)
  (visible-mark-faces '(visible-mark-face1 visible-mark-face2))
  (visible-mark-forward-max 2)
  (visible-mark-forward-faces '(visible-mark-forward-face1 visible-mark-forward-face2)))

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
  (defun hoagie-open-org (arg)
    (interactive "P")
    (let ((org-file (read-file-name "Open org file:" "~/org/")))
      (if arg
          (find-file-other-window org-file)
        (find-file org-file))))
  ;; from https://www.emacswiki.org/emacs/BackwardDeleteWord
  ;; because I agree C-backspace shouldn't kill the word!
  ;; it litters my kill ring
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
  :bind
  ("<S-f1>" . (lambda () (interactive) (find-file user-init-file)))
  ("<f1>" . hoagie-go-home)
  ("<f2>" . project-switch-project)
  ("<f3>" . hoagie-open-org)
  ;; from https://stackoverflow.com/a/6465415
  ("C-x 3" . (lambda () (interactive)(split-window-right) (other-window 1)))
  ("C-x 2" . (lambda () (interactive)(split-window-below) (other-window 1)))
  ;; Window management
  ("C-M-}" . (lambda () (interactive)(shrink-window-horizontally 5)))
  ("C-M-{" . (lambda () (interactive)(enlarge-window-horizontally 5)))
  ("C-M-_" . (lambda () (interactive)(shrink-window 5)))
  ("C-M-+" . (lambda () (interactive)(shrink-window -5)))
  ("M-o" . other-window)
  ("M-O" . other-frame)
  ("M-N" . next-buffer)
  ("M-P" . previous-buffer)
  ;; from https://emacsredux.com/blog/2020/06/10/comment-commands-redux/
  ("<remap> <comment-dwim>" . comment-line)
  ;; replace delete-char
  ("C-d" . delete-forward-char)
  ;; like backspace, but closer
  ("C-S-d" . delete-backward-char)
  ("M-c" . capitalize-dwim)
  ("M-u" . upcase-dwim)
  ("M-l" . downcase-dwim)
  ("C-<backspace>" . backward-delete-word)
  ;; like flycheck's C-c ! l
  ("C-c !" . flymake-show-diagnostics-buffer)
  :custom
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
  (enable-recursive-minibuffers t)
  (global-mark-ring-max 64)
  (mark-ring-max 64)
  (grep-command "grep --color=always -nHi -r --include=*.* -e \"pattern\" .")
  (inhibit-startup-screen t)
  (initial-buffer-choice t)
  (reb-re-syntax 'string)
  (initial-scratch-message
   ";; Il semble que la perfection soit atteinte non quand il n'y a plus rien à ajouter, mais quand il n'y a plus à retrancher. - Antoine de Saint Exupéry\n;; It seems that perfection is attained not when there is nothing more to add, but when there is nothing more to remove.\n\n")
  (save-interprogram-paste-before-kill t)
  (visible-bell t)
  ;; from https://gitlab.com/jessieh/dot-emacs
  (backup-by-copying t)   ; Don't delink hardlinks
  (version-control t)     ; Use version numbers on backups
  (delete-old-versions t) ; Do not keep old backups
  (kept-new-versions 5)   ; Keep 5 new versions
  (kept-old-versions 5)   ; Keep 3 old versions
  ;; Experimental - from LSP perf suggestions
  (gc-cons-threshold 100000000)
  (read-process-output-max (* 1024 1024))
  ;; from https://depp.brause.cc/dotemacs/
  (echo-keystrokes 0.25)
  :config
  ;; see https://emacs.stackexchange.com/a/28746/17066
  ;; https://blog.danielgempesaw.com/post/129841682030/fixing-a-laggy-compilation-buffer
  ;;
  (setf disabled-command-function nil
        w32-use-native-image-API t
        inhibit-compacting-font-caches t
        auto-window-vscroll nil
        compilation-error-regexp-alist (delete 'maven compilation-error-regexp-alist))
  (defalias 'yes-or-no-p 'y-or-n-p)
  ;; from http://www.jurta.org/en/emacs/dotemacs, set the major mode
  ;; of buffers that are not visiting a file
  (setq-default major-mode (lambda ()
                             (if buffer-file-name
                                 (fundamental-mode)
                               (let ((buffer-file-name (buffer-name)))
                                 (set-auto-mode)))))
  ;; https://200ok.ch/posts/2020-09-29_comprehensive_guide_on_handling_long_lines_in_emacs.html
  (setq-default bidi-paragraph-direction 'left-to-right)
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
  (defvar hoagie-container-name nil "Stores the name of the current container, if present.")
  (if (file-exists-p "/run/.containerenv")
      ;; from http://ergoemacs.org/emacs/elisp_read_file_content.html
      ;; insert content in temp buffer rather than open a file
      (with-temp-buffer
        (insert-file-contents "/run/.containerenv")
        (search-forward "name=") ;; move point to the line with the name
        (setf hoagie-container-name
              (cl-subseq (thing-at-point 'line) 6 -2)))
    (setf hoagie-container-name (system-name)))
  ;; Identify the toolbox container for this Emacs instance in the frame title
  (setf frame-title-format '(" %b @ " (:eval hoagie-container-name))
        icon-title-format '(" %b @ " (:eval hoagie-container-name))))

;; Convenient to work with AWS timestamps
(defun hoagie-convert-timestamp (&optional timestamp)
  "Convert a Unix TIMESTAMP (as string) to date.  If the parameter is not provided use word at point."
  (interactive)
  (setf timestamp (or timestamp (thing-at-point 'word t)))
  (let ((to-convert (if (< 10 (length timestamp)) (substring timestamp 0 10) timestamp))
        (millis (if (< 10 (length timestamp)) (substring timestamp 10 (length timestamp)) "000")))
    (message "%s.%s"
             (format-time-string "%Y-%m-%d %H:%M:%S"
                                 (seconds-to-time
                                  (string-to-number to-convert)))
             millis)))
(define-key hoagie-keymap (kbd "t") 'hoagie-convert-timestamp)

;; simplified version that restores stored window config and advices delete-other-windows
;; idea from https://erick.navarro.io/blog/save-and-restore-window-configuration-in-emacs/
(defvar hoagie-window-configuration nil "Last window configuration saved.")
(defun hoagie-restore-window-configuration ()
  "Use `hoagie-window-configuration' to restore the window setup."
  (interactive)
  (when hoagie-window-configuration
    (set-window-configuration hoagie-window-configuration)))
(define-key hoagie-keymap (kbd "1") #'hoagie-restore-window-configuration)
(defun hoagie-store-config (&rest _)
  "Store the current window configuration in `hoagie-window-configuration'."
  (setf hoagie-window-configuration (current-window-configuration)))
(advice-add 'delete-other-windows :before #'hoagie-store-config)

;; Per-OS configuration

(setf user-full-name "Sebastián Monía"
      user-mail-address "seb.hoagie@outlook.com")

(when (string= system-type "windows-nt")
  (global-set-key (kbd "M-`") #'other-frame) ;; Gnome-like frame switching in Windows
  (load "c:/repos/miscscripts/workonlyconfig.el"))
;; (load "/home/hoagie/repos/miscscripts/workonlyconfig.el")

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
           (monitor-font '(("S240HL" . 113) ;; 24"
                           ("2757" . 105) ;; 27"
                           ("LG HDR 4K" . 181))) ;; 27" office
           (size (alist-get monitor-name monitor-font
                            143 ;; default size, "big just in case"
                            nil
                            'equal)))
      ;; override for "laptop screen only"
      (when (eq (length (display-monitor-attributes-list)) 1)
        (setf size 120))
      (set-face-attribute 'default (selected-frame) :height size)
      (set-face-font 'eldoc-box-body
                     (frame-parameter nil 'font))))
  (add-hook 'window-size-change-functions #'hoagie-adjust-font-size))

;; MARK PUSH AND POP - maybe I should make a package out of this
;; For a long time I longed for the VS navigation commands as described in
;; https://blogs.msdn.microsoft.com/zainnab/2010/03/01/navigate-backward-and-navigate-forward/
;; By the time I finally found a way to implement them using the package back-button,
;; I started running into problems integrating with existing commands, and also I was too
;; used to navigating locations within the same buffer rather than globally.
;; So, rolling back :)

(defun push-mark-no-activate ()
  "Pushes `point` to `mark-ring' and does not activate the region.
Equivalent to \\[set-mark-command] when \\[transient-mark-mode] is disabled.
Source: https://masteringemacs.org/article/fixing-mark-commands-transient-mark-mode"
  (interactive)
  (push-mark (point) t nil)) ; removed the message, visible-mark takes care of this

(defun unpop-to-mark-command ()
  "Unpop off mark ring.  Does nothing if mark ring is empty.
Source: from https://www.emacswiki.org/emacs/MarkCommands#toc4"
  (interactive)
  (when mark-ring
    (let ((pos (marker-position (car (last mark-ring)))))
      (if (not (= (point) pos))
          (goto-char pos)
        (setf mark-ring (cons (copy-marker (mark-marker)) mark-ring))
        (set-marker (mark-marker) pos)
        (setf mark-ring (nbutlast mark-ring))
        (goto-char (marker-position (car (last mark-ring)))))
      (message "Mark unpopped"))))

;; TODO: try re-implementing using `push-mark-if-not-repeat'
(defun pop-to-mark-push-if-first ()
  "Pop the mark ring, but push a mark if this is a first invocation."
  (interactive)
  (unless (equal last-command 'pop-to-mark-push-if-first)
    (push-mark-no-activate)
    (pop-to-mark-command))
  (pop-to-mark-command))

(defun push-mark-if-not-repeat (command &rest _)
  "Push a mark if this is not a repeat invocation of COMMAND."
  (unless (equal last-command this-command)
    (push-mark-no-activate)))

;; manually setting the mark bindings
(global-set-key (kbd "C-<return>") #'push-mark-no-activate)
(global-set-key (kbd "M-<return>") #'pop-to-mark-push-if-first)
(global-set-key (kbd "M-S-<return>") #'unpop-to-mark-command)

;; Using advice instead of isearch-mode-end-hook, as the latter pushes mark first in search
;; destination, then in search start position.
;; Using the advice pushes first at start position, and then destination.
(require 'isearch)
(advice-add 'isearch-forward :after #'push-mark-no-activate)
(advice-add 'isearch-backward :after #'push-mark-no-activate)
(require 'window)
(advice-add 'scroll-up-command :before #'push-mark-if-not-repeat)
(advice-add 'scroll-down-command :before #'push-mark-if-not-repeat)

(use-package modus-operandi-theme
  :demand t
  :custom
  (modus-themes-completions 'moderate)
  (modus-themes-intense-hl-line t)
  :config
  (load-theme 'modus-operandi t)
  (enable-theme 'modus-operandi))

(use-package mood-line
  :demand t
  :init
  (mood-line-mode)
  (defun mood-line-segment-position ()
    "Displays the current cursor position in the mode-line, with region size if applicable."
    (let ((region-size (when (use-region-p)
                         (propertize (format " (%sL:%sC)"
                                             (count-lines (region-beginning)
                                                          (region-end))
                                             (- (region-end) (region-beginning)))
                                     'face 'mood-line-unimportant)))
          (position (propertize " %p%%" 'face 'mood-line-unimportant)))
    (list "%l:%c" position region-size))))

;;; init.el ends here
