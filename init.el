;; .emacs --- My dot emacs file

;; Author: Sebastian Monia <smonia@outlook.com>
;; URL: https://github.com/sebasmonia/.emacs
;; Version: 3
;; Keywords: .emacs dotemacs

;; This file is not part of GNU Emacs.

;;; Commentary:

;; My dot Emacs file
;; In theory I should be able to just drop the file in any computer and have
;; the config synced without merging/adapting anything
;; Update 2019-05-06: V3 means I moved to use-package

;;; Code:

(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Try to refresh package contents,  handle error and
;; print message if it fails (no internet connection?)
;;(condition-case err
;;  (package-refresh-contents)
;;  (error
;;   (message "%s" (error-message-string err))))

(require 'use-package)
(setq use-package-verbose t)
(setq use-package-always-ensure t)

(setq custom-file (concat user-emacs-directory "custom.el"))

(custom-set-faces
 '(default ((t (:family "Consolas" :foundry "outline" :slant normal :weight normal :height 78 :width normal)))))

;; based on http://www.ergoemacs.org/emacs/emacs_menu_app_keys.html
(defvar hoagie-keymap (define-prefix-command 'hoagie-keymap) "My custom bindings.")
(define-key key-translation-map (kbd "<apps>") (kbd "<menu>")) ;; compat Linux-Windows
(global-set-key (kbd "<menu>") 'hoagie-keymap)

(use-package 2048-game
  :commands 2048-game)

;; could be replaced by isearch-lazy-count...
(use-package anzu
  :bind
  (("<remap> <isearch-query-replace>" . anzu-isearch-query-replace)
   ("<remap> <isearch-query-replace-regexp>" . anzu-isearch-query-replace-regexp)
   ("<remap> <query-replace>" . anzu-query-replace)
   ("<remap> <query-replace-regexp>" . anzu-query-replace-regexp))
  :init
  (global-anzu-mode 1)
  :custom
  (anzu-deactivate-region t)
  (anzu-mode-lighter "")
  (anzu-replace-threshold 50)
  (anzu-replace-to-string-separator " => ")
  (anzu-search-threshold 1000))

(use-package browse-kill-ring
  :config
  (browse-kill-ring-default-keybindings))

(use-package company
  :bind
  ("M-S-<SPC>" . company-complete-common)
  (:map hoagie-keymap
        ("<SPC>" . company-complete-common))
  :hook (after-init . global-company-mode)
  :custom
  (company-idle-delay 0)
  (company-minimum-prefix-length 1)
  (company-selection-wrap-around t)
  :config
  (define-key company-active-map (kbd "C-<return>") #'company-abort)
  (define-key company-active-map [tab] #'company-complete-selection)
  (define-key company-active-map [tab] #'company-complete-selection)
  (define-key company-active-map (kbd "TAB") #'company-complete-selection)
  (define-key company-active-map (kbd "C-n") #'company-select-next)
  (define-key company-active-map (kbd "C-p") #'company-select-previous))

(use-package awscli-capf :load-path "~/github/awscli-capf"
  :commands (awscli-add-to-capf)
  :hook ((shell-mode . awscli-capf-add)
         (eshell-mode . awscli-capf-add)))

(use-package csharp-mode ;; manual load since I removed omnisharp
  :demand
  :hook
  (csharp-mode . (lambda ()
                        (subword-mode)
                        (setq-local fill-function-arguments-first-argument-same-line t)
                        (setq-local fill-function-arguments-second-argument-same-line nil)
                        (setq-local fill-function-arguments-last-argument-same-line t)
                        (define-key csharp-mode-map [remap c-fill-paragraph] 'fill-function-arguments-dwim))))

(use-package dired
  :ensure nil
  :custom
  (dired-dwim-target t)
  (dired-listing-switches "-laogGhvD")
  :config
  (progn
    (global-set-key (kbd "<f1>") (lambda () (interactive) (dired "~/")))
    (define-key hoagie-keymap (kbd "f") 'project-find-file)
    (define-key hoagie-keymap (kbd "F") 'find-name-dired)
    (defun hoagie-dired-jump (&optional arg)
      "Call dired-jump.  With prefix ARG, open in current window."
      (interactive "P")
      (let ((inverted (not arg)))
        (dired-jump inverted)))
    (define-key hoagie-keymap (kbd "j") 'hoagie-dired-jump)
    (define-key hoagie-keymap (kbd "J") (lambda () (interactive) (hoagie-dired-jump 4)))))

(use-package dired-narrow
  :after dired
  :bind
  (:map dired-mode-map
        ;; more standard binding for filtering,
        ;; but I'm so used to \, leaving both
        ("\\" . dired-narrow)
        ("/" . dired-narrow)))

(use-package dired-git-info
  :after dired
  :bind
  (:map dired-mode-map
        (")" . dired-git-info-mode)))

(use-package dired-sidebar
  :bind (("M-z" . dired-sidebar-toggle-sidebar))
  :commands (dired-sidebar-toggle-sidebar)
  :init
  (add-hook 'dired-sidebar-mode-hook
            (lambda ()
              (unless (file-remote-p default-directory)
                (auto-revert-mode))))
  :config
  (setq dired-sidebar-toggle-hidden-commands '(rotate-windows toggle-window-split balance-windows))
  (setq dired-sidebar-theme 'ascii)
  (setq dired-sidebar-subtree-line-prefix "__"))

(use-package deadgrep
  :bind
  (:map hoagie-keymap
        ("g" . deadgrep))
  :config
  (progn
    (defun deadgrep--format-command-patch (rg-command)
      "Add --hidden to rg-command."
      (replace-regexp-in-string "^rg " "rg --hidden " rg-command))
    (advice-add 'deadgrep--format-command :filter-return #'deadgrep--format-command-patch)))

(use-package docker
  :bind
  ("C-c d" . docker))

(use-package dockerfile-mode
  :demand t ;; not sure if really needed
  )

(use-package dotnet
  :demand t  ;; needed since the global keybinding has to be ready. I think.
  :config
  (progn
    (setq dotnet-mode-keymap-prefix nil)
    (define-key hoagie-keymap (kbd "n") dotnet-mode-command-map)))

(use-package ediff
  :demand
  :custom
  (ediff-forward-word-function 'forward-char) ;; from https://emacs.stackexchange.com/a/9411/17066
  (ediff-highlight-all-diffs t)
  (ediff-keep-variants nil)
  (ediff-window-setup-function 'ediff-setup-windows-plain)
  :config
  (progn
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
    (add-hook 'ediff-keymap-setup-hook 'add-d-to-ediff-mode-map)))

(use-package eglot
  :commands (eglot eglot-ensure)
  :hook ((python-mode . eglot-ensure)
         (csharp-mode . eglot-ensure))
  :bind
  (:map eglot-mode-map
        (("C-c e r" . eglot-rename)
         ("C-c e f" . eglot-format)
         ("C-c e h" . eglot-help-at-point)))
  :config
  (progn
    (add-to-list 'eglot-server-programs
                 `(csharp-mode . ("c:/home/omnisharp_64/omnisharp.exe" "-lsp")))
    ;; patch the argument. When nil, use "" instead.
    (defun eglot--format-markup-patch (args)
      (list (or (car args) "")))
    (advice-add 'eglot--format-markup :filter-args #'eglot--format-markup-patch)))

(use-package expand-region
  :bind
  ("M-<SPC>" . er/expand-region)
  :config
  (er/enable-mode-expansions 'csharp-mode 'er/add-cc-mode-expansions))

(use-package eww-lnum
  :config
  '(progn (define-key eww-mode-map "f" 'eww-lnum-follow)
          (define-key eww-mode-map "F" 'eww-lnum-universal)))

(use-package fill-function-arguments
  :commands (fill-function-arguments-dwim)
  :custom
  (fill-function-arguments-indent-after-fill t)
  :config
  (progn
    ;; taken literally from the project's readme.
    ;; reformat for more use-packageness if this sticks
    (add-hook 'prog-mode-hook (lambda () (local-set-key (kbd "M-q") #'fill-function-arguments-dwim)))
    (add-hook 'sgml-mode-hook (lambda ()
                          (setq-local fill-function-arguments-first-argument-same-line t)
                          (setq-local fill-function-arguments-argument-sep " ")
                          (local-set-key (kbd "M-q") #'fill-function-arguments-dwim)))
    (add-hook 'emacs-lisp-mode-hook (lambda ()
                                  (setq-local fill-function-arguments-first-argument-same-line t)
                                  (setq-local fill-function-arguments-second-argument-same-line t)
                                  (setq-local fill-function-arguments-last-argument-same-line t)
                                  (setq-local fill-function-arguments-argument-separator " ")
                                  (local-set-key (kbd "M-q") #'fill-function-arguments-dwim)))))

(use-package format-all
  :bind ("C-c f" . format-all-buffer))

(use-package gud-cdb :load-path "~/.emacs.d/lisp/"
  :commands (cdb))

(use-package hl-line
  :init
  (global-hl-line-mode t))

(use-package ibuffer
  :bind ("C-x C-b" . ibuffer-other-window)
  :custom
  (ibuffer-default-sorting-mode 'major-mode)
  (ibuffer-expert t)
  (ibuffer-show-empty-filter-groups nil))

(use-package ibuffer-vc
  :demand t
  :after ibuffer
  :hook (ibuffer-mode . (lambda ()
                          (ibuffer-vc-set-filter-groups-by-vc-root)
                          (unless (eq ibuffer-sorting-mode 'alphabetic)
                            (ibuffer-do-sort-by-alphabetic))))
  :init
  (setq ibuffer-formats '((mark modified read-only vc-status-mini " "
                                (name 18 18 :left :elide)
                                " "
                                (size 9 -1 :right)
                                " "
                                (mode 16 16 :left :elide)
                                " "
                                (vc-status 16 16 :left)
                                " "
                                vc-relative-file))))

(use-package ido
  :init
  (progn
    (ido-mode 1)
    (use-package ido-vertical-mode
      :config
      (ido-vertical-mode 1)
      :custom
      (ido-vertical-define-keys 'c-n-and-c-p-only)))
  :custom
  (ido-enable-flex-matching t)
  (ido-everywhere t)
  (ido-create-new-buffer 'always)
  (ido-default-buffer-method 'selected-window))

(use-package ido-completing-read+
  :demand t
  :init
  (ido-ubiquitous-mode 1))

(use-package idomenu
  :bind ("M-g d" . idomenu))

(use-package json-mode
  :mode "\\.json$")

(use-package lyrics
  :commands lyrics)

(use-package magit
  :init
  :bind
  ("C-x g" . magit-status)
  :hook
  (magit-mode . turn-on-magit-gitflow))

(use-package magit-gitflow
  :after magit
  :init
  (progn
    (setq magit-gitflow-popup-key "C-;"))
  :config
  (progn
    (add-hook 'magit-mode-hook 'turn-on-magit-gitflow)))

(use-package git-timemachine
  :bind ("C-x M-G" . git-timemachine))

(use-package minions
  :config
  (minions-mode 1)
  :custom
  (minions-mode-line-lighter "^"))

(use-package package-lint
  :commands package-lint-current-buffer)

(use-package plantuml-mode
  :commands plantuml-mode
  :mode (("\\.puml$" . plantuml-mode)
	 ("\\.plantuml$" . plantuml-mode))
  :config
  (setq plantuml-jar-path "~/plantuml.jar"))

(use-package powershell
  :bind
  ;; this one shadows the command to go back in
  ;; the mark ring
  (:map powershell-mode-map
        ("M-`" . nil)))

(use-package python
  :ensure nil
  :custom
  (python-shell-interpreter "ipython")
  (python-shell-interpreter-args "--pprint --simple-prompt --no-color-info"))

(use-package replace
  :ensure nil
  :config
  (progn
    ;; I'm surprised this isn't the default behaviour,
    ;; also couldn't find a way to change it from options
    (defun hoagie-occur-dwim ()
      "Run occur, if there's a region selected use that as input."
      (interactive)
      (if (use-region-p)
          (occur (buffer-substring-no-properties (region-beginning) (region-end)))
        (command-execute 'occur)))
    (define-key hoagie-keymap (kbd "o") 'hoagie-occur-dwim)))

(use-package shell
  :init
  (use-package better-shell
    :bind (:map hoagie-keymap
                ("`" . better-shell-for-current-dir)))
  :hook
  (shell-mode . (lambda ()
                  (toggle-truncate-lines t))))

(use-package sly
  :commands sly
  :config
  (setq inferior-lisp-program "sbcl")
  (use-package sly-quicklisp))

(use-package amx
  :demand t
  :commands (amx-mode amx)
  :custom
  (amx-backend 'ido)
  (amx-history-length 25)
  :config
  (progn
    (define-key hoagie-keymap (kbd "<menu>") #'amx)
    (amx-mode)))

(use-package speed-type
  :commands (speed-type-text speed-type-region speed-type-buffer))

(use-package terraform-mode
  :mode "\\.tf$"
  :config
  (add-hook 'terraform-mode-hook #'terraform-format-on-save-mode))

(use-package visible-mark
  :demand t ;; has to be loaded, no command
  :config
  (global-visible-mark-mode t)
  :custom-face
  (visible-mark-face1 ((t (:box (:line-width 2 :color "red")))))
  (visible-mark-face2 ((t (:box (:line-width 1 :color "orange")))))
  (visible-mark-forward-face1 ((t (:box (:line-width 2 :color "chartreuse")))))
  (visible-mark-forward-face2 ((t (:box (:line-width 1 :color "purple1")))))
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
  :init
  :custom
  (web-mode-enable-css-colorization t)
  (web-mode-enable-sql-detection t)
  (web-mode-enable-current-element-highlight t)
  (web-mode-markup-indent-offset 2))

(use-package which-key
  :config
  (progn
    (which-key-mode)
    (which-key-setup-side-window-right-bottom))
  :custom
  (which-key-side-window-max-width 0.4)
  (which-key-sort-order 'which-key-prefix-then-key-order))

(use-package ws-butler
  :hook (prog-mode . ws-butler-mode))

(use-package yaml-mode
  :mode "\\.yml$")

;; MISC STUFF THAT IS NOT IN CUSTOMIZE (or easier to customize here)
;; and stuff that I moved from Custom to here hehehehe
(defalias 'yes-or-no-p 'y-or-n-p)
(setq frame-title-format "%b - Emacs")
(setq inhibit-compacting-font-caches t)
; see https://emacs.stackexchange.com/a/28746/17066
(setq auto-window-vscroll nil)
;; behaviour for C-l. I prefer one extra line rather than top & bottom
;; and also start with the top position, which I found more useful
(setq recenter-positions '(1 middle -2))
;; Substring is matchier than basic but not as much as flex
(setq completion-styles '(substring basic emacs22))
;; helps with company and capf all the same
(setq completion-ignore-case t)
;; Useful in Linux
(setq read-file-name-completion-ignore-case t)
; from https://emacs.stackexchange.com/questions/7362/how-to-show-a-diff-between-two-buffers-with-character-level-diffs
(setq-default ediff-forward-word-function 'forward-char)
;; helps compilation buffer not slowdown
;; see https://blog.danielgempesaw.com/post/129841682030/fixing-a-laggy-compilation-buffer
(setq compilation-error-regexp-alist
      (delete 'maven compilation-error-regexp-alist))
(add-hook 'sql-interactive-mode-hook (lambda () (setq truncate-lines t)))
;; from http://www.jurta.org/en/emacs/dotemacs, set the major mode
;; of buffers that are not visiting a file
(setq-default major-mode (lambda ()
                           (if buffer-file-name
                               (fundamental-mode)
                             (let ((buffer-file-name (buffer-name)))
                               (set-auto-mode)))))
;; Better defaults from https://github.com/jacmoe/emacs.d/blob/master/jacmoe.org
(setq help-window-select t)
(add-hook 'focus-out-hook 'garbage-collect)
;; From https://github.com/wasamasa/dotemacs/blob/master/init.org
(setq line-number-display-limit-width 10000)
(setq comint-prompt-read-only t)
(defun my-shell-turn-echo-off ()
  (setq comint-process-echoes t))
(add-hook 'shell-mode-hook 'my-shell-turn-echo-off)
;; tired of this question. Sorry not sorry:
(setq custom-safe-themes t)
;; Separate from the "~" shortcut
(global-set-key (kbd "<S-f1>") (lambda () (interactive) (find-file user-init-file)))
;; What was in custom that didn't get use-package'd:
(tool-bar-mode 0)
(scroll-bar-mode 0)
(menu-bar-mode 0)
(delete-selection-mode)
(blink-cursor-mode -1)
(column-number-mode 1)
(horizontal-scroll-bar-mode -1)
(savehist-mode)
(setq-default indent-tabs-mode nil)
(setq
      dabbrev-case-distinction nil
      dabbrev-case-fold-search t
      dabbrev-case-replace nil
      default-frame-alist '((fullscreen . maximized) (vertical-scroll-bars . nil) (horizontal-scroll-bars . nil))
      delete-by-moving-to-trash t
      enable-recursive-minibuffers t
      global-mark-ring-max 32
      grep-command "grep --color=always -nHi -r --include=*.* -e \"pattern\" ."
      inhibit-startup-screen t
      initial-buffer-choice t
      initial-scratch-message
      ";; Il semble que la perfection soit atteinte non quand il n'y a plus rien à ajouter, mais quand il n'y a plus à retrancher. - Antoine de Saint Exupéry\n;; It seems that perfection is attained not when there is nothing more to add, but when there is nothing more to remove.\n\n"
      mark-ring-max 32
      proced-filter 'all
      save-interprogram-paste-before-kill t
      set-mark-command-repeat-pop t
      visible-bell t)

(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)

;; ;; from https://stackoverflow.com/a/22176971, move auto saves and
;; ;; back up files to a different folder so git or dotnet core won't
;; ;; pick them up as changes or new files in the project
(make-directory (concat user-emacs-directory "auto-save") t)
(setq auto-save-file-name-transforms
      `((".*" ,(concat user-emacs-directory "auto-save/") t)))

(make-directory (concat user-emacs-directory "backups") t)
(setq backup-directory-alist
      `(("." . ,(expand-file-name
                 (concat user-emacs-directory "backups/")))))

;; OTHER BINDINGS
; adapted for https://stackoverflow.com/questions/6464738/how-can-i-switch-focus-after-buffer-split-in-emacs
(global-set-key (kbd "C-x 3") (lambda () (interactive)(split-window-right) (other-window 1)))
(global-set-key (kbd "C-x 2") (lambda () (interactive)(split-window-below) (other-window 1)))
(global-set-key (kbd "C-M-}") (lambda () (interactive)(shrink-window-horizontally 5)))
(global-set-key (kbd "C-M-{") (lambda () (interactive)(enlarge-window-horizontally 5)))
(global-set-key (kbd "C-M-_") (lambda () (interactive)(shrink-window 5)))
(global-set-key (kbd "C-M-+") (lambda () (interactive)(shrink-window -5)))
(global-set-key (kbd "M-o") 'other-window)
(global-set-key (kbd "M-O") 'other-frame)
(global-set-key (kbd "M-N") 'next-buffer)
(global-set-key (kbd "M-P") 'previous-buffer)
(global-set-key (kbd "C-d") 'delete-forward-char) ;; replace delete-char
(global-set-key (kbd "M-c") 'capitalize-dwim)
(global-set-key (kbd "M-u") 'upcase-dwim)
(global-set-key (kbd "M-l") 'downcase-dwim)
(define-key hoagie-keymap (kbd "b") 'browse-url-at-point)
(define-key hoagie-keymap (kbd "t") 'toggle-truncate-lines)
;; used to be C-x K. Honestly I never used C-x C-k (macros) commands that much so :shrug:
;; without the lambda it would simply show the menu like C-x k
(defun hoagie-kill-this-buffer ()
  "Kill the current buffer.
If defined as a lambda then it shows a ? in the bindings list."
  (interactive)
  (kill-buffer))
(define-key hoagie-keymap (kbd "k") 'hoagie-kill-this-buffer)
(global-set-key (kbd "C-c !") 'flymake-show-diagnostics-buffer) ;; like flycheck's C-c ! l
(global-set-key (kbd "C-;") 'dabbrev-expand)
(global-set-key (kbd "<f6>") 'kmacro-start-macro)
(global-set-key (kbd "<f7>") 'kmacro-end-macro)
(global-set-key (kbd "<f8>") 'kmacro-end-and-call-macro)
(global-set-key (kbd "<mouse-3>") 'kill-ring-save)

(defun hoagie-kill-buffer-filename ()
  "Sends the current buffer's filename to the kill ring."
  (interactive)
  (let ((name (buffer-file-name)))
    (when name
      (kill-new name))
    (message (format "Filename: %s" (or name "-No file for this buffer-")))))
(global-set-key (kbd "<C-f1>") 'hoagie-kill-buffer-filename)
(define-key dired-mode-map (kbd "<C-f1>") (lambda () (interactive) (dired-copy-filename-as-kill 0)))

;; from https://www.emacswiki.org/emacs/BackwardDeleteWord
;; because I agree C-backspace shouldn't kill the word!
;; it litters my kill ring
(defun delete-word (arg)
  "Delete characters forward until encountering the end of a word.
With ARG, do this that many times."
  (interactive "p")
  (if (use-region-p)
      (delete-region (region-beginning) (region-end))
    (delete-region (point) (progn (forward-word arg) (point)))))
(defun backward-delete-word (arg)
  "Delete characters backward until encountering the end of a word.
With ARG, do this that many times."
  (interactive "p")
  (delete-word (- arg)))
(global-set-key (kbd "C-<backspace>") 'backward-delete-word)

;; Convenient to work with AWS timestamps
(defun hoagie-convert-timestamp (&optional timestamp)
  "Convert a Unix TIMESTAMP (as string) to date.  If the parameter is not provided use word at point."
  (interactive)
  (setq timestamp (or timestamp (thing-at-point 'word t)))
  (let ((to-convert (if (< 10 (length timestamp)) (substring timestamp 0 10) timestamp))
        (millis (if (< 10 (length timestamp)) (substring timestamp 10 (length timestamp)) "000")))
    (message "%s.%s"
             (format-time-string "%Y-%m-%d %H:%M:%S"
                                 (seconds-to-time
                                  (string-to-number to-convert)))
             millis)))
(define-key hoagie-keymap (kbd "t") 'hoagie-convert-timestamp)

;; MARK PUSH AND POP - should make a package out of this
;; including a macro or common func to "push a mark if first time"

;; from: https://masteringemacs.org/article/fixing-mark-commands-transient-mark-mode
(defun push-mark-no-activate ()
  "Pushes `point` to `mark-ring' and does not activate the region.
Equivalent to \\[set-mark-command] when \\[transient-mark-mode] is disabled"
  (interactive)
  (push-mark (point) t nil)) ; removed the message, visible-mark takes care of this

;; from https://www.emacswiki.org/emacs/MarkCommands#toc4
(defun unpop-to-mark-command ()
  "Unpop off mark ring.  Does nothing if mark ring is empty."
  (interactive)
  (when mark-ring
    (let ((pos (marker-position (car (last mark-ring)))))
      (if (not (= (point) pos))
          (goto-char pos)
        (setq mark-ring (cons (copy-marker (mark-marker)) mark-ring))
        (set-marker (mark-marker) pos)
        (setq mark-ring (nbutlast mark-ring))
        (goto-char (marker-position (car (last mark-ring))))))))

;; Author: me XD
(defun pop-to-mark-push-if-first ()
  "Pop the mark ring, but push a mark if this is a first invocation."
  ;; The idea is these commands bring me closer to C-- C-_ in Visual Studio
  ;; But per-buffer :)
  (interactive)
  (unless (equal last-command 'pop-to-mark-push-if-first)
    (push-mark-no-activate)
    (pop-to-mark-command))
  (pop-to-mark-command))

;; manually setting the mark bindings
(global-set-key (kbd "C-`") 'push-mark-no-activate)
(global-set-key (kbd "M-`") 'pop-to-mark-push-if-first)
(global-set-key (kbd "M-~") 'unpop-to-mark-command)

;; from https://blogs.msdn.microsoft.com/zainnab/2010/03/01/navigate-backward-and-navigate-forward/
;; I finally know the conditions that trigger adding a marker in Visual Studio. I used those a lot.
;; The hook below pushes the mark when exiting isearch to match #1 in that post
;; UPDATE: converted hook to advice as per https://github.com/abo-abo/swiper/issues/2128
;; UPDATE 2: kept advice but with isearch, as using the hook pushed mark first in search destination, then
;; in search start position. Using the advice pushes first at destination then at search start.
;;the idea with this is similar to the "11 lines away" comment in the post above
(require 'isearch)
(advice-add 'isearch-forward :after #'push-mark-no-activate)
(advice-add 'isearch-backward :after #'push-mark-no-activate)
;; (setq isearch-lazy-count t) ;; new in Emacs 27!

;; the idea with the next two functions is similar to the "11 lines away" comment in the post above
(defun hoagie-scroll-down-with-mark ()
  "Like `scroll-down-command`, but push a mark if this is not a repeat invocation."
  (interactive)
  (unless (equal last-command 'hoagie-scroll-down-with-mark)
    (push-mark-no-activate))
  (scroll-down-command))

(defun hoagie-scroll-up-with-mark ()
  "Like `scroll-up-command`, but push a mark if this is not a repeat invocation."
  (interactive)
  (unless (equal last-command 'hoagie-scroll-up-with-mark)
    (push-mark-no-activate))
  (scroll-up-command))

(global-set-key (kbd "C-v") 'hoagie-scroll-up-with-mark)
(global-set-key (kbd "M-v") 'hoagie-scroll-down-with-mark)

;; Emacs window management

;; Using the code in link below as starting point:
;; https://protesilaos.com/dotemacs/#h:3d8ebbb1-f749-412e-9c72-5d65f48d5957
;; My config is a lot simpler for now. Just display most things below, use
;; 1/3rd of the screen. On the left shell/xref on the left and on the right
;; compilation/help/messages and a few others
(setq display-buffer-alist
      '(;; stuff that splits to the right
        ("\\(magit\\|somethingelse\\).*"
         (display-buffer-reuse-window
          display-buffer-in-direction)
         (window-width . 0.5)
         (direction . right))
        ;; bottom left side window
        ("\\*\\(e?shell.*\\|xref\\)"
         (display-buffer-in-side-window)
         (window-height . 0.33)
         (side . bottom)
         (slot . 0))
        ;; bottom right side window - reuse if in another frame
        ("\\*\\(Backtrace\\|Warnings\\|compilation\\|[Hh]elp\\|Messages\\|Flymake.*\\|eglot.*\\)\\*"
         (display-buffer-reuse-window
          display-buffer-in-side-window)
         (window-height . 0.33)
         (reusable-frames . visible)
         (side . bottom)
         (slot . 1))))

;; function from https://lunaryorn.com/2015/04/29/the-power-of-display-buffer-alist.html
;; (via wayback machine)
(defun hoagie-quit-side-windows ()
  "Quit side windows of the current frame."
  (interactive)
  (dolist (window (window-at-side-list))
    (quit-window nil window)))

;; from https://stackoverflow.com/a/33456622/91877, just like ediff's |
(defun toggle-window-split ()
  "Swap two windows between vertical and horizontal split."
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
         (next-win-buffer (window-buffer (next-window)))
         (this-win-edges (window-edges (selected-window)))
         (next-win-edges (window-edges (next-window)))
         (this-win-2nd (not (and (<= (car this-win-edges)
                     (car next-win-edges))
                     (<= (cadr this-win-edges)
                     (cadr next-win-edges)))))
         (splitter
          (if (= (car this-win-edges)
             (car (window-edges (next-window))))
          'split-window-horizontally
        'split-window-vertically)))
    (delete-other-windows)
    (let ((first-win (selected-window)))
      (funcall splitter)
      (if this-win-2nd (other-window 1))
      (set-window-buffer (selected-window) this-win-buffer)
      (set-window-buffer (next-window) next-win-buffer)
      (select-window first-win)
      (if this-win-2nd (other-window 1))))))
(global-set-key (kbd "C-M-|") 'toggle-window-split)

;; simplified version that restores stored window config and advices delete-other-windows
;; idea from https://erick.navarro.io/blog/save-and-restore-window-configuration-in-emacs/
(defvar hoagie-window-configuration nil "Last window configuration saved.")
(defun hoagie-restore-window-configuration ()
  "Use `hoagie-window-configuration' to restore the window setup."
  (interactive)
  (when hoagie-window-configuration
    (set-window-configuration hoagie-window-configuration)))
(define-key hoagie-keymap (kbd "1") #'hoagie-restore-window-configuration)
(advice-add 'delete-other-windows :before (lambda () (setq hoagie-window-configuration (current-window-configuration))))

(define-key hoagie-keymap (kbd "0") #'hoagie-quit-side-windows)

;; Trial stuff
(defun hoagie-move-buffer-other-frame ()
  "Send the buffer to the next frame.  If no other frame, behave like C-x 5 b."
  (interactive)
  (let ((this-buffer (buffer-name))
        (frame-count (length (frame-list))))
    (if (equal frame-count 1)
        (switch-to-buffer-other-frame this-buffer)
      (other-frame 1) ;; go away
      (switch-to-buffer this-buffer) ;; change it
      (other-frame 1)) ;; come back
    (switch-to-prev-buffer)))

(global-set-key (kbd "C-M-O") 'hoagie-move-buffer-other-frame)

;; THEMES

(use-package modus-vivendi-theme
  :demand t)

(use-package modus-operandi-theme
  :demand t)

(use-package challenger-deep-theme
  :demand t)

(defun hoagie-load-theme (new-theme)
  "Pick a theme to load from a harcoded list. Or load NEW-THEME."
  (interactive (list (completing-read "Theme:"
                                      '(modus-vivendi
                                        modus-operandi
                                        challenger-deep)
                                      nil
                                      t)))
    (mapc 'disable-theme custom-enabled-themes)
    (load-theme (intern new-theme) t))

(global-set-key (kbd "C-<f11>") #'hoagie-load-theme)
(hoagie-load-theme "challenger-deep")

(use-package mood-line
  :demand t
  :init
  (mood-line-mode)
  (defun mood-line-segment-position ()
    "Displays the current cursor position in the mode-line, with region size if applicable."
    (let ((region-size (when (use-region-p)
                         (format " (%sl:%sc)"
                                 (count-lines (region-beginning)
                                              (region-end))
                                 (- (region-end) (region-beginning))))))
    (list "%l:%c" region-size))))

;; Per-OS configuration

(when (string= system-type "windows-nt")
  (load "c:/repos/miscscripts/workonlyconfig.el"))

(when (string= system-type "gnu/linux")
  (defun find-alternative-file-with-sudo ()
    (interactive)
    (let ((fname (or buffer-file-name
		     dired-directory)))
      (when fname
        (if (string-match "^/sudo:root@localhost:" fname)
	    (setq fname (replace-regexp-in-string
		         "^/sudo:root@localhost:" ""
		         fname))
	  (setq fname (concat "/sudo:root@localhost:" fname)))
        (find-alternate-file fname))))
  (global-set-key (kbd "C-x F") 'find-alternative-file-with-sudo))
