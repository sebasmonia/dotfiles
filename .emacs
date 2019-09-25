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

(setq custom-file "~/.emacscustom.el")
(load custom-file)

;; based on http://www.ergoemacs.org/emacs/emacs_menu_app_keys.html
(defvar hoagie-keymap (define-prefix-command 'hoagie-keymap) "My custom bindings.")
(define-key key-translation-map (kbd "<apps>") (kbd "<menu>")) ;; compat Linux-Windows
(global-set-key (kbd "<menu>") 'hoagie-keymap)

(use-package 2048-game
  :commands 2048-game)

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
  :config
  (define-key company-active-map [return] nil)
  (define-key company-active-map (kbd "C-<return>") 'company-complete-selection)
  (define-key company-active-map [tab] 'company-complete-selection)
  (define-key company-active-map [tab] 'company-complete-selection)
  (define-key company-active-map (kbd "TAB") 'company-complete-selection))

(use-package awscli-capf :load-path "~/github/awscli-capf"
  :commands (awscli-add-to-capf)
  :hook ((shell-mode . awscli-capf-add)
         (eshell-mode . awscli-capf-add)))

(use-package csharp-mode ;; manual load since I removed omnisharp
  :demand
  :hook
  (csharp-mode-hook . (lambda () (setq-local fill-function-arguments-first-argument-same-line t)
                                 (setq-local fill-function-arguments-second-argument-same-line t)
                                 (setq-local fill-function-arguments-last-argument-same-line t)
                                 (define-key csharp-mode-map [remap c-fill-paragraph] 'fill-function-arguments-dwim))))

(use-package dired
  :ensure nil
  :custom
  (dired-dwim-target t)
  (dired-listing-switches "-laogGhvD")
  :config
  (progn
    (global-set-key [f1] (lambda () (interactive) (dired "~/")))
    (defun hoagie-find-name-project-root ()
      "Call find-name-dired with a broad pattern and using project.el if available."
      (interactive)
      (let ((root-dir (or (cdr (project-current)) default-directory))
            (partial-name (read-string "Partial filename: ")))
        (find-name-dired root-dir (format "*%s*" partial-name))))
    (define-key hoagie-keymap (kbd "f") 'hoagie-find-name-project-root)
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
  :config
  (progn
    (define-key eglot-mode-map (kbd "C-c e r") 'eglot-rename)
    (define-key eglot-mode-map (kbd "C-c e f") 'eglot-format)
    (define-key eglot-mode-map (kbd "C-c e h") 'eglot-help-at-point)
    (add-to-list 'eglot-server-programs
                 `(csharp-mode . ("C:/Home/omnisharp_64/OmniSharp.exe" "-lsp")))
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
  :hook (ibuffer-mode . (lambda ()
                          (ibuffer-auto-mode 1)
                          (ibuffer-switch-to-saved-filter-groups "home")))
  :custom
  (ibuffer-default-sorting-mode 'major-mode)
  (ibuffer-expert t)
  (ibuffer-show-empty-filter-groups nil))

(use-package ido
  :init
  (progn
    (ido-mode 1)
    (use-package ido-vertical-mode
      :config
      (ido-vertical-mode 1)
      :custom
      (ido-vertical-define-keys 'c-n-and-c-p-only)
      (ido-use-virtual-buffers t)))
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

(use-package projectile
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  (projectile-mode)
  :custom
  (projectile-completion-system 'ido)
  (projectile-indexing-method 'alien)
  (projectile-switch-project-action 'projectile-find-file-dwim))

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

;; (use-package shell
;;   :init
;;   (use-package better-shell
;;     :bind (:map hoagie-keymap
;;                 ("`" . better-shell-for-current-dir)))
;;   :hook
;;   (shell-mode . (lambda ()
;;                   (toggle-truncate-lines t))))

(use-package eshell
  :demand nil
  :hook
  (eshell-mode . (lambda ()
                  (toggle-truncate-lines t)))
  :config
  (progn
    (defun hoagie-eshell-to-current-dir ()
      "Pop eshell to current dir.
Based on https://www.reddit.com/r/emacs/comments/1zkj2d/advanced_usage_of_eshell/cfuhl2x?utm_source=share&utm_medium=web2x"
      (interactive)
      (let ((dir default-directory)
            (eshell-buf (or (get-buffer "*eshell*") (eshell))))
        (with-current-buffer eshell-buf
          (goto-char (point-max))
          (eshell/pushd ".")
          (cd dir)
          (goto-char (point-max))
          (eshell-kill-input)
          (goto-char (point-max))
          (eshell-send-input))
        (pop-to-buffer eshell-buf)))
    (define-key hoagie-keymap (kbd "`") 'hoagie-eshell-to-current-dir)
    (with-eval-after-load 'eshell
      (defun hoagie-eshell-prompt ()
        (concat (eshell/pwd) "\n$ "))
      (setq eshell-prompt-regexp "^[^#$\n]*[#$] "
            eshell-prompt-function #'hoagie-eshell-prompt))))
      ;; (eshell/alias "ff" "find-file $1")
      ;; (eshell/alias "ffo" "find-file-other-window $1")
      ;; (eshell/alias "d" "dired $1"))))

(use-package sly
  :commands sly
  :config
  (setq inferior-lisp-program "sbcl")
  (use-package sly-quicklisp))

(use-package smex
  :init
  (smex-initialize)
  :bind
  (("M-x" . smex)
   :map hoagie-keymap
   ("<menu>" . smex)))

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
  :custom
  (visible-mark-max 2)
  (visible-mark-faces '(visible-mark-face1 visible-mark-face2))
  (visible-mark-forward-max 2)
  (visible-mark-forward-faces '(visible-mark-forward-face1 visible-mark-forward-face2)))

(use-package vlf
  :ensure t
  :config
  (require 'vlf-setup))

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
(defalias 'yes-or-no-p 'y-or-n-p)
(setq frame-title-format "%b - Emacs")
(setq inhibit-compacting-font-caches t)
; see https://emacs.stackexchange.com/a/28746/17066
(setq auto-window-vscroll nil)
;; behaviour for C-l. I prefer one extra line rather than top & bottom
;; and also start with the top position, which I found more useful
(setq recenter-positions '(1 middle -2))
;; Flex is a newer completion style that works as advertised...flex
(setq completion-styles '(flex basic emacs22))
;; helps with company and capf all the same
(setq completion-ignore-case t)
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

;; ;; from https://stackoverflow.com/a/22176971, move auto saves and
;; ;; back up files to a different folder so git or dotnet core won't
;; ;; pick them up as changes or new files in the project
(make-directory (concat user-emacs-directory "auto-save") t)
(setq auto-save-file-name-transforms
      `((".*" ,(concat user-emacs-directory "auto-save/") t)))

(make-directory (concat user-emacs-directory "backups") t)
(setq backup-directory-alist
      `(("." . ,(expand-file-name
                 (concat user-emacs-directory "backups")))))

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

(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)


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
      (other-frame 1)))) ;; come back

(global-set-key (kbd "C-M-O") 'hoagie-move-buffer-other-frame)

(define-key hoagie-keymap (kbd "0") 'kill-buffer-and-window)

;; simplified version that toggles full focus vs stored window config from
;; https://erick.navarro.io/blog/save-and-restore-window-configuration-in-emacs/
(defvar hoagie-window-configuration nil "Last window configuration saved.")
(defun hoagie-toggle-focus-windows ()
  "Toggle between focusing on a single window or restoring an existing config.
Uses `hoagie-window-configuration' to store the current setup."
  (interactive)
  (if hoagie-window-configuration
      (progn
        (set-window-configuration hoagie-window-configuration)
        (setq hoagie-window-configuration nil))
    (setq hoagie-window-configuration (current-window-configuration))
    (delete-other-windows)))
(define-key hoagie-keymap (kbd "1") 'hoagie-toggle-focus-windows)

(use-package challenger-deep-theme
  :demand t)

(use-package pastelmac-theme
  :init
  (load-theme 'pastelmac t))

(defun hoagie-toggle-lights ()
  "Swap my preferred light and dark themes."
  (interactive)
  (let* ((themes '(challenger-deep pastelmac))
         (current (car custom-enabled-themes))
         (to-load (car (remove current themes))))
    (mapc #'disable-theme custom-enabled-themes)
    (load-theme to-load t)))
(global-set-key (kbd "C-<f11>") 'hoagie-toggle-lights)

(use-package mood-line
  :demand t
  :config
  (mood-line-mode))

;; Updates to run after a mood-line update...until they are added
;; or I just branch the package. AND BYTE-COMPILE AFTERWARDS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; I like this better than the default. No "Top" "Bottom" + region size ;;
;; (defun mood-line-segment-position ()                                    ;;
;;   "Displays the current cursor position in the mode-line."              ;;
;;   (let ((region-size (when (use-region-p)                               ;;
;;                        (format " (%sL:%sC)"                             ;;
;;                                (count-lines (region-beginning)          ;;
;;                                             (region-end))               ;;
;;                                (- (region-end) (region-beginning))))))  ;;
;;     (list "%l:%c" region-size)))                                        ;;
;;                                                                         ;;
;; ;; replace  mood-line-segment-position with these two:                  ;;
;; (:eval (mood-line-segment-position))                                    ;;
;; (:eval (lambda () mode-line-misc-info))))                               ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
