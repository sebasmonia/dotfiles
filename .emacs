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
(package-refresh-contents)
;; Try to refresh package contents,  handle error and
;; print message if it fails (no internet connection?)
;;(condition-case err
;;  (package-refresh-contents)
;;  (error
;;   (message "%s" (error-message-string err))))

(package-install 'use-package)

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
  :hook (after-init . global-company-mode)
  :custom
  (company-idle-delay 0.1)
  (company-minimum-prefix-length 2))

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

(use-package expand-region
  :bind
  ("M-<SPC>" . er/expand-region)
  :config
  (er/enable-mode-expansions 'csharp-mode 'er/add-cc-mode-expansions))

(use-package eww-lnum
  :config
  '(progn (define-key eww-mode-map "f" 'eww-lnum-follow)
          (define-key eww-mode-map "F" 'eww-lnum-universal)))

(use-package dired
  :ensure nil
  :custom
  (dired-dwim-target t)
  (dired-listing-switches "-laogGhvD")
  :config
  (progn
    (global-set-key [f1] (lambda () (interactive) (dired "~/")))
    (define-key hoagie-keymap (kbd "f") 'find-name-dired)
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
        ("g" . deadgrep)))

(use-package docker
  :bind
  ("C-c d" . docker))

(use-package dockerfile-mode
  :demand t ;; not sure if really needed
  )

(use-package telephone-line
  :config
  (progn
    (defface telephone-line-extra-accent-active
      '((t (:foreground "white" :background "dark slate blue" :inherit mode-line)))
      "Extra accent face for my telephone-line."
      :group 'telephone-line)

    (defface telephone-line-extra-accent-inactive
      '((t (:foreground "white" :background "grey14" :inherit mode-line-inactive)))
      "Extra accent face for my telephone-line."
      :group 'telephone-line)

    (defface telephone-line-buffer-modified-face
      '((t (:foreground "white" :background "firebrick1" :inherit mode-line)))
      "Face for buffer modified segment."
      :group 'telephone-line)

    (telephone-line-defsegment* telephone-line-vc-nobackend-segment ()
      (if vc-mode
          (substring-no-properties vc-mode (+ 1 (string-match "[-:@!?]" vc-mode)))
        " - "))

    (telephone-line-defsegment* telephone-line-buffer-shortname-segment ()
      ;; Avoids the padding in the regular "buffer only" segment
      (buffer-name))

    (telephone-line-defsegment* telephone-line-position+region-segment ()
      (let ((region-size (when (use-region-p)
                           (format " (%sL:%sC)"
                                   (count-lines (region-beginning)
                                                (region-end))
                                   (- (region-end) (region-beginning))))))
        (list "%l:%c" region-size)))

    (defun telephone-line-buffer-mod-color-segment-face (active)
      "Determine the color for a buffer modified segment."
      (if (and active (not buffer-read-only) (buffer-modified-p))
          'telephone-line-buffer-modified-face
        'telephone-line-accent-inactive))

    (telephone-line-defsegment* telephone-line-buffer-mod-segment ()
      (cond
       (buffer-read-only "·")
       ((buffer-modified-p) "!")
       (t "-")))

    (setq telephone-line-faces
          '((extra-accent . (telephone-line-extra-accent-active . telephone-line-extra-accent-inactive))
            (accent . (telephone-line-accent-active . telephone-line-accent-inactive))
            (nil . (mode-line . mode-line-inactive))
            (buffer-state . telephone-line-buffer-mod-color-segment-face)))

    (setq telephone-line-primary-left-separator 'telephone-line-abs-left
          telephone-line-secondary-left-separator 'telephone-line-abs-left)
    (setq telephone-line-primary-right-separator 'telephone-line-abs-right
          telephone-line-secondary-right-separator 'telephone-line-abs-right)
    (setq telephone-line-lhs
          '((buffer-state . (telephone-line-buffer-mod-segment))
            (extra-accent . (telephone-line-buffer-shortname-segment))
            (accent       . (telephone-line-projectile-segment))
            (nil          . (telephone-line-position+region-segment
                             telephone-line-narrow-segment))))
    (setq telephone-line-rhs
          '((nil          . (telephone-line-process-segment
                             telephone-line-misc-info-segment))
            (extra-accent . (telephone-line-vc-nobackend-segment))
            (accent       . (telephone-line-minions-mode-segment))))
    (telephone-line-mode t)))

;; (use-package mood-line
;;   :demand t
;;   :config
;;   (mood-line-mode))

;; (use-package doom-modeline
;;   :hook (after-init . doom-modeline-mode)
;;   :custom
;;   (doom-modeline-buffer-file-name-style 'buffer-name)
;;   (doom-modeline-icon t)
;;   (doom-modeline-major-mode-icon t)
;;   (doom-modeline-major-mode-color-icon t)
;;   (doom-modeline-buffer-state-icon t)
;;   (doom-modeline-buffer-modification-icon t)
;;   (doom-modeline-minor-modes t)
;;   (doom-modeline-enable-word-count nil)
;;   (doom-modeline-checker-simple-format t)
;;   (doom-modeline-persp-name nil)
;;   (doom-modeline-lsp nil)
;;   (doom-modeline-github nil)
;;   (doom-modeline-env-version nil)
;;   (doom-modeline-mu4e nil)
;;   (doom-modeline-irc nil)
;;   (doom-modeline-buffer-encoding nil)
;;   (doom-modeline-indent-info nil))

(use-package challenger-deep-theme)
;; (use-package doom-themes
;;   :ensure t
;;   :config
;;   (progn
;;     (load-theme 'doom-challenger-deep t)))

(add-to-list 'load-path "c:/home/github/dotnet.el")
(use-package dotnet
  :demand t  ;; needed since the global keybinding has to be ready. I think.
  :config
  (progn
    (setq dotnet-mode-keymap-prefix nil)
    (define-key hoagie-keymap (kbd "n") dotnet-mode-command-map)))

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
      (ido-vertical-define-keys 'C-n-and-C-p-only)))
  :custom
  (ido-enable-flex-matching t)
  (ido-everywhere t)
  (ido-create-new-buffer 'always)
  (ido-default-buffer-method 'selected-window))

(use-package icomplete
  :config
  (icomplete-mode 1))

(use-package idomenu
  :bind ("M-g d" . idomenu))

;; ;; LSP is still too slow on Windows
;; (use-package lsp-mode
;;   :commands lsp
;;   :custom
;;   (lsp-enable-snippet nil))
;; (add-hook 'python-mode-hook #'lsp)
;; (use-package lsp-ui :commands lsp-ui-mode)
;; (use-package company-lsp :commands company-lsp)

(use-package eglot
  :commands (eglot eglot-ensure)
  :config
  (define-key eglot-mode-map (kbd "C-c e r") 'eglot-rename)
  (define-key eglot-mode-map (kbd "C-c e f") 'eglot-format)
  (define-key eglot-mode-map (kbd "C-c e h") 'eglot-help-at-point))

(use-package json-mode
  :mode "\\.json$")

(use-package lyrics
  :commands lyrics)

(use-package magit
  :init
  (use-package magit-gitflow
    :commands (turn-on-magit-gitflow))
  :bind
  ("C-x g" . magit-status)
  :hook
  (magit-mode . turn-on-magit-gitflow))

(use-package minions
  :config
  (minions-mode 1)
  :custom
  (minions-direct '(flycheck-mode))
  (minions-mode-line-lighter "^"))

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

(use-package omnisharp
  :config
  (with-eval-after-load
      'company
    '(add-to-list 'company-backends 'company-omnisharp))
  :hook
  (csharp-mode . omnisharp-mode)
  :bind (:map omnisharp-mode-map
              ("C-." . omnisharp-auto-complete)
              ("C-c o e" . omnisharp-solution-errors)
              ("C-c o u" . omnisharp-find-usages-with-ido)
              ("M-?" . omnisharp-find-usages) ;; "compatibility" with other plaforms
              ("C-c o i" . omnisharp-find-implementations-with-ido)
              ("C-c o d" . omnisharp-go-to-definition-other-window)
              ("M-." . omnisharp-go-to-definition) ; more standard
              ("C-c o q" . omnisharp-run-code-action-refactoring)
              ("C-c o f" . omnisharp-fix-code-issue-at-point)
              ("C-c o r" . omnisharp-rename)
              ("C-c o t i" . omnisharp-current-type-information)
              ("C-c o t d" . omnisharp-current-type-documentation)
              ("<f5>" . recompile))
  :custom
  (omnisharp-auto-complete-template-use-yasnippet nil)
  (omnisharp-company-begin-after-member-access nil)
  (omnisharp-company-template-use-yasnippet nil)
  (omnisharp-imenu-support t)
  (omnisharp-server-executable-path "C:/Home/omnisharp_64/OmniSharp.exe"))

(use-package package-lint
  :commands package-lint-current-buffer)

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
  (projectile-indexing-method 'alien)
  (projectile-switch-project-action 'projectile-find-file-dwim))

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
  (setq inferior-lisp-program "sbcl"))

(use-package smex
  :init
  (smex-initialize)
  :bind
  (("M-x" . smex)
   :map hoagie-keymap
   ("<menu>" . smex)))

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
; from https://emacs.stackexchange.com/questions/7362/how-to-show-a-diff-between-two-buffers-with-character-level-diffs
(setq-default ediff-forward-word-function 'forward-char)
;; helps compilation buffer not slowdown
;; see https://blog.danielgempesaw.com/post/129841682030/fixing-a-laggy-compilation-buffer
(setq compilation-error-regexp-alist
      (delete 'maven compilation-error-regexp-alist))
;; from http://www.jurta.org/en/emacs/dotemacs, set the major mode
;; of buffers that are not visiting a file
(setq-default major-mode (lambda ()
                           (if buffer-file-name
                               (fundamental-mode)
                             (let ((buffer-file-name (buffer-name)))
                               (set-auto-mode)))))
;; from https://stackoverflow.com/a/22176971, move auto saves and
;; back up files to a different folder so git or dotnet core won't
;; pick them up as changes or new files in the project
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
;; used to be C-x K. Honestly I never used C-x C-k (macros) commands that much so :shrug:
;; without the lambda it would simply show the menu like C-x k
(defun hoagie-kill-this-buffer ()
  "Kill the current buffer.
If defined as a lambda then it shows a ? in the bindings list."
  (interactive)
  (kill-buffer))
(define-key hoagie-keymap (kbd "k") 'hoagie-kill-this-buffer)
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
(global-set-key (kbd "C-c C-t") 'hoagie-convert-timestamp)

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
(require 'isearch)
(add-hook 'isearch-mode-end-hook 'hoagie-isearch-end-push-mark)
(defun hoagie-isearch-end-push-mark ()
  "Push the mark -without activating- when exiting isearch."
  (unless isearch-mode-end-hook-quit
    (push-mark-no-activate)))
;; the idea with this is similar to the "11 lines away" comment in the post above
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
