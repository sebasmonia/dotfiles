;;; .emacs --- My dot emacs file

;; Author: Sebastian Monia <smonia@outlook.com>
;; URL: https://github.com/sebasmonia/.emacs
;; Version: 2
;; Keywords: .emacs dotemacs

;; This file is not part of GNU Emacs.

;;; Commentary:

;; My dot Emacs file
;; In theory I should be able to just drop the file in any computer and have
;; the config synced without merging/adapting anything

;;; Code:

(require 'package)
(package-initialize)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)
(ignore-errors
  (package-refresh-contents))

(defun hoagie-ensure-package (package)
  "Guarantee that PACKAGE is installed."
  (when (not (package-installed-p package))
    (package-install package))
  (require package))

(defun hoagie-ensure-selected-packages ()
  "Guarantee that packages in package-selected-packages are installed."
  (dolist (p package-selected-packages)
    (when (not (package-installed-p p))
      (package-install p))))

(defun hoagie-laptop-p ()
  "Return t if running in any of the laptops."
  (or (string-equal (system-name) "HogzLaptop")
      (string-equal (system-name) "BrokenButWorking")))

(defun hoagie-work-p ()
  "Return t if running in the work computer."
  ;; only Win computer is the work one
  (string-equal system-type "windows-nt"))

(defun hoagie-rpi-p ()
  "Return t if running in the raspberry pi."
  ;; TODO
  (string-equal system-type "windows-nt"))

(add-to-list 'load-path "~/.emacs.d/lisp/")
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(hoagie-ensure-package 'doom-themes)
(hoagie-ensure-package 'projectile)
(add-hook 'after-init-hook 'hoagie-ensure-selected-packages)

;; CUSTOM-SET
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#1b1d1e" "#d02b61" "#60aa00" "#d08928" "#6c9ef8" "#b77fdb" "#00aa80" "#DFDFDF"])
 '(anzu-deactivate-region t)
 '(anzu-mode-lighter "")
 '(anzu-replace-threshold 50)
 '(anzu-replace-to-string-separator " => ")
 '(anzu-search-threshold 1000)
 '(beacon-color "#d54e53")
 '(blink-cursor-mode nil)
 '(bm-buffer-persistence t)
 '(bm-repository-size 1000)
 '(bubbles-game-theme (quote difficult))
 '(bubbles-grid-size (quote (20 . 15)))
 '(column-number-mode t)
 '(custom-enabled-themes (quote (doom-challenger-deep)))
 '(custom-safe-themes
   (quote
    ("068da66dd5ef78a0fe9245895740a0ba472369032b29bc55df1e7b9db025e46c" "6b2636879127bf6124ce541b1b2824800afc49c6ccd65439d6eb987dbf200c36" "1c082c9b84449e54af757bcae23617d11f563fc9f33a832a8a2813c4d7dfb652" "8aca557e9a17174d8f847fb02870cb2bb67f3b6e808e46c0e54a44e3e18e1020" "75d3dde259ce79660bac8e9e237b55674b910b470f313cdf4b019230d01a982a" "7e78a1030293619094ea6ae80a7579a562068087080e01c2b8b503b27900165c" "d2e9c7e31e574bf38f4b0fb927aaff20c1e5f92f72001102758005e53d77b8c9" "f0dc4ddca147f3c7b1c7397141b888562a48d9888f1595d69572db73be99a024" "a5f068e6c26c2ed952096c034eb49f3ad15a329c905bf3475fae63c1ddb9f402" "144f05e2dfa7a7b50cad0c3519498ac064cc9da1f194b8ea27d0fb20129d8d7a" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "4138944fbed88c047c9973f68908b36b4153646a045648a22083bd622d1e636d" "a3fa4abaf08cc169b61dea8f6df1bbe4123ec1d2afeb01c17e11fdc31fc66379" default)))
 '(dabbrev-case-distinction nil)
 '(dabbrev-case-fold-search t)
 '(dabbrev-case-replace nil)
 '(diary-entry-marker (quote font-lock-variable-name-face))
 '(dired-dwim-target t)
 '(dired-listing-switches "-laogGhvD")
 '(dired-sort-menu-saved-config
   (quote
    ((dired-actual-switches . "-al")
     (ls-lisp-ignore-case)
     (ls-lisp-dirs-first . t))))
 '(diredp-ignore-compressed-flag t)
 '(display-line-numbers (quote relative))
 '(display-line-numbers-current-absolute nil)
 '(doom-challenger-deep-brighter-comments nil)
 '(doom-challenger-deep-brighter-modeline t)
 '(doom-dracula-brighter-comments t)
 '(ediff-highlight-all-diffs t)
 '(ediff-keep-variants nil)
 '(ediff-window-setup-function (quote ediff-setup-windows-plain))
 '(eww-search-prefix "http://www.bing.com/search?q=")
 '(fci-rule-color "#505050")
 '(flycheck-color-mode-line-face-to-color (quote mode-line-buffer-id))
 '(frame-background-mode (quote dark))
 '(global-flycheck-mode t)
 '(global-mark-ring-max 32)
 '(global-visible-mark-mode t)
 '(grep-command
   "grep --color=always -nHi -r --include=*.* -e \"pattern\" .")
 '(hl-sexp-background-color "#1c1f26")
 '(ido-default-buffer-method (quote selected-window))
 '(ido-ubiquitous-mode t)
 '(jdee-db-active-breakpoint-face-colors (cons "#1b1d1e" "#fc20bb"))
 '(jdee-db-requested-breakpoint-face-colors (cons "#1b1d1e" "#60aa00"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#1b1d1e" "#505050"))
 '(lisp-mode-hook
   (quote
    (#[nil "\300\301\302\303\211$\207"
           [add-hook font-lock-extend-region-functions sly-extend-region-for-font-lock t]
           5]
     common-lisp-lisp-mode-hook sly-editing-mode
     #[nil "\300\301\302\303\211$\207"
           [add-hook font-lock-extend-region-functions slime-extend-region-for-font-lock t]
           5])))
 '(ls-lisp-dirs-first t)
 '(ls-lisp-format-time-list (quote ("%Y-%m-%d %H:%M" "%Y-%m-%d %H:%M")))
 '(ls-lisp-use-insert-directory-program nil)
 '(ls-lisp-use-localized-time-format t)
 '(ls-lisp-verbosity nil)
 '(main-line-color1 "#222232")
 '(main-line-color2 "#333343")
 '(menu-bar-mode nil)
 '(minions-direct (quote (flycheck-mode)))
 '(minions-mode t)
 '(minions-mode-line-lighter "^")
 '(nrepl-message-colors
   (quote
    ("#CC9393" "#DFAF8F" "#F0DFAF" "#7F9F7F" "#BFEBBF" "#93E0E3" "#94BFF3" "#DC8CC3")))
 '(omnisharp-auto-complete-template-use-yasnippet nil)
 '(omnisharp-company-begin-after-member-access nil)
 '(omnisharp-company-template-use-yasnippet nil)
 '(omnisharp-imenu-support t)
 '(omnisharp-server-executable-path "C:/Home/omnisharp_64/OmniSharp.exe")
 '(org-fontify-emphasized-text nil)
 '(org-hide-emphasis-markers t)
 '(org-plantuml-jar-path "c:/HomeFolder/PlantUML/plantuml.jar")
 '(package-selected-packages
   (quote
    (json-navigator dired-git-info company-lsp lsp-ui lsp-mode ido-completing-read+ visible-mark package-lint dockerfile-mode quasi-monochrome-theme srcery-theme eww-lnum nyan-mode ws-butler sly doom-modeline dotnet anzu pomidor minions deadgrep expand-region format-all lyrics docker json-mode company browse-kill-ring 2048-game doom-themes gist ibuffer-projectile wttrin dashboard powershell projectile smex which-key ido-vertical-mode dired-narrow circe web-mode symon omnisharp magit)))
 '(pdf-view-midnight-colors (quote ("#DCDCCC" . "#383838")))
 '(pomidor-play-sound-file nil)
 '(pos-tip-background-color "#36473A")
 '(pos-tip-foreground-color "#FFFFC8")
 '(proced-filter (quote all))
 '(projectile-indexing-method (quote alien))
 '(projectile-mode t nil (projectile))
 '(projectile-switch-project-action (quote projectile-find-file-dwim))
 '(reb-re-syntax (quote string))
 '(scroll-bar-mode nil)
 '(set-mark-command-repeat-pop t)
 '(size-indication-mode t)
 '(spaceline-all-the-icons-slim-render nil)
 '(sql-ms-options nil)
 '(sql-ms-program "sqlcmdline")
 '(sql-product (quote ms))
 '(sunshine-units (quote metric))
 '(symon-delay 10)
 '(symon-mode t)
 '(symon-refresh-rate 5)
 '(tool-bar-mode nil)
 '(tramp-syntax (quote default) nil (tramp))
 '(vc-annotate-background "#1b1d1e")
 '(vc-annotate-color-map
   (list
    (cons 20 "#60aa00")
    (cons 40 "#859f0d")
    (cons 60 "#aa931a")
    (cons 80 "#d08928")
    (cons 100 "#d38732")
    (cons 120 "#d6863d")
    (cons 140 "#da8548")
    (cons 160 "#ce8379")
    (cons 180 "#c281aa")
    (cons 200 "#b77fdb")
    (cons 220 "#bf63b2")
    (cons 240 "#c74789")
    (cons 260 "#d02b61")
    (cons 280 "#b0345c")
    (cons 300 "#903d58")
    (cons 320 "#704654")
    (cons 340 "#505050")
    (cons 360 "#505050")))
 '(vc-annotate-very-old-color nil)
 '(visible-mark-faces
   (quote
    (visible-mark-face1 visible-mark-face2 visible-mark-forward-face1 visible-mark-forward-face2)))
 '(visible-mark-max 4)
 '(web-mode-enable-css-colorization t)
 '(web-mode-enable-sql-detection t)
 '(which-key-side-window-max-width 0.4)
 '(which-key-sort-order (quote which-key-prefix-then-key-order))
 '(wttrin-default-accept-language (quote ("Accept-Language" . "en-US")))
 '(wttrin-default-cities (quote ("Denver?m" "Buenos Aires?m")))
 '(yas-prompt-functions
   (quote
    (yas-ido-prompt yas-dropdown-prompt yas-completing-prompt yas-maybe-ido-prompt yas-no-prompt))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Hack" :foundry "outline" :slant normal :weight normal :height 83 :width normal))))
 '(dgi-commit-message-face ((t (:foreground "cornflower blue"))))
 '(diredp-compressed-file-name ((t (:foreground "slate gray"))))
 '(diredp-compressed-file-suffix ((((class color) (min-colors 89)) (:foreground "#b218b2"))))
 '(diredp-deletion ((((class color) (min-colors 89)) (:foreground "#ffffff" :background "#a40000"))))
 '(diredp-deletion-file-name ((((class color) (min-colors 89)) (:foreground "#cc0000"))))
 '(diredp-dir-heading ((((class color) (min-colors 89)) (:foreground "#5f5f5f" :background "#d7ff00" :bold t))))
 '(diredp-dir-name ((t (:foreground "gold"))))
 '(diredp-flag-mark ((((class color) (min-colors 89)) (:foreground "#ffffff" :background "#ff1f8b" :bold t))))
 '(diredp-flag-mark-line ((((class color) (min-colors 89)) (:foreground "#5f5f5f" :background "#ff7bbb"))))
 '(diredp-ignored-file-name ((((class color) (min-colors 257)) (:foreground "#858FA5")) (((class color) (min-colors 256)) (:foreground "#525252")) (((class color) (min-colors 16)) (:foreground "brightblack"))))
 '(ediff-current-diff-B ((t (:inherit ediff-current-diff-A))))
 '(ediff-current-diff-C ((t (:inherit ediff-current-diff-A))))
 '(ediff-even-diff-A ((t (:inherit hl-line))))
 '(ediff-even-diff-Ancestor ((t (:background "cornflower blue"))))
 '(ediff-even-diff-B ((t (:inherit ediff-even-diff-A))))
 '(ediff-even-diff-C ((t (:inherit ediff-even-diff-A))))
 '(ediff-fine-diff-Ancestor ((t (:background "DodgerBlue2"))))
 '(ediff-fine-diff-B ((t (:inherit ediff-fine-diff-A))))
 '(ediff-fine-diff-C ((t (:inherit ediff-fine-diff-A))))
 '(ediff-odd-diff-A ((t (:inherit ediff-even-diff-A))))
 '(ediff-odd-diff-Ancestor ((t (:background "cornflower blue"))))
 '(ediff-odd-diff-B ((t (:background "dark slate gray"))))
 '(ediff-odd-diff-C ((t (:background "dark slate gray"))))
 '(visible-mark-face1 ((t (:box (:line-width 1 :color "turquoise")))))
 '(visible-mark-face2 ((t (:box (:line-width 1 :color "dodger blue")))))
 '(visible-mark-forward-face1 ((t (:box (:line-width 1 :color "dark green")))))
 '(visible-mark-forward-face2 ((t (:box (:line-width 1 :color "dark olive green")))) t)
 '(web-mode-block-face ((t nil))))

;; ANZU
(hoagie-ensure-package 'anzu)
(global-anzu-mode +1)
(set-face-attribute 'anzu-mode-line nil
                    :foreground "light slate blue" :weight 'bold)

(define-key isearch-mode-map [remap isearch-query-replace]  #'anzu-isearch-query-replace)
(define-key isearch-mode-map [remap isearch-query-replace-regexp] #'anzu-isearch-query-replace-regexp)

;; BROWSE-KILL-RING
(hoagie-ensure-package 'browse-kill-ring)
(browse-kill-ring-default-keybindings)

;; COMPANY
(hoagie-ensure-package 'company)
(add-hook 'after-init-hook 'global-company-mode)

;; EDIFF
(require 'ediff)
; from https://emacs.stackexchange.com/questions/7362/how-to-show-a-diff-between-two-buffers-with-character-level-diffs
(setq-default ediff-forward-word-function 'forward-char)
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
(add-hook 'ediff-keymap-setup-hook 'add-d-to-ediff-mode-map)

;; EXPAND REGION
(hoagie-ensure-package 'expand-region)
(global-set-key (kbd "M-<SPC>") 'er/expand-region)

;; EWW-LNUM
(hoagie-ensure-package 'eww-lnum)
(eval-after-load "eww"
  '(progn (define-key eww-mode-map "f" 'eww-lnum-follow)
          (define-key eww-mode-map "F" 'eww-lnum-universal)))

;; DASHBOARD
(hoagie-ensure-package 'dashboard)
(dashboard-setup-startup-hook)
(setq dashboard-startup-banner 'logo)
(setq dashboard-banner-logo-title "If anything at all, perfection is finally attained not when there is no longer anything to add, but when there is no longer anything to take away - Exupéry")
(setq dashboard-items '((projects . 10)
                        (recents  . 30)))
(setq initial-buffer-choice  (lambda () (get-buffer "*dashboard*")))

;; DIRED
(require 'dired)
(global-set-key (kbd "\C-cj") 'dired-jump)
(define-key dired-mode-map (kbd "\\") 'dired-narrow)
;; more standard binding for filtering, but I'm so used to \, leaving both
(define-key dired-mode-map (kbd "/") 'dired-narrow)
(with-eval-after-load 'dired
  (define-key dired-mode-map ")" 'dired-git-info-mode))

;; C-c l to launch a file in Windows similar to running
;; start "" filename in the console
(when (hoagie-work-p)
  (defun w32-browser (doc)
    "Have Windows start DOC."
    (w32-shell-execute 1 doc))
  (define-key dired-mode-map (kbd "\C-cl")
    (lambda () (interactive) (w32-browser (dired-replace-in-string "/" "\\" (dired-get-filename))))))

;; copy to clipboard from dired, in Windows
;; useful to copy files and then paste in Outlook
(when (hoagie-work-p)
  ;; from https://github.com/roryyorke/picellif/
  (defun picellif-dired-marked-files ()
    "Send marked files (or current file, if none marked) in current Dired buffer to picellif."
    (interactive)
    (apply 'call-process "picellif" nil nil nil
           (dired-get-marked-files)))
  (define-key dired-mode-map (kbd "W") 'picellif-dired-marked-files))

;; DOCKER
(hoagie-ensure-package 'docker)
(global-set-key (kbd "C-c d") 'docker)

;; DOOM-MODELINE
(hoagie-ensure-package 'doom-modeline)
(setq doom-modeline-buffer-file-name-style 'buffer-name)
(setq doom-modeline-icon t)
(setq doom-modeline-major-mode-icon t)
(setq doom-modeline-major-mode-color-icon t)
(setq doom-modeline-minor-modes t)
(setq doom-modeline-persp-name nil)
(setq doom-modeline-lsp nil)
(setq doom-modeline-github nil)
(doom-modeline-init)

;; DOTNET
(hoagie-ensure-package 'dotnet)
(setq dotnet-mode-keymap-prefix (kbd "C-c n"))
(add-hook 'csharp-mode-hook 'dotnet-mode)

;; FORMAT-ALL-THE-CODE
(hoagie-ensure-package 'format-all)
(global-set-key (kbd "C-c C-f b") 'format-all-buffer)

;; IBUFFER
(require 'ibuffer)
(global-set-key (kbd "C-x C-b") 'ibuffer-other-window)
(setq ibuffer-default-sorting-mode 'major-mode)
(add-hook 'ibuffer-mode-hook
	  '(lambda ()
	     (ibuffer-auto-mode 1)
	     (ibuffer-switch-to-saved-filter-groups "home")))
(setq ibuffer-expert t)
(setq ibuffer-show-empty-filter-groups nil)
(hoagie-ensure-package 'ibuffer-projectile)
(add-hook 'ibuffer-hook
    (lambda ()
      (ibuffer-projectile-set-filter-groups)
      (unless (eq ibuffer-sorting-mode 'alphabetic)
        (ibuffer-do-sort-by-alphabetic))))

;; IDO
(hoagie-ensure-package 'ido-vertical-mode)
(hoagie-ensure-package 'ido-completing-read+)
(ido-mode 1)
(ido-vertical-mode 1)
(setq ido-vertical-define-keys 'C-n-and-C-p-only)
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(setq ido-create-new-buffer 'always)
(ido-ubiquitous-mode 1)
(setq magit-completing-read-function 'magit-ido-completing-read)
(require 'icomplete)
(icomplete-mode 1)

;; IMENU
(require 'imenu)
;; from https://gist.github.com/magnars/2360578
(defun ido-imenu ()
  "Update the imenu index and then use ido to select a symbol to navigate to.
Symbols matching the text at point are put first in the completion list."
  (interactive)
  (imenu--make-index-alist)
  (let ((name-and-pos '())
        (symbol-names '()))
    (flet ((addsymbols (symbol-list)
                       (when (listp symbol-list)
                         (dolist (symbol symbol-list)
                           (let ((name nil) (position nil))
                             (cond
                              ((and (listp symbol) (imenu--subalist-p symbol))
                               (addsymbols symbol))

                              ((listp symbol)
                               (setq name (car symbol))
                               (setq position (cdr symbol)))

                              ((stringp symbol)
                               (setq name symbol)
                               (setq position (get-text-property 1 'org-imenu-marker symbol))))

                             (unless (or (null position) (null name))
                               (add-to-list 'symbol-names name)
                               (add-to-list 'name-and-pos (cons name position))))))))
      (addsymbols imenu--index-alist))
    ;; If there are matching symbols at point, put them at the beginning of `symbol-names'.
    (let ((symbol-at-point (thing-at-point 'symbol)))
      (when symbol-at-point
        (let* ((regexp (concat (regexp-quote symbol-at-point) "$"))
               (matching-symbols (delq nil (mapcar (lambda (symbol)
                                                     (if (string-match regexp symbol) symbol))
                                                   symbol-names))))
          (when matching-symbols
            (sort matching-symbols (lambda (a b) (> (length a) (length b))))
            (mapc (lambda (symbol) (setq symbol-names (cons symbol (delete symbol symbol-names))))
                  matching-symbols)))))
    (let* ((selected-symbol (ido-completing-read "Symbol? " symbol-names))
           (position (cdr (assoc selected-symbol name-and-pos))))
      (goto-char position))))

(global-set-key (kbd "M-g d") 'ido-imenu)

;; LSP MODE
;; (hoagie-ensure-package 'lsp-mode)
;; (hoagie-ensure-package 'lsp-ui)
;; (hoagie-ensure-package 'company-lsp)
;; (require 'lsp-python-ms)
;; (add-hook 'python-mode-hook #'lsp)
;; (setq lsp-python-ms-dir "c:/home/github/ms-python-language-server/output/bin/Release/win-x64/publish/")
;; (setq lsp-python-ms-executable "c:/home/github/ms-python-language-server/output/bin/Release/win-x64/publish/Microsoft.Python.LanguageServer.exe")

;; JSON MODE
(hoagie-ensure-package 'json-mode)
(add-to-list 'auto-mode-alist '("\\.json$" . json-mode))

;; MAGIT
(hoagie-ensure-package 'magit)
(global-set-key (kbd "C-x g") 'magit-status)

;; MINIONS
(hoagie-ensure-package 'minions)
(global-set-key [f3] 'minions-minor-modes-menu)

;; NYAN MODE - now doom-modeline supports it
(hoagie-ensure-package 'nyan-mode)
(nyan-mode)
(nyan-start-animation)
(nyan-toggle-wavy-trail)

;; OMNISHARP
(hoagie-ensure-package 'omnisharp)
(add-hook 'csharp-mode-hook 'omnisharp-mode)
(with-eval-after-load
  'company
  '(add-to-list 'company-backends 'company-omnisharp))
(add-hook 'csharp-mode-hook #'company-mode)
(define-key omnisharp-mode-map (kbd "C-.") 'omnisharp-auto-complete)
;; (define-key omnisharp-mode-map (kbd ".") 'omnisharp-add-dot-and-auto-complete)
(define-key omnisharp-mode-map (kbd "C-c o e") 'omnisharp-solution-errors)
(define-key omnisharp-mode-map (kbd "C-c o u") 'omnisharp-find-usages)
(define-key omnisharp-mode-map (kbd "M-?") 'omnisharp-find-usages) ;; "compatibility" with other plaforms
(define-key omnisharp-mode-map (kbd "C-c o i") 'omnisharp-find-implementations)
(define-key omnisharp-mode-map (kbd "C-c o d") 'omnisharp-go-to-definition)
(define-key omnisharp-mode-map (kbd "M-.") 'omnisharp-go-to-definition) ; more standard
(define-key omnisharp-mode-map (kbd "C-c o q") 'omnisharp-run-code-action-refactoring)
(define-key omnisharp-mode-map (kbd "C-c o f") 'omnisharp-fix-code-issue-at-point)
(define-key omnisharp-mode-map (kbd "C-c o r") 'omnisharp-rename)
(define-key omnisharp-mode-map (kbd "C-c o t i") 'omnisharp-current-type-information)
(define-key omnisharp-mode-map (kbd "C-c o t d") 'omnisharp-current-type-documentation)
(define-key omnisharp-mode-map (kbd "<f5>") 'recompile)

;; POWERSHELL MODE
(hoagie-ensure-package 'powershell)
;; this one shadows the command to go back in
;; the mark ring
(define-key powershell-mode-map (kbd "M-`") nil)

;; PROJECTILE
(hoagie-ensure-package 'projectile)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

;; SHELL
(add-hook 'shell-mode-hook
          (lambda ()
            (toggle-truncate-lines t)))

;; SLY
(hoagie-ensure-package 'sly)
(setq inferior-lisp-program "sbcl")

;; SMEX
(hoagie-ensure-package 'smex)
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(cond
 ((hoagie-work-p) (global-set-key (kbd "<apps>") 'smex))
 ((hoagie-laptop-p) (global-set-key (kbd "<menu>") 'smex)))
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

;; SQL MODE
(require 'sql)
(sql-set-product-feature 'ms :prompt-regexp "^.*>")
(sql-set-product-feature 'ms :prompt-cont-regexp "^.*>")
;After moving to Emacs 26.0.9, I don't get prompted for buffer name when doing C-u M-x sql-connect
;added the function below and a call in the SQLi hook to go back to the old behaviour
(defun sql-rename-buffer-prompt ()
  "Prompts for a rename of the SQLi buffer."
  (interactive)
  (let ((current-prefix-arg '(4)))
    (call-interactively 'sql-rename-buffer)))
(add-hook 'sql-interactive-mode-hook
          (lambda ()
            (toggle-truncate-lines t)
            (font-lock-mode -1)
            (sql-rename-buffer-prompt)))

;; SYMON
(hoagie-ensure-package 'symon)
(when (hoagie-work-p)
  ;; disable symon before changing the list of monitors
  (symon-mode nil)
  (setq symon-monitors
        (quote
         (symon-windows-memory-monitor symon-windows-cpu-monitor symon-windows-battery-monitor symon-windows-network-rx-monitor symon-windows-network-tx-monitor)))
  (symon-mode t))
;; TFSMACS
;; (hoagie-ensure-package 'tfsmacs)
;; (global-set-key  "\C-ct" 'tfsmacs-map)

;; WEB MODE
(hoagie-ensure-package 'web-mode)
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.cshtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.css\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.xml?\\'" . web-mode))
(setq web-mode-enable-current-element-highlight t)
(defun my-web-mode-hook ()
  "Hooks for Web mode."
  (setq web-mode-markup-indent-offset 2))
(add-hook 'web-mode-hook  'my-web-mode-hook)

;; WHICH KEY
(hoagie-ensure-package 'which-key)
(which-key-mode)
(which-key-setup-side-window-right-bottom)

;; WS-BUTLER
(hoagie-ensure-package 'ws-butler)
(add-hook 'prog-mode-hook #'ws-butler-mode)

;; MISC
(setq-default indent-tabs-mode nil)  ; use only spaces and no tabs
(setq default-tab-width 4)
(global-hl-line-mode t)
(setq frame-title-format "%b - Emacs")
(global-set-key (kbd "M-RET") 'toggle-frame-fullscreen)
(delete-selection-mode t)
(electric-pair-mode)
; see https://emacs.stackexchange.com/questions/33510/unicode-txt-slowness
(setq inhibit-compacting-font-caches t)
; from https://emacs.stackexchange.com/questions/7362/how-to-show-a-diff-between-two-buffers-with-character-level-diffs
(setq-default ediff-forward-word-function 'forward-char)
(add-to-list 'default-frame-alist '(fullscreen . maximized))
; adapted for https://stackoverflow.com/questions/6464738/how-can-i-switch-focus-after-buffer-split-in-emacs
(global-set-key (kbd "C-x 3") (lambda () (interactive)(split-window-right) (other-window 1)))
(global-set-key (kbd "C-x 2") (lambda () (interactive)(split-window-below) (other-window 1)))
;; on trial
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
(global-set-key (kbd "C-x C-k") (lambda () (interactive) (kill-buffer)))
(global-set-key (kbd "C-;") 'dabbrev-expand)
(global-set-key (kbd "C-c M-d") 'sql-connect)
(global-set-key (kbd "<f6>") 'kmacro-start-macro)
(global-set-key (kbd "<f7>") 'kmacro-end-macro)
(global-set-key (kbd "<f8>") 'kmacro-end-and-call-macro)
(global-set-key (kbd "C-z") 'find-name-dired)
(global-set-key (kbd "M-z") 'deadgrep)
(global-set-key (kbd "<mouse-3>") 'kill-ring-save)
(global-set-key (kbd "<f12>") 'zone)
(put 'narrow-to-region 'disabled nil)
;; helps compilation buffer not slowdown
;; see https://blog.danielgempesaw.com/post/129841682030/fixing-a-laggy-compilation-buffer
(setq compilation-error-regexp-alist
      (delete 'maven compilation-error-regexp-alist))

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

(global-set-key (kbd "C-`") 'push-mark-no-activate)
(global-set-key (kbd "M-`") 'pop-to-mark-push-if-first)
(global-set-key (kbd "M-~") 'unpop-to-mark-command)

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

(global-set-key [f1] (lambda () (interactive) (dired "~/")))

(when (hoagie-work-p)
  (load "c:/repos/miscscripts/workonlyconfig.el"))

(when (or (hoagie-laptop-p) (hoagie-rpi-p))
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
