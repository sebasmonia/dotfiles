;; .emacs --- My dot emacs file

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
(unless package--initialized
  (package-initialize))
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

(add-to-list 'load-path (expand-file-name  "~/.emacs.d/lisp/"))
(set-language-environment 'utf-8)
(set-keyboard-coding-system 'utf-8-mac) ; For old Carbon emacs on OS X only
(setq locale-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-selection-coding-system
 (if (eq system-type 'windows-nt)
     'utf-16-le  ;; https://rufflewind.com/2014-07-20/pasting-unicode-in-emacs-on-windows
   'utf-8))
(prefer-coding-system 'utf-8)

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
 '(bubbles-game-theme 'difficult)
 '(bubbles-grid-size '(20 . 15))
 '(column-number-mode t)
 '(company-idle-delay 0.1)
 '(company-minimum-prefix-length 2)
 '(custom-enabled-themes '(doom-challenger-deep))
 '(custom-safe-themes
   (quote
    ("36c86cb6c648b9a15d849026c90bd6a4ae76e4d482f7bcd138dedd4707ff26a5" "4e4befa32590db02faa3b1589e7ce9f3b6065cd24e8da804b39b747f2473dd50" "6eb36e7cab0fe6d05e7da2acd8b52fe9daea7edd3922d022b0e8e550c41c3a62" "de9fa4b3614611bed2fe75e105bd0d37542924b977299736f158dd4d7343c666" "54449a089fc2f95f99ebc9b9b6067c802532fd50097cf44c46a53b4437d5c6cc" "6d589ac0e52375d311afaa745205abb6ccb3b21f6ba037104d71111e7e76a3fc" "068da66dd5ef78a0fe9245895740a0ba472369032b29bc55df1e7b9db025e46c" "6b2636879127bf6124ce541b1b2824800afc49c6ccd65439d6eb987dbf200c36" "1c082c9b84449e54af757bcae23617d11f563fc9f33a832a8a2813c4d7dfb652" "8aca557e9a17174d8f847fb02870cb2bb67f3b6e808e46c0e54a44e3e18e1020" "75d3dde259ce79660bac8e9e237b55674b910b470f313cdf4b019230d01a982a" "7e78a1030293619094ea6ae80a7579a562068087080e01c2b8b503b27900165c" "d2e9c7e31e574bf38f4b0fb927aaff20c1e5f92f72001102758005e53d77b8c9" "f0dc4ddca147f3c7b1c7397141b888562a48d9888f1595d69572db73be99a024" "a5f068e6c26c2ed952096c034eb49f3ad15a329c905bf3475fae63c1ddb9f402" "144f05e2dfa7a7b50cad0c3519498ac064cc9da1f194b8ea27d0fb20129d8d7a" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "4138944fbed88c047c9973f68908b36b4153646a045648a22083bd622d1e636d" "a3fa4abaf08cc169b61dea8f6df1bbe4123ec1d2afeb01c17e11fdc31fc66379" default)))
 '(dabbrev-case-distinction nil)
 '(dabbrev-case-fold-search t)
 '(dabbrev-case-replace nil)
 '(diary-entry-marker 'font-lock-variable-name-face)
 '(dired-dwim-target t)
 '(dired-listing-switches "-laogGhvD")
 '(dired-sort-menu-saved-config
   '((dired-actual-switches . "-al")
     (ls-lisp-ignore-case)
     (ls-lisp-dirs-first . t)))
 '(diredp-ignore-compressed-flag t)
 '(display-line-numbers 'relative)
 '(display-line-numbers-current-absolute nil)
 '(doom-challenger-deep-brighter-comments nil)
 '(doom-challenger-deep-brighter-modeline t)
 '(doom-dracula-brighter-comments t)
 '(doom-modeline-mode nil)
 '(ediff-highlight-all-diffs t)
 '(ediff-keep-variants nil)
 '(ediff-window-setup-function 'ediff-setup-windows-plain)
 '(eww-search-prefix "http://www.bing.com/search?q=")
 '(fci-rule-color "#858FA5")
 '(flycheck-color-mode-line-face-to-color 'mode-line-buffer-id)
 '(frame-background-mode 'dark)
 '(global-flycheck-mode t)
 '(global-mark-ring-max 32)
 '(global-visible-mark-mode t)
 '(grep-command
   "grep --color=always -nHi -r --include=*.* -e \"pattern\" .")
 '(hl-sexp-background-color "#1c1f26")
 '(ido-default-buffer-method 'selected-window)
 '(ido-ubiquitous-mode t)
 '(inhibit-startup-screen t)
 '(initial-buffer-choice t)
 '(initial-scratch-message
   ";; Il semble que la perfection soit atteinte non quand il n'y a plus rien à ajouter, mais quand il n'y a plus à retrancher. - Antoine_de_Saint_Exupéry
;; It seems that perfection is attained not when there is nothing more to add, but when there is nothing more to remove.
")
 '(jdee-db-active-breakpoint-face-colors (cons "#100e23" "#906cff"))
 '(jdee-db-requested-breakpoint-face-colors (cons "#100e23" "#95ffa4"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#100e23" "#565575"))
 '(lisp-mode-hook
   '(#[nil "\300\301\302\303\211$\207"
           [add-hook font-lock-extend-region-functions sly-extend-region-for-font-lock t]
           5]
     common-lisp-lisp-mode-hook sly-editing-mode
     #[nil "\300\301\302\303\211$\207"
           [add-hook font-lock-extend-region-functions slime-extend-region-for-font-lock t]
           5]))
 '(ls-lisp-dirs-first t)
 '(ls-lisp-format-time-list '("%Y-%m-%d %H:%M" "%Y-%m-%d %H:%M"))
 '(ls-lisp-use-insert-directory-program nil)
 '(ls-lisp-use-localized-time-format t)
 '(ls-lisp-verbosity nil)
 '(main-line-color1 "#222232")
 '(main-line-color2 "#333343")
 '(menu-bar-mode nil)
 '(minions-direct '(flycheck-mode))
 '(minions-mode t)
 '(minions-mode-line-lighter "^")
 '(nrepl-message-colors
   '("#CC9393" "#DFAF8F" "#F0DFAF" "#7F9F7F" "#BFEBBF" "#93E0E3" "#94BFF3" "#DC8CC3"))
 '(omnisharp-auto-complete-template-use-yasnippet nil)
 '(omnisharp-company-begin-after-member-access nil)
 '(omnisharp-company-template-use-yasnippet nil)
 '(omnisharp-imenu-support t)
 '(omnisharp-server-executable-path "C:/Home/omnisharp_64/OmniSharp.exe")
 '(org-fontify-emphasized-text nil)
 '(org-hide-emphasis-markers t)
 '(org-plantuml-jar-path "c:/HomeFolder/PlantUML/plantuml.jar")
 '(package-selected-packages
   '(cheat-sh better-shell doom-modeline doom-themes expand-region eglot flatland-black-theme yaml-mode magit-gitflow request android-mode csv json-navigator dired-git-info company-lsp lsp-ui lsp-mode visible-mark package-lint dockerfile-mode eww-lnum ws-butler sly dotnet anzu pomidor minions deadgrep format-all lyrics docker json-mode company browse-kill-ring 2048-game gist ibuffer-projectile powershell projectile smex which-key ido-vertical-mode dired-narrow circe web-mode omnisharp magit))
 '(pdf-view-midnight-colors '("#DCDCCC" . "#383838"))
 '(pomidor-play-sound-file nil)
 '(pos-tip-background-color "#36473A")
 '(pos-tip-foreground-color "#FFFFC8")
 '(proced-filter 'all)
 '(projectile-indexing-method 'alien)
 '(projectile-mode t nil (projectile))
 '(projectile-switch-project-action 'projectile-find-file-dwim)
 '(python-shell-interpreter "ipython")
 '(python-shell-interpreter-args "--pprint ")
 '(reb-re-syntax 'string)
 '(savehist-mode t)
 '(scroll-bar-mode nil)
 '(set-mark-command-repeat-pop t)
 '(size-indication-mode t)
 '(spaceline-all-the-icons-slim-render nil)
 '(sql-ms-options nil)
 '(sql-ms-program "sqlcmdline")
 '(sql-product 'ms)
 '(sunshine-units 'metric)
 '(tool-bar-mode nil)
 '(tramp-syntax 'default nil (tramp))
 '(vc-annotate-background "#1b182c")
 '(vc-annotate-color-map
   (list
    (cons 20 "#95ffa4")
    (cons 40 "#b8f7a6")
    (cons 60 "#dbf0a8")
    (cons 80 "#ffe9aa")
    (cons 100 "#ffd799")
    (cons 120 "#ffc488")
    (cons 140 "#ffb378")
    (cons 160 "#eda79b")
    (cons 180 "#db9cbd")
    (cons 200 "#c991e1")
    (cons 220 "#db8bc0")
    (cons 240 "#ed85a0")
    (cons 260 "#ff8080")
    (cons 280 "#d4757d")
    (cons 300 "#aa6a7a")
    (cons 320 "#805f77")
    (cons 340 "#858FA5")
    (cons 360 "#858FA5")))
 '(vc-annotate-very-old-color nil)
 '(visible-mark-faces '(visible-mark-face1 visible-mark-face2))
 '(visible-mark-forward-faces '(visible-mark-forward-face2 visible-mark-forward-face1))
 '(visible-mark-forward-max 2)
 '(visible-mark-max 2)
 '(web-mode-enable-css-colorization t)
 '(web-mode-enable-sql-detection t)
 '(which-key-side-window-max-width 0.4)
 '(which-key-sort-order 'which-key-prefix-then-key-order)
 '(wttrin-default-accept-language '("Accept-Language" . "en-US"))
 '(wttrin-default-cities '("Denver?m" "Buenos Aires?m"))
 '(yas-prompt-functions
   '(yas-ido-prompt yas-dropdown-prompt yas-completing-prompt yas-maybe-ido-prompt yas-no-prompt)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Consolas" :foundry "outline" :slant normal :weight normal :height 102 :width normal))))
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
 '(visible-mark-face1 ((t (:box (:line-width 1 :color "red")))))
 '(visible-mark-face2 ((t (:box (:line-width 1 :color "orange")))))
 '(visible-mark-forward-face1 ((t (:box (:line-width 1 :color "green")))))
 '(visible-mark-forward-face2 ((t (:box (:line-width 1 :color "yellow")))) t)
 '(web-mode-block-face ((t nil))))

;; based on http://www.ergoemacs.org/emacs/emacs_menu_app_keys.html
;; CUSTOM KEYMAP
(defvar hoagie-keymap (define-prefix-command 'hoagie-keymap) "My custom bindings.")
(define-key key-translation-map (kbd "<apps>") (kbd "<menu>"))
(global-set-key (kbd "<menu>") 'hoagie-keymap)

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

;; CHEAT-SH
(require 'cheat-sh)
(global-set-key (kbd "C-c s") 'cheat-sh-maybe-region)
(global-set-key (kbd "C-c S") 'cheat-sh-search-topic)

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
(er/enable-mode-expansions 'csharp-mode 'er/add-cc-mode-expansions)
(global-set-key (kbd "M-<SPC>") 'er/expand-region)

;; EWW-LNUM
(hoagie-ensure-package 'eww-lnum)
(eval-after-load "eww"
  '(progn (define-key eww-mode-map "f" 'eww-lnum-follow)
          (define-key eww-mode-map "F" 'eww-lnum-universal)))

;; DIRED
(require 'dired)
(defun hoagie-dired-jump (&optional arg)
  "Call dired-jump.  With prefix ARG, open in current window."
  (interactive "P")
  (let ((inverted (not arg)))
    (dired-jump inverted)))
(define-key hoagie-keymap (kbd "j") 'hoagie-dired-jump)
(define-key hoagie-keymap (kbd "J") (lambda () (interactive) (hoagie-dired-jump 4)))
(define-key dired-mode-map (kbd "\\") 'dired-narrow)
;; more standard binding for filtering, but I'm so used to \, leaving both
(define-key dired-mode-map (kbd "/") 'dired-narrow)
(with-eval-after-load 'dired
  (define-key dired-mode-map ")" 'dired-git-info-mode))

;; C-c l to launch a file in Windows similar to running
;; start "" filename in the console
(when (hoagie-work-p)
  (defun hoagie-dired-winstart ()
    "Start the file under point on Windows."
    (interactive)
    (let ((filename (dired-replace-in-string "/"
                                             "\\"
                                             (dired-get-filename))))
      (w32-shell-execute 1 filename)))
  (define-key hoagie-keymap (kbd "l") 'hoagie-dired-winstart))

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
(setq doom-modeline-major-mode-color-icon nil)
(setq doom-modeline-minor-modes t)
(setq doom-modeline-enable-word-count t)
(setq doom-modeline-checker-simple-format t)
(setq doom-modeline-persp-name nil)
(setq doom-modeline-lsp nil)
(setq doom-modeline-github nil)
(setq doom-modeline-env-version nil)
(setq doom-modeline-mu4e nil)
(setq doom-modeline-irc nil)
(doom-modeline-mode 1)

;; DOTNET
(hoagie-ensure-package 'dotnet)
(setq dotnet-mode-keymap-prefix nil)
(define-key hoagie-keymap (kbd "d") dotnet-mode-command-map)


;; FORMAT-ALL-THE-CODE
(hoagie-ensure-package 'format-all)
(global-set-key (kbd "C-c f") 'format-all-buffer)

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
(ido-mode 1)
(ido-vertical-mode 1)
(setq ido-vertical-define-keys 'C-n-and-C-p-only)
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(setq ido-create-new-buffer 'always)
(require 'icomplete)
(icomplete-mode 1)

;; IMENU
(hoagie-ensure-package 'idomenu)
(global-set-key (kbd "M-g d") 'idomenu)

;; LSP MODE
;; (hoagie-ensure-package 'lsp-mode)
;; (hoagie-ensure-package 'lsp-ui)
;; (hoagie-ensure-package 'company-lsp)
;; (require 'lsp-python-ms)
;; (add-hook 'python-mode-hook #'lsp)
;; (setq lsp-python-ms-dir "c:/home/github/ms-python-language-server/output/bin/Release/win-x64/publish/")
;; (setq lsp-python-ms-executable "c:/home/github/ms-python-language-server/output/bin/Release/win-x64/publish/Microsoft.Python.LanguageServer.exe")
(hoagie-ensure-package 'eglot)
;; (add-hook 'python-mode-hook 'eglot-ensure)
(add-to-list 'eglot-server-programs
             `(python-mode . ("c:/home/github/ms-python-language-server/output/bin/Release/win-x64/publish/Microsoft.Python.LanguageServer.exe")))

;; JSON MODE
(hoagie-ensure-package 'json-mode)
(add-to-list 'auto-mode-alist '("\\.json$" . json-mode))

;; MAGIT
(hoagie-ensure-package 'magit)
(hoagie-ensure-package 'magit-gitflow)
(define-key hoagie-keymap (kbd "m") 'magit-status)
(add-hook 'magit-mode-hook 'turn-on-magit-gitflow)

;; MINIONS
(hoagie-ensure-package 'minions)
(global-set-key [f3] 'minions-minor-modes-menu)

;; OCCUR
(require 'replace)
;; I'm surprised this isn't the default behaviour,
;; also couldn't find a way to change it from options
(defun hoagie-occur-dwim ()
  "Run occur, if there's a region selected use that as input."
  (interactive)
  (if (use-region-p)
      (occur (buffer-substring-no-properties (region-beginning) (region-end)))
    (command-execute 'occur)))
(define-key hoagie-keymap (kbd "o") 'hoagie-occur-dwim)

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
(hoagie-ensure-package 'better-shell)
(define-key hoagie-keymap (kbd "`") 'better-shell-for-current-dir)

;; SLY
(hoagie-ensure-package 'sly)
(setq inferior-lisp-program "sbcl")

;; SMEX
(hoagie-ensure-package 'smex)
(smex-initialize)
(define-key hoagie-keymap (kbd "x") 'smex)
(define-key hoagie-keymap (kbd "<menu>") 'smex)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

;; SQL MODE
(require 'sql)
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
(setq save-interprogram-paste-before-kill t)
(delete-selection-mode t)
;; sometimes this is great...other times it's annoying...
;; (electric-pair-mode)
; see https://emacs.stackexchange.com/questions/33510/unicode-txt-slowness
(setq inhibit-compacting-font-caches t)
; see https://emacs.stackexchange.com/a/28746/17066
(setq auto-window-vscroll nil)
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
(define-key hoagie-keymap (kbd "n") 'next-buffer)
(define-key hoagie-keymap (kbd "p") 'previous-buffer)
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
(define-key hoagie-keymap (kbd "f") 'find-name-dired)
(define-key hoagie-keymap (kbd "g") 'deadgrep)
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
(put 'narrow-to-region 'disabled nil)
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
(define-key hoagie-keymap (kbd "|") 'toggle-window-split)

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
