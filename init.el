;;; .emacs --- My dot emacs file

;; Author: Sebastian Monia <smonia@outlook.com>
;; URL: https://github.com/sebasmonia/.emacs
;; Version: 1
;; Keywords: .emacs dotemacs

;; This file is not part of GNU Emacs.

;;; Commentary:

;; My dot Emacs file

;;; Code:

(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)
(package-initialize)

(add-to-list 'load-path "~/.emacs.d/lisp/")
(prefer-coding-system 'utf-8)
;; CUSTOM-SET
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#100e23" "#ff8080" "#95ffa4" "#ffe9aa" "#91ddff" "#c991e1" "#aaffe4" "#BAC9E4"])
 '(anzu-deactivate-region t)
 '(anzu-mode-lighter "")
 '(anzu-replace-threshold 50)
 '(anzu-replace-to-string-separator " => ")
 '(anzu-search-threshold 1000)
 '(beacon-color "#d54e53")
 '(blink-cursor-blinks 0)
 '(bm-buffer-persistence t)
 '(bm-repository-size 1000)
 '(bubbles-game-theme (quote difficult))
 '(bubbles-grid-size (quote (20 . 15)))
 '(column-number-mode t)
 '(custom-enabled-themes (quote (doom-challenger-deep)))
 '(custom-safe-themes
   (quote
    ("06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "4138944fbed88c047c9973f68908b36b4153646a045648a22083bd622d1e636d" "a3fa4abaf08cc169b61dea8f6df1bbe4123ec1d2afeb01c17e11fdc31fc66379" default)))
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
 '(doom-challenger-deep-brighter-comments t)
 '(doom-challenger-deep-comment-bg t)
 '(doom-dracula-brighter-comments t)
 '(ediff-highlight-all-diffs t)
 '(ediff-keep-variants nil)
 '(ediff-quit-hook (quote (ediff-cleanup-mess delete-frame)))
 '(ediff-window-setup-function (quote ediff-setup-windows-plain))
 '(eww-search-prefix "http://www.bing.com/search?q=")
 '(fci-rule-color "#858FA5")
 '(flycheck-color-mode-line-face-to-color (quote mode-line-buffer-id))
 '(frame-background-mode (quote dark))
 '(global-flycheck-mode t)
 '(global-visible-mark-mode t)
 '(grep-command
   "grep --color=always -nHi -r --include=*.* -e \"pattern\" .")
 '(hl-sexp-background-color "#1c1f26")
 '(ido-default-buffer-method (quote selected-window))
 '(jdee-db-active-breakpoint-face-colors (cons "#100e23" "#906cff"))
 '(jdee-db-requested-breakpoint-face-colors (cons "#100e23" "#95ffa4"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#100e23" "#565575"))
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
    (anzu eglot telephone-line pomidor minions deadgrep expand-region format-all lyrics docker json-mode company browse-kill-ring 2048-game use-package doom-themes gist package-lint ibuffer-projectile visible-mark wttrin dashboard powershell projectile smex dired-sort-menu dired-sort-menu+ dired+ which-key ido-vertical-mode dired-narrow circe web-mode symon omnisharp magit slime nyan-mode)))
 '(pdf-view-midnight-colors (quote ("#DCDCCC" . "#383838")))
 '(pomidor-play-sound-file nil)
 '(pos-tip-background-color "#36473A")
 '(pos-tip-foreground-color "#FFFFC8")
 '(proced-filter (quote all))
 '(projectile-indexing-method (quote alien))
 '(projectile-mode t nil (projectile))
 '(projectile-switch-project-action (quote projectile-find-file-dwim))
 '(python-shell-interpreter "ipython")
 '(python-shell-interpreter-args "-i --simple-prompt")
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
 '(symon-monitors
   (quote
    (symon-windows-memory-monitor symon-windows-cpu-monitor symon-windows-battery-monitor symon-windows-network-rx-monitor symon-windows-network-tx-monitor)))
 '(symon-refresh-rate 5)
 '(symon-sparkline-use-xpm t)
 '(tool-bar-mode nil)
 '(tramp-syntax (quote default) nil (tramp))
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
 '(visible-mark-faces
   (quote
    (visible-mark-face1 visible-mark-face2 visible-mark-forward-face1 visible-mark-forward-face2)))
 '(visible-mark-max 4)
 '(weather-metno-location-latitude 39)
 '(weather-metno-location-longitude 104)
 '(weather-metno-location-msl 1600)
 '(weather-metno-location-name "Denver")
 '(web-mode-enable-css-colorization t)
 '(web-mode-enable-sql-detection t)
 '(which-key-side-window-max-width 0.4)
 '(which-key-sort-order (quote which-key-prefix-then-key-order))
 '(wttrin-default-accept-language (quote ("Accept-Language" . "en-US")))
 '(wttrin-default-cities (quote ("Denver?m" "Buenos Aires?m")))
 '(yahoo-weather-format "[%(weather) %(temperature)? %(atmosphere-humidity)%% hum]")
 '(yahoo-weather-location "Denver, USA")
 '(yahoo-weather-temperture-format "%.1f")
 '(yas-prompt-functions
   (quote
    (yas-ido-prompt yas-dropdown-prompt yas-completing-prompt yas-maybe-ido-prompt yas-no-prompt))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((((class color) (min-colors 257)) (:background "#1b182c" :foreground "#cbe3e7" :family "Consolas" :foundry "outline" :slant normal :weight normal :height 108 :width normal)) (((class color) (min-colors 256)) (:background "#1c1c1c" :foreground "#2d2d2d" :family "Consolas" :foundry "outline" :slant normal :weight normal :height 108 :width normal)) (((class color) (min-colors 16)) (:background nil :foreground "white" :family "Consolas" :foundry "outline" :slant normal :weight normal :height 108 :width normal))))
 '(diredp-compressed-file-name ((t (:foreground "slate gray"))))
 '(diredp-compressed-file-suffix ((((class color) (min-colors 257)) (:foreground "#858FA5")) (((class color) (min-colors 256)) (:foreground "#525252")) (((class color) (min-colors 16)) (:foreground "brightblack"))))
 '(diredp-deletion ((((class color) (min-colors 89)) (:foreground "#ffffff" :background "#a40000"))))
 '(diredp-deletion-file-name ((((class color) (min-colors 89)) (:foreground "#cc0000"))))
 '(diredp-dir-heading ((((class color) (min-colors 257)) (:foreground "#91ddff" :weight bold)) (((class color) (min-colors 256)) (:foreground "#51afef" :weight bold)) (((class color) (min-colors 16)) (:foreground "brightblue" :weight bold))))
 '(diredp-dir-name ((((class color) (min-colors 257)) (:foreground "#BAC9E4" :weight bold)) (((class color) (min-colors 256)) (:foreground "#dfdfdf" :weight bold)) (((class color) (min-colors 16)) (:foreground "white" :weight bold))))
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
 '(ediff-odd-diff-B ((t (:inherit ediff-odd-diff-A))))
 '(ediff-odd-diff-C ((t (:inherit ediff-odd-diff-A))))
 '(line-number ((((class color) (min-colors 257)) (:inherit default :foreground "#565575" :distant-foreground nil :weight normal :italic nil :underline nil :strike-through nil)) (((class color) (min-colors 256)) (:inherit default :foreground "#3f3f3f" :distant-foreground nil :weight normal :italic nil :underline nil :strike-through nil)) (((class color) (min-colors 16)) (:inherit default :foreground "brightblack" :distant-foreground nil :weight normal :italic nil :underline nil :strike-through nil))))
 '(line-number-current-line ((((class color) (min-colors 257)) (:inherit (hl-line default) :foreground "#cbe3e7" :distant-foreground nil :weight normal :italic nil :underline nil :strike-through nil)) (((class color) (min-colors 256)) (:inherit (hl-line default) :foreground "#2d2d2d" :distant-foreground nil :weight normal :italic nil :underline nil :strike-through nil)) (((class color) (min-colors 16)) (:inherit (hl-line default) :foreground "white" :distant-foreground nil :weight normal :italic nil :underline nil :strike-through nil))))
 '(visible-mark-face1 ((t (:box (:line-width 1 :color "turquoise")))))
 '(visible-mark-face2 ((t (:box (:line-width 1 :color "dodger blue")))))
 '(visible-mark-forward-face1 ((t (:box (:line-width 1 :color "dark green")))))
 '(visible-mark-forward-face2 ((t (:box (:line-width 1 :color "dark olive green")))) t)
 '(web-mode-block-face ((t nil))))

;; ANZU
(require 'anzu)
(global-anzu-mode +1)
(set-face-attribute 'anzu-mode-line nil
                    :foreground "light slate blue" :weight 'bold)

(define-key isearch-mode-map [remap isearch-query-replace]  #'anzu-isearch-query-replace)
(define-key isearch-mode-map [remap isearch-query-replace-regexp] #'anzu-isearch-query-replace-regexp)

;; BROWSE-KILL-RING
(require 'browse-kill-ring)
(browse-kill-ring-default-keybindings)

;; COMPANY
(require 'company)
(add-hook 'after-init-hook 'global-company-mode)

;; EXPAND REGION
(require 'expand-region)
(global-set-key (kbd "M-<SPC>") 'er/expand-region)
(global-set-key (kbd "C-S-<SPC>") 'er/expand-region)

;; DASHBOARD
(require 'dashboard)
(dashboard-setup-startup-hook)
(setq dashboard-startup-banner 'logo)
(setq dashboard-banner-logo-title "If anything at all, perfection is finally attained not when there is no longer anything to add, but when there is no longer anything to take away - Exupéry")
(setq dashboard-items '((projects . 10)
                        (recents  . 30)))
(setq initial-buffer-choice  (lambda () (get-buffer "*dashboard*")))

;; DIRED
(autoload 'dired-async-mode "dired-async.el" nil t)
(dired-async-mode 1)
(global-set-key (kbd "\C-cj") 'dired-jump)
(define-key dired-mode-map (kbd "\\") 'dired-narrow)
;; more standard binding for filtering, but I'm so used to \, leaving both
(define-key dired-mode-map (kbd "/") 'dired-narrow)
;; from SO, apparently this makes Dired faster on Windows
(setq directory-free-space-program nil)
;; from https://stackoverflow.com/questions/2284319/opening-files-with-default-windows-application-from-within-emacs
(defun w32-browser (doc)
  "Have Windows start DOC."
  (w32-shell-execute 1 doc))
(define-key dired-mode-map (kbd "\C-cl")
  (lambda () (interactive) (w32-browser (dired-replace-in-string "/" "\\" (dired-get-filename)))))
;; from the manual, to use ls instead of Elisp-ls in Windows
;(setq ls-lisp-use-insert-directory-program t)
;(setq insert-directory-program "ls")
;; from https://github.com/roryyorke/picellif/
(defun picellif-dired-marked-files ()
  "Send marked files (or current file, if none marked) in current Dired buffer to picellif."
  (interactive)
  (apply 'call-process "picellif" nil nil nil
         (dired-get-marked-files)))
(define-key dired-mode-map (kbd "W") 'picellif-dired-marked-files)
;; from https://stackoverflow.com/questions/4115465/emacs-dired-too-much-information
(defun custom-dired-mode-setup ()
  "Show less information in dired buffers."
  (dired-hide-details-mode 1))
(add-hook 'dired-mode-hook 'custom-dired-mode-setup)
                        
;; DOCKER
(global-set-key (kbd "C-c d") 'docker)

;; DOTNET
(require 'dotnet)
(setq dotnet-mode-keymap-prefix (kbd "C-c n"))
(add-hook 'csharp-mode-hook 'dotnet-mode)

;; FORMAT-ALL-THE-CODE
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
(require 'ibuffer-projectile)
(add-hook 'ibuffer-hook
    (lambda ()
      (ibuffer-projectile-set-filter-groups)
      (unless (eq ibuffer-sorting-mode 'alphabetic)
        (ibuffer-do-sort-by-alphabetic))))

;; IDO
(require 'ido-vertical-mode)
(ido-mode 1)
(ido-vertical-mode 1)
(setq ido-vertical-define-keys 'C-n-and-C-p-only)
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(setq ido-create-new-buffer 'always)

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

;; JSON MODE
(require 'json-mode)
(add-to-list 'auto-mode-alist '("\\.json$" . json-mode))

;; LSP MODE
;; from https://vxlabs.com/2018/06/08/python-language-server-with-emacs-and-lsp-mode/
;; (require 'lsp-mode)
;; (require 'lsp-ui)
;; (require 'lsp-imenu)
;; (add-hook 'lsp-after-open-hook 'lsp-enable-imenu)
;; (setq lsp-ui-sideline-ignore-duplicate t)
;; (add-hook 'lsp-mode-hook 'lsp-ui-mode)
;; (lsp-define-stdio-client lsp-python "python"
;;                            #'projectile-project-root
;;                            '("pyls"))
;; (add-hook 'python-mode-hook
;;             (lambda ()
;;               (lsp-python-enable)))
;; (eval-after-load
;;  'company
;;  '(add-to-list 'company-backends 'company-lsp))

;; (require 'lsp-python)
;; (add-hook 'python-mode-hook #'lsp-python-enable)
;; EGLOT
(require 'eglot)
(add-hook 'python-mode-hook 'eglot-ensure)

;; MAGIT
(global-set-key (kbd "C-x g") 'magit-status)

;; MINIONS
(require 'minions)
(global-set-key [f3] 'minions-minor-modes-menu)

;; NYAN MODE
(nyan-mode)
(nyan-start-animation)
(nyan-toggle-wavy-trail)

;; OMNISHARP
(require 'omnisharp)
(add-hook 'csharp-mode-hook 'omnisharp-mode)
(eval-after-load
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

;; ORG MODE
(defun org-formatted-copy (&optional b e)
  "Export region between B and E to HTML, and copy it to the clipboard."
  (interactive "r")
  (save-window-excursion
        (shell-command-on-region
         b
         e
         "pandoc -f org -t html | python c:/HomeFolder/PythonModules/htmlclip.py")))
(global-set-key (kbd "C-M-w") 'org-formatted-copy)
;see https://superuser.com/questions/71786/can-i-create-a-link-to-a-specific-email-message-in-outlook
;; (require 'org-outlook)
;; ;enable languages in org-babel
;; (with-eval-after-load 'org
;;   (org-babel-do-load-languages 'org-babel-load-languages '((plantuml . t))))

;; POMIDOR
(require 'pomidor)
(global-set-key (kbd "<f12>") #'pomidor)

;; POWERSHELL MODE
(require 'powershell)
;; avoid shadowing of global M-` in PS mode
(define-key powershell-mode-map "\M-`" nil)

;; PROJECTILE
(require 'projectile)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

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

;; SHELL
(add-hook 'shell-mode-hook
          (lambda ()
            (toggle-truncate-lines t)))

;; SMEX
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

;; TELEPHONE LINE
(require 'telephone-line)

(defface theme-accent-tp '((t (:background "dark slate blue"))) "")
;; (require 'all-the-icons)
;; (telephone-line-defsegment* telephone-line-buffer-mod1-segment ()
;;   (cond
;;    (buffer-read-only (all-the-icons-octicon "lock" :height 0.8 :v-adjust 0.1))
;;    ((buffer-modified-p) (all-the-icons-faicon "chain-broken" :height 0.8 :v-adjust -0.0 :face '(:foreground "red")))
;;    (t (all-the-icons-faicon "link" :height 0.8 :v-adjust -0.0))))

(telephone-line-defsegment* telephone-line-buffer-mod-segment ()
   (cond
    (buffer-read-only "·")
    ((buffer-modified-p) (propertize "!" 'face '(:foreground "red" :weight bold)))
    (t "-")))

(setq telephone-line-faces
      '((taccent . (theme-accent-tp . telephone-line-accent-inactive))
        (accent . (telephone-line-accent-active . telephone-line-accent-inactive))
        (nil . (mode-line . mode-line-inactive))))
(setq telephone-line-primary-left-separator 'telephone-line-abs-left
      telephone-line-secondary-left-separator 'telephone-line-nil)
(setq telephone-line-primary-right-separator 'telephone-line-abs-right
      telephone-line-secondary-right-separator 'telephone-line-nil)
(setq telephone-line-lhs
      '((nil     . (telephone-line-buffer-mod-segment))
        (taccent . (telephone-line-buffer-name-segment))
        (accent  . (telephone-line-airline-position-segment))
        (nil     . ((telephone-line-nyan-segment :active)
                    telephone-line-process-segment))))
(setq telephone-line-rhs
      '((nil     . (telephone-line-misc-info-segment))
        (accent  . (telephone-line-minions-mode-segment))
        (taccent . (telephone-line-vc-segment))
        (nil     . (telephone-line-projectile-segment))))
(telephone-line-mode 1)

;; TFSMACS
;; (require 'tfsmacs)
;; (global-set-key  "\C-ct" 'tfsmacs-map)

;; WEB MODE
(require 'web-mode)
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
(which-key-mode)
(which-key-setup-side-window-right-bottom)

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
(global-set-key (kbd "C-M-{") (lambda () (interactive)(shrink-window-horizontally 5)))
(global-set-key (kbd "C-M-}") (lambda () (interactive)(enlarge-window-horizontally 5)))
(global-set-key (kbd "C-M-_") (lambda () (interactive)(shrink-window 5)))
(global-set-key (kbd "C-M-+") (lambda () (interactive)(shrink-window -5)))
;; on trial
(global-set-key (kbd "M-o") 'other-window)
(global-set-key (kbd "M-O") 'other-frame)
(global-set-key (kbd "M-n") 'next-buffer)
(global-set-key (kbd "M-p") 'previous-buffer)
;; in some special buffers M-n and M-p are already bound (compilation, for example)
(global-set-key (kbd "M-N") 'next-buffer)
(global-set-key (kbd "M-P") 'previous-buffer)
;; used to be C-x K. Honestly I never used C-x C-k (macros) commands that much so :shrug:
(global-set-key (kbd "C-x C-k") 'kill-this-buffer)
(global-set-key (kbd "C-;") 'dabbrev-expand)
(global-set-key (kbd "M-*") 'pop-tag-mark)
(global-set-key (kbd "C-x C-r") 'rgrep)
(global-set-key (kbd "C-c M-d") 'sql-connect)
(global-set-key (kbd "<f6>") 'kmacro-start-macro)
(global-set-key (kbd "<f7>") 'kmacro-end-macro)
(global-set-key (kbd "<f8>") 'kmacro-end-and-call-macro)
(global-set-key (kbd "C-z") 'find-name-dired)
;;(global-set-key (kbd "M-z") 'rgrep)
(global-set-key (kbd "M-z") 'deadgrep)
(global-set-key (kbd "<mouse-3>") 'kill-ring-save)

; from: https://masteringemacs.org/article/fixing-mark-commands-transient-mark-mode
(defun push-mark-no-activate ()
  "Pushes `point` to `mark-ring' and does not activate the region.
Equivalent to \\[set-mark-command] when \\[transient-mark-mode] is disabled"
  (interactive)
  (push-mark (point) t nil)) ; removed the message, visible-mark takes care of this
(defun jump-to-mark ()
  "Jumps to the local mark, respecting the `mark-ring' order.
This is the same as using \\[set-mark-command] with the prefix argument."
  (interactive)
  (set-mark-command 1))

(global-set-key (kbd "C-`") 'push-mark-no-activate)
(global-set-key (kbd "M-`") 'jump-to-mark)

; from: https://emacs.stackexchange.com/questions/7244/enable-emacs-column-selection-using-mouse
;; (I very rarely use the below function, should I delete it?)
(defun mouse-start-rectangle (start-event)
  "Rectangle selection via mouse.  START-EVENT."
  (interactive "e")
  (deactivate-mark)
  (mouse-set-point start-event)
  (rectangle-mark-mode +1)
  (let ((drag-event))
    (track-mouse
      (while (progn
               (setq drag-event (read-event))
               (mouse-movement-p drag-event))
        (mouse-set-point drag-event)))))
(global-set-key (kbd "S-<down-mouse-1>") #'mouse-start-rectangle)


;; from https://stackoverflow.com/a/22176971, move auto saves and
;; back up files to a different folder so git or dotnet core won't
;; pick them up as changes or new files in the project
(setq auto-save-file-name-transforms
      `((".*" ,(concat user-emacs-directory "auto-save/") t)))

(setq backup-directory-alist
      `(("." . ,(expand-file-name
                 (concat user-emacs-directory "backups")))))
;; WORK BINDINGS
(global-set-key (kbd "<apps>") 'smex)
(global-set-key (kbd "C-<f1>") (lambda () (interactive) (dired "~/")))
(global-set-key (kbd "C-<f2>") (lambda () (interactive) (dired "C:/Repos")))
(put 'narrow-to-region 'disabled nil)
