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
 '(blink-cursor-mode nil)
 '(column-number-mode t)
 '(confirm-nonexistent-file-or-buffer nil)
 '(custom-enabled-themes (quote (doom-challenger-deep)))
 '(custom-safe-themes
   (quote
    ("a3fa4abaf08cc169b61dea8f6df1bbe4123ec1d2afeb01c17e11fdc31fc66379" default)))
 '(dabbrev-case-distinction nil)
 '(dabbrev-case-fold-search t)
 '(dabbrev-case-replace nil)
 '(default-frame-alist (quote ((fullscreen . maximized))))
 '(delete-by-moving-to-trash t)
 '(delete-selection-mode t)
 '(doom-challenger-deep-brighter-comments nil)
 '(doom-challenger-deep-brighter-modeline t)
 '(doom-challenger-deep-comment-bg t)
 '(doom-dracula-brighter-comments t)
 '(eww-search-prefix "http://www.bing.com/search?q=")
 '(flycheck-color-mode-line-face-to-color (quote mode-line-buffer-id))
 '(global-flycheck-mode t)
 '(global-mark-ring-max 32)
 '(grep-command
   "grep --color=always -nHi -r --include=*.* -e \"pattern\" .")
 '(hl-sexp-background-color "#1c1f26")
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(initial-buffer-choice t)
 '(initial-scratch-message
   ";; Il semble que la perfection soit atteinte non quand il n'y a plus rien à ajouter, mais quand il n'y a plus à retrancher. - Antoine de Saint Exupéry
;; It seems that perfection is attained not when there is nothing more to add, but when there is nothing more to remove.
")
 '(lisp-mode-hook
   (quote
    (#[nil "\300\301\302\303\211$\207"
           [add-hook font-lock-extend-region-functions sly-extend-region-for-font-lock t]
           5]
     common-lisp-lisp-mode-hook sly-editing-mode
     #[nil "\300\301\302\303\211$\207"
           [add-hook font-lock-extend-region-functions slime-extend-region-for-font-lock t]
           5])))
 '(mark-ring-max 32)
 '(menu-bar-mode nil)
 '(org-fontify-emphasized-text nil)
 '(org-hide-emphasis-markers t)
 '(package-selected-packages
   (quote
    (csv android-mode yaml-mode ws-butler which-key web-mode visible-mark smex sly better-shell ibuffer-projectile projectile powershell package-lint omnisharp minions magit-gitflow magit lyrics company-lsp lsp-ui lsp-mode idomenu ido-vertical-mode format-all dotnet doom-themes doom-modeline dockerfile-mode docker deadgrep dired-git-info dired-narrow eww-lnum expand-region company browse-kill-ring anzu 2048-game use-package)))
 '(pdf-view-midnight-colors (quote ("#DCDCCC" . "#383838")))
 '(pomidor-play-sound-file nil)
 '(proced-filter (quote all))
 '(python-shell-interpreter "ipython")
 '(python-shell-interpreter-args "--pprint ")
 '(reb-re-syntax (quote string))
 '(revert-without-query (quote (".*")))
 '(save-interprogram-paste-before-kill t)
 '(savehist-mode t)
 '(scroll-bar-mode nil)
 '(set-mark-command-repeat-pop t)
 '(size-indication-mode t)
 '(sql-ms-options nil)
 '(sql-ms-program "sqlcmdline")
 '(sql-product (quote ms))
 '(tool-bar-mode nil)
 '(tooltip-mode nil)
 '(tramp-syntax (quote default) nil (tramp))
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
 '(visible-bell t)
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
 '(visible-mark-face1 ((t (:box (:line-width 1 :color "red")))))
 '(visible-mark-face2 ((t (:box (:line-width 1 :color "orange")))))
 '(visible-mark-forward-face1 ((t (:box (:line-width 1 :color "green")))))
 '(visible-mark-forward-face2 ((t (:box (:line-width 1 :color "yellow")))) t))
