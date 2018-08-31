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
   ["#212526" "#ff4b4b" "#b4fa70" "#fce94f" "#729fcf" "#e090d7" "#8cc4ff" "#eeeeec"])
 '(bubbles-game-theme 'difficult)
 '(bubbles-grid-size '(20 . 15))
 '(column-number-mode t)
 '(custom-enabled-themes '(gruber-darker))
 '(custom-safe-themes
   '("c2831730b24d526a7b6268a9e42e8e57d2a1279c8f92c5db554af03d4333af2c" "021720af46e6e78e2be7875b2b5b05344f4e21fad70d17af7acfd6922386b61e" "e88abed2a39b47dfedb1272066f214cb2c9db28ee6aa1794bfb27948792f81c0" "a4d03266add9a1c8f12b5309612cbbf96e1291773c7bc4fb685bfdaf83b721c6" "ed0b4fc082715fc1d6a547650752cd8ec76c400ef72eb159543db1770a27caa7" "7356632cebc6a11a87bc5fcffaa49bae528026a78637acd03cae57c091afd9b9" "04dd0236a367865e591927a3810f178e8d33c372ad5bfef48b5ce90d4b476481" "d6922c974e8a78378eacb01414183ce32bc8dbf2de78aabcc6ad8172547cb074" "a24c5b3c12d147da6cef80938dca1223b7c7f70f2f382b26308eba014dc4833a" "c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "3edbdd0ad45cb8f7c2575c0ad8f6625540283c6e928713c328b0bacf4cfbb60f" "eecacf3fb8efc90e6f7478f6143fd168342bbfa261654a754c7d47761cec07c8" "70f073dc36e2421b5f04309792b12852ec464423a213129cbf18663ab8cdaf3f" "ff7625ad8aa2615eae96d6b4469fcc7d3d20b2e1ebc63b761a349bebbb9d23cb" "580d632430ae18daa5109ecd675c1b8df91d7e1e657f049b36bb6fc3c79bfc41" "d61fc0e6409f0c2a22e97162d7d151dee9e192a90fa623f8d6a071dbf49229c6" "dcb9fd142d390bb289fee1d1bb49cb67ab7422cd46baddf11f5c9b7ff756f64c" default))
 '(diary-entry-marker 'font-lock-variable-name-face)
 '(dired-dwim-target t)
 '(dired-listing-switches "-laogGhvD")
 '(dired-sort-menu-saved-config
   ('((dired-actual-switches . "-al")
      (ls-lisp-ignore-case)
      (ls-lisp-dirs-first . t))))
 '(diredp-ignore-compressed-flag t)
 '(display-line-numbers 'relative)
 '(display-line-numbers-current-absolute nil)
 '(ediff-highlight-all-diffs t)
 '(ediff-keep-variants nil)
 '(ediff-quit-hook '(ediff-cleanup-mess delete-frame))
 '(ediff-window-setup-function 'ediff-setup-windows-plain)
 '(eww-search-prefix "http://www.bing.com/search?q=")
 '(fci-rule-color "#383838")
 '(frame-brackground-mode 'dark)
 '(global-flycheck-mode t)
 '(global-visible-mark-mode t)
 '(grep-command
   "grep --color=always -nHi -r --include=*.* -e \"pattern\" .")
 '(hl-sexp-background-color "#1c1f26")
 '(ls-lisp-dirs-first t)
 '(ls-lisp-format-time-list '("%Y-%m-%d %H:%M" "%Y-%m-%d %H:%M"))
 '(ls-lisp-use-insert-directory-program nil)
 '(ls-lisp-use-localized-time-format t)
 '(ls-lisp-verbosity nil)
 '(menu-bar-mode nil)
 '(nrepl-message-colors
   '("#CC9393" "#DFAF8F" "#F0DFAF" "#7F9F7F" "#BFEBBF" "#93E0E3" "#94BFF3" "#DC8CC3"))
 '(omnisharp-imenu-support t)
 '(omnisharp-server-executable-path "C:/HomeFolder/omnisharp_64/OmniSharp.exe")
 '(org-hide-emphasis-markers t)
 '(org-plantuml-jar-path "c:/HomeFolder/PlantUML/plantuml.jar")
 '(package-selected-packages
   '(package-lint tablist yahoo-weather ibuffer-projectile visible-mark elpy wttrin yasnippet yasnippet-snippets dashboard powershell projectile smex ido-vertical-mode which-key spaceline-all-the-icons dired+ dired-sort-menu+ dired-sort-menu spaceline dired-narrow circe web-mode gruber-darker-theme lyrics xah-find symon omnisharp magit slime dired-launch nyan-mode))
 '(pdf-view-midnight-colors '("#DCDCCC" . "#383838"))
 '(pos-tip-background-color "#36473A")
 '(pos-tip-foreground-color "#FFFFC8")
 '(powerline-default-separator-dir '(left . left))
 '(proced-filter 'all)
 '(projectile-indexing-method 'alien)
 '(projectile-mode t nil (projectile))
 '(projectile-switch-project-action 'projectile-find-file-dwim)
 '(scroll-bar-mode nil)
 '(set-mark-command-repeat-pop t)
 '(spaceline-all-the-icons-clock-always-visible nil)
 '(spaceline-all-the-icons-eyebrowse-display-name nil)
 '(spaceline-all-the-icons-flycheck-alternate t)
 '(spaceline-all-the-icons-hide-long-buffer-path t)
 '(spaceline-all-the-icons-highlight-file-name t)
 '(spaceline-all-the-icons-icon-set-bookmark 'star)
 '(spaceline-all-the-icons-icon-set-eyebrowse-slot 'string)
 '(spaceline-all-the-icons-icon-set-flycheck-slim 'outline)
 '(spaceline-all-the-icons-icon-set-git-ahead 'commit)
 '(spaceline-all-the-icons-icon-set-modified 'chain)
 '(spaceline-all-the-icons-icon-set-sun-time 'sun/moon)
 '(spaceline-all-the-icons-icon-set-window-numbering 'square)
 '(spaceline-all-the-icons-primary-separator "")
 '(spaceline-all-the-icons-secondary-separator "")
 '(spaceline-all-the-icons-separator-type 'slant)
 '(spaceline-all-the-icons-separators-invert-direction t)
 '(spaceline-all-the-icons-slim-render nil)
 '(sunshine-units 'metric)
 '(symon-delay 5)
 '(symon-mode t)
 '(symon-refresh-rate 5)
 '(symon-sparkline-use-xpm t)
 '(tool-bar-mode nil)
 '(tramp-syntax 'default nil (tramp))
 '(vc-annotate-background "#2B2B2B")
 '(vc-annotate-color-map
   '((20 . "#BC8383")
     (40 . "#CC9393")
     (60 . "#DFAF8F")
     (80 . "#D0BF8F")
     (100 . "#E0CF9F")
     (120 . "#F0DFAF")
     (140 . "#5F7F5F")
     (160 . "#7F9F7F")
     (180 . "#8FB28F")
     (200 . "#9FC59F")
     (220 . "#AFD8AF")
     (240 . "#BFEBBF")
     (260 . "#93E0E3")
     (280 . "#6CA0A3")
     (300 . "#7CB8BB")
     (320 . "#8CD0D3")
     (340 . "#94BFF3")
     (360 . "#DC8CC3")))
 '(vc-annotate-very-old-color "#DC8CC3")
 '(visible-mark-faces
   '(visible-mark-face1 visible-mark-face2 visible-mark-forward-face1 visible-mark-forward-face2))
 '(visible-mark-max 4)
 '(web-mode-enable-css-colorization t)
 '(web-mode-enable-sql-detection t)
 '(which-key-side-window-max-width 0.4)
 '(which-key-sort-order 'which-key-prefix-then-key-order)
 '(wttrin-default-accept-language '("Accept-Language" . "en-US"))
 '(wttrin-default-cities '("Denver?m" "Buenos Aires?m"))
 '(yahoo-weather-format "[%(weather) %(temperature)℃    %(atmosphere-humidity)]")
 '(yahoo-weather-location "Denver, USA")
 '(yahoo-weather-mode t)
 '(yas-prompt-functions
   '(yas-ido-prompt yas-dropdown-prompt yas-completing-prompt yas-maybe-ido-prompt yas-no-prompt)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Consolas" :foundry "outline" :slant normal :weight normal :height 113 :width normal))))
 '(diredp-compressed-file-name ((t (:foreground "slate gray"))))
 '(diredp-compressed-file-suffix ((((class color) (min-colors 89)) (:foreground "#b218b2"))))
 '(diredp-deletion ((((class color) (min-colors 89)) (:foreground "#ffffff" :background "#a40000"))))
 '(diredp-deletion-file-name ((((class color) (min-colors 89)) (:foreground "#cc0000"))))
 '(diredp-dir-heading ((((class color) (min-colors 89)) (:foreground "#5f5f5f" :background "#d7ff00" :bold t))))
 '(diredp-dir-name ((t (:foreground "gold"))))
 '(diredp-flag-mark ((((class color) (min-colors 89)) (:foreground "#ffffff" :background "#ff1f8b" :bold t))))
 '(diredp-flag-mark-line ((((class color) (min-colors 89)) (:foreground "#5f5f5f" :background "#ff7bbb"))))
 '(diredp-ignored-file-name ((((class color) (min-colors 89)) (:foreground "#b2b2b2"))))
 '(ediff-current-diff-B ((t (:background "#553333"))))
 '(ediff-current-diff-C ((t (:background "#553333"))))
 '(ediff-even-diff-A ((t (:background "dark slate gray"))))
 '(ediff-even-diff-Ancestor ((t (:background "cornflower blue"))))
 '(ediff-even-diff-B ((t (:background "dark slate gray"))))
 '(ediff-even-diff-C ((t (:background "dark slate gray"))))
 '(ediff-fine-diff-Ancestor ((t (:background "DodgerBlue2"))))
 '(ediff-fine-diff-B ((t (:background "#aa2222"))))
 '(ediff-fine-diff-C ((t (:background "#aa2222"))))
 '(ediff-odd-diff-A ((t (:background "dark slate gray"))))
 '(ediff-odd-diff-Ancestor ((t (:background "cornflower blue"))))
 '(ediff-odd-diff-B ((t (:background "dark slate gray"))))
 '(ediff-odd-diff-C ((t (:background "dark slate gray"))))
 '(line-number ((t (:foreground "DarkGoldenrod2"))))
 '(line-number-current-line ((t (:inherit line-number :background "dark slate gray"))))
 '(spaceline-unmodified ((t (:background "DodgerBlue1" :foreground "white" :inherit 'mode-line))))
 '(visible-mark-face1 ((t (:box (:line-width 1 :color "turquoise")))))
 '(visible-mark-face2 ((t (:box (:line-width 1 :color "dodger blue")))))
 '(visible-mark-forward-face1 ((t (:box (:line-width 1 :color "dark green")))))
 '(visible-mark-forward-face2 ((t (:background "dark olive green"))) t)
 '(web-mode-block-face ((t nil))))

;; COMPANY
(add-hook 'after-init-hook 'global-company-mode)

;; DASHBOARD
(require 'dashboard)
(dashboard-setup-startup-hook)
(setq dashboard-startup-banner 'logo)
(setq dashboard-banner-logo-title "If anything at all, perfection is finally attained not when there is no longer anything to add, but when there is no longer anything to take away - Exupéry")
(setq dashboard-items '((projects . 10)
                        (recents  . 30)))
(setq initial-buffer-choice  (lambda () (get-buffer "*dashboard*")))

;; DIRED
(require 'dired)
(dired-launch-enable)
(autoload 'dired-async-mode "dired-async.el" nil t)
(dired-async-mode 1)
(global-set-key (kbd "\C-cj") 'dired-jump)
(define-key dired-mode-map (kbd "\\") 'dired-narrow)
;; from the manual, to use ls instead of Elisp-ls in Windows
;(setq ls-lisp-use-insert-directory-program t)
;(setq insert-directory-program "ls")

;; ELFEED
(global-set-key (kbd "C-c e") 'elfeed)
(setq elfeed-feeds
      '("https://blogs.msdn.microsoft.com/oldnewthing/feed"
        "http://pragmaticemacs.com/feed/"
        "http://planet.emacsen.org/atom.xml"
        "http://oremacs.com/atom.xml"
        "http://endlessparentheses.com/atom.xml"
        "https://www.jwz.org/blog/feed/"))

;; ELPY
(elpy-enable)
(setq flycheck-highlighting-mode 'lines)
(setq python-shell-interpreter "jupyter"
      python-shell-interpreter-args "console --simple-prompt")

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
; from https://gist.github.com/magnars/2360578
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

;; MAGIT
(global-set-key (kbd "C-x g") 'magit-status)

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
(define-key omnisharp-mode-map (kbd ".") 'omnisharp-add-dot-and-auto-complete)
(define-key omnisharp-mode-map (kbd "C-c o e") 'omnisharp-solution-errors)
(define-key omnisharp-mode-map (kbd "C-c o u") 'omnisharp-find-usages)
(define-key omnisharp-mode-map (kbd "C-c o i") 'omnisharp-find-implementations)
(define-key omnisharp-mode-map (kbd "C-c o d") 'omnisharp-go-to-definition)
(define-key omnisharp-mode-map (kbd "M-.") 'omnisharp-go-to-definition) ; more standard
(define-key omnisharp-mode-map (kbd "C-c o q") 'omnisharp-run-code-action-refactoring)
(define-key omnisharp-mode-map (kbd "C-c o f") 'omnisharp-fix-code-issue-at-point)
(define-key omnisharp-mode-map (kbd "C-c o r") 'omnisharp-rename)
(define-key omnisharp-mode-map (kbd "C-c o t i") 'omnisharp-current-type-information)
(define-key omnisharp-mode-map (kbd "C-c o t d") 'omnisharp-current-type-documentation)
(define-key omnisharp-mode-map (kbd "<f5>") 'recompile)

;; PROJECTILE
(projectile-mode)
(setq projectile-switch-project-action 'projectile-find-file-dwim)

;; SQL MODE
(require 'sql)
(sql-set-product-feature 'ms :prompt-regexp "^.*>")
(sql-set-product-feature 'ms :prompt-cont-regexp "^.*>")
;After moving to Emacs 26.0.9, I don't get prompted for buffer name when doing C-u M-x sql-connect
;added the function below and a call in the SQLi hook to go back to the old behaviour 
(defun sql-rename-buffer-prompt ()
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
(global-set-key (kbd "<menu>") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

;; SPACELINE
(require 'spaceline-config)
(spaceline-emacs-theme)
(require 'spaceline-all-the-icons)
(spaceline-all-the-icons-theme)
(spaceline-toggle-all-the-icons-time-off)
(spaceline-toggle-all-the-icons-hud-off)
(spaceline-toggle-selection-info-on)
(spaceline-toggle-global-on)

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
  (setq web-mode-markup-indent-offset 2)
)
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
(global-set-key (kbd "M-o") 'other-window)
(global-set-key (kbd "M-O") 'other-frame)
(global-set-key (kbd "M-N") 'next-buffer)
(global-set-key (kbd "M-P") 'previous-buffer)
(global-set-key (kbd "C-x K") 'kill-this-buffer)
(global-set-key (kbd "C-x C-d") 'find-name-dired)
(global-set-key (kbd "C-'") 'dabbrev-expand)
(global-set-key (kbd "M-*") 'pop-tag-mark)
(global-set-key (kbd "C-x C-r") 'rgrep)
(global-set-key (kbd "C-c M-d") 'sql-connect)
(global-set-key (kbd "<f6>") 'kmacro-start-macro)
(global-set-key (kbd "<f7>") 'kmacro-end-macro)
(global-set-key (kbd "<f8>") 'kmacro-end-and-call-macro)
(global-set-key (kbd "<f8>") 'kmacro-end-and-call-macro)
(global-set-key (kbd "<f8>") 'kmacro-end-and-call-macro)

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
;until I figure out what works
(global-set-key (kbd "C-`") 'push-mark-no-activate)
(global-set-key (kbd "M-`") 'jump-to-mark)

(global-set-key (kbd "C-<f1>")
  (lambda ()
    (interactive)
    (dired "~/")))

;; TRAMP
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

(global-set-key (kbd "C-c C-r") 'find-alternative-file-with-sudo)
