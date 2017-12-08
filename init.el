(setq url-proxy-services
   '(("no_proxy" . "^\\(localhost\\|10.*\\)")
     ("http" . "0.0.0.0:8080")
     ("https" . "0.0.0.0:8080")))

(require 'package)
;(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("elpy" . "http://jorgenschaefer.github.io/packages/"))
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
 '(bm-buffer-persistence t)
 '(bm-repository-size 1000)
 '(column-number-mode t)
 '(custom-enabled-themes (quote (gruber-darker)))
 '(custom-safe-themes
   (quote
    ("d61fc0e6409f0c2a22e97162d7d151dee9e192a90fa623f8d6a071dbf49229c6" "0b6cb9b19138f9a859ad1b7f753958d8a36a464c6d10550119b2838cedf92171" "d6922c974e8a78378eacb01414183ce32bc8dbf2de78aabcc6ad8172547cb074" "70f073dc36e2421b5f04309792b12852ec464423a213129cbf18663ab8cdaf3f" "eb62f4ee07ab1475878add3723dfcb4abe80d0c4ba2313b8ee4c7733ffe0b6b5" "811fdfb1c3d04988a0215c0595da8e626e48182566806ece2f7a06832187e7aa" "d9a9e18d049d25494a5abe0bf23776d3e10f3b4812f04f9377e1467c89e5e3d8" "3617b11b1fd89fa6c77b5ed019b6124a22c156639bd45a8d230728f77caa20b5" default)))
 '(dired-dwim-target t)
 '(dired-listing-switches "-laogGhvD")
 '(dired-narrow-exit-action (quote ignore))
 '(dired-sort-menu-saved-config
   (quote
    ((dired-actual-switches . "-al")
     (ls-lisp-ignore-case)
     (ls-lisp-dirs-first . t))))
 '(diredp-ignore-compressed-flag t)
 '(ediff-highlight-all-diffs t)
 '(ediff-keep-variants nil)
 '(ediff-quit-hook (quote (ediff-cleanup-mess delete-frame)))
 '(ediff-window-setup-function (quote ediff-setup-windows-plain))
 '(eww-search-prefix "https://www.bing.com/search?q=")
 '(fci-rule-color "#383838")
 '(frame-brackground-mode (quote dark))
 '(grep-command
   "grep --color=always -nHi -r --include=*.* -e \"pattern\" .")
 '(ls-lisp-dirs-first t)
 '(ls-lisp-format-time-list (quote ("%Y-%m-%d %H:%M" "%Y-%m-%d %H:%M")))
 '(ls-lisp-use-insert-directory-program nil)
 '(ls-lisp-use-localized-time-format t)
 '(ls-lisp-verbosity nil)
 '(menu-bar-mode nil)
 '(nrepl-message-colors
   (quote
    ("#CC9393" "#DFAF8F" "#F0DFAF" "#7F9F7F" "#BFEBBF" "#93E0E3" "#94BFF3" "#DC8CC3")))
 '(omnisharp-server-executable-path "C:/HomeFolder/omnisharp_64/OmniSharp.exe")
 '(org-hide-emphasis-markers t)
 '(org-plantuml-jar-path "c:/HomeFolder/PlantUML/plantuml.jar")
 '(package-selected-packages
   (quote
    (smex ido-vertical-mode which-key spaceline-all-the-icons dired+ dired-sort-menu+ dired-sort-menu spaceline dired-narrow circe web-mode cyberpunk-theme grandshell-theme gruber-darker-theme lyrics xah-find ox-clip symon omnisharp magit slime bm dired-launch nyan-mode elpy)))
 '(pdf-view-midnight-colors (quote ("#DCDCCC" . "#383838")))
 '(powerline-default-separator (quote arrow))
 '(powerline-default-separator-dir (quote (left . left)))
 '(powerline-height 20)
 '(proced-filter (quote all))
 '(scroll-bar-mode nil)
 '(set-mark-command-repeat-pop t)
 '(sql-ms-options nil)
 '(sql-ms-program "sqlcmdline")
 '(sql-product (quote ms))
 '(symon-delay 5)
 '(symon-mode t)
 '(symon-monitors
   (quote
    (symon-windows-memory-monitor symon-windows-cpu-monitor symon-windows-battery-monitor symon-windows-network-rx-monitor symon-windows-network-tx-monitor)))
 '(symon-refresh-rate 5)
 '(symon-sparkline-use-xpm t)
 '(tool-bar-mode nil)
 '(tramp-syntax (quote default) nil (tramp))
 '(vc-annotate-background "#2B2B2B")
 '(vc-annotate-color-map
   (quote
    ((20 . "#BC8383")
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
     (360 . "#DC8CC3"))))
 '(vc-annotate-very-old-color "#DC8CC3")
 '(web-mode-enable-css-colorization t)
 '(web-mode-enable-sql-detection t)
 '(which-key-side-window-max-width 0.4)
 '(which-key-sort-order (quote which-key-prefix-then-key-order)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Consolas" :foundry "outline" :slant normal :weight normal :height 98 :width normal))))
 '(diredp-compressed-file-name ((t (:foreground "slate gray"))))
 '(diredp-compressed-file-suffix ((t (:foreground "slate gray"))))
 '(diredp-deletion ((t (:background "black" :foreground "red"))))
 '(diredp-deletion-file-name ((t (:background "black" :foreground "red"))))
 '(diredp-dir-heading ((t (:foreground "Yellow"))))
 '(diredp-dir-name ((t (:foreground "gold"))))
 '(diredp-flag-mark ((t (:background "black" :foreground "dark grey"))))
 '(diredp-flag-mark-line ((t (:background "black" :foreground "dark grey"))))
 '(diredp-ignored-file-name ((t (:foreground "slate gray"))))
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
 '(web-mode-block-face ((t nil))))

;; ELPY
(elpy-enable)
(setq flycheck-highlighting-mode 'lines)

;; DIRED
(dired-launch-enable)
(autoload 'dired-async-mode "dired-async.el" nil t)
(dired-async-mode 1)
(global-set-key (kbd "\C-cj") 'dired-jump)
(define-key dired-mode-map (kbd "\\") 'dired-narrow) 
;; from the manual, to use ls instead of Elisp-ls in Windows
;(setq ls-lisp-use-insert-directory-program t)
;(setq insert-directory-program "ls")

;; IDO
(require 'ido-vertical-mode)
(ido-mode 1)
(ido-vertical-mode 1)
(setq ido-vertical-define-keys 'C-n-and-C-p-only)
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(setq ido-create-new-buffer 'always)

;; .NET COMPAT
(autoload 'vbnet-mode "vbnet-mode" "Mode for editing VB.NET code." t)
 (setq auto-mode-alist (append '(("\\.\\(frm\\|bas\\|cls\\|vb\\)$" . vbnet-mode)) auto-mode-alist))

;; SPACELINE
(require 'spaceline-config)
(spaceline-emacs-theme)
;; Retry after Windows upgrade :)
;; (require 'spaceline-all-the-icons)
;; (spaceline-all-the-icons-theme)

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

;; OMNISHARP
(require 'omnisharp)
(add-hook 'csharp-mode-hook 'omnisharp-mode)
(define-key omnisharp-mode-map (kbd "M-.") 'omnisharp-auto-complete)
(define-key omnisharp-mode-map (kbd ".") 'omnisharp-add-dot-and-auto-complete)
(define-key omnisharp-mode-map (kbd "\C-cou") 'omnisharp-find-usages)
(define-key omnisharp-mode-map (kbd "\C-coi") 'omnisharp-find-implementations)
(define-key omnisharp-mode-map (kbd "\C-cod") 'omnisharp-go-to-definition)
(define-key omnisharp-mode-map (kbd "\C-coq") 'omnisharp-run-code-action-refactoring)
(define-key omnisharp-mode-map (kbd "\C-cof") 'omnisharp-fix-code-issue-at-point)
(define-key omnisharp-mode-map (kbd "\C-cor") 'omnisharp-rename)
(define-key omnisharp-mode-map (kbd "\C-coti") 'omnisharp-current-type-information)
(define-key omnisharp-mode-map (kbd "\C-cotd") 'omnisharp-current-type-documentation)
(define-key omnisharp-mode-map (kbd "\C-cos") 'omnisharp-start-omnisharp-server)
(define-key omnisharp-mode-map (kbd "<f5>") 'recompile)

;; MAGIT
(global-set-key (kbd "C-x g") 'magit-status)

;; ORG MODE
(defun org-formatted-copy (&optional b e)
  "Export region to HTML, and copy it to the clipboard."
  (interactive "r")
  (save-window-excursion
        (shell-command-on-region
         b
         e
         "pandoc -f org -t html | python c:/HomeFolder/PythonModules/htmlclip.py")))
(global-set-key (kbd "C-M-w") 'org-formatted-copy)
;see https://superuser.com/questions/71786/can-i-create-a-link-to-a-specific-email-message-in-outlook
(require 'org-outlook)
;enable languages in org-babel
(with-eval-after-load 'org
  (org-babel-do-load-languages 'org-babel-load-languages '((plantuml . t))))


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
            (linum-mode 0)
            (toggle-truncate-lines t)
            (sql-rename-buffer-prompt)))

;; NYAN MODE
(nyan-mode)
(nyan-start-animation)
(nyan-toggle-wavy-trail)

;; SMEX
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

;; WHICH KEY
(which-key-mode)
(which-key-setup-side-window-right-bottom)

;; WHITESPACE MODE

;; removed from CUSTOM above
;; '(global-whitespace-mode t)
;; '(whitespace-style
;;   (quote
;;    (face trailing tabs spaces newline empty indentation space-after-tab space-before-tab space-mark tab-mark newline-mark)))

; (setq whitespace-display-mappings '(
  ; (space-mark   ?\     [?\u00B7]     [?.])
  ; (space-mark   ?\xA0  [?\u00A4]     [?_])
  ; (newline-mark ?\n    [?\u21B5 ?\n])
  ; (tab-mark     ?\t    [?\u2192 ?\t] [?\\ ?\t])))

;; MISC
(setq-default indent-tabs-mode nil)  ; use only spaces and no tabs
(setq default-tab-width 4)
(global-linum-mode t)
(global-hl-line-mode t)
(setq frame-title-format "%b - Emacs")
(global-set-key (kbd "M-RET") 'toggle-frame-fullscreen)
(delete-selection-mode t)
; see https://emacs.stackexchange.com/questions/33510/unicode-txt-slowness
(setq inhibit-compacting-font-caches t)
; from https://emacs.stackexchange.com/questions/7362/how-to-show-a-diff-between-two-buffers-with-character-level-diffs
(setq-default ediff-forward-word-function 'forward-char)
(add-to-list 'default-frame-alist '(fullscreen . maximized))
(global-set-key (kbd "M-o") 'other-window)
(global-set-key (kbd "M-N") 'next-buffer)
(global-set-key (kbd "M-P") 'previous-buffer)
(global-set-key (kbd "C-x K") 'kill-this-buffer)
(global-set-key (kbd "C-x C-d") 'find-name-dired)
(global-set-key (kbd "C-'") 'dabbrev-expand)

; from: https://emacs.stackexchange.com/questions/7244/enable-emacs-column-selection-using-mouse
(defun mouse-start-rectangle (start-event)
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

;; modified version of the one in https://www.emacswiki.org/emacs/InsertDate
(defun insert-date (prefix)
  "Insert the current date. With prefix-argument, use ISO format. With
   two prefix arguments, write out the day and month name."
  (interactive "P")
  (let ((format (if prefix "%Y-%m-%dT%H:%M:%S" "%Y-%m-%d")))
    (insert (format-time-string format))))
(global-set-key (kbd "C-c d") 'insert-date)

(defun dired-file-to-clip ()
  "Invoke the file2clip script in the file at point"
  (interactive)
  (shell-command (concat "f2c " (dired-get-filename))))

(define-key dired-mode-map (kbd "W") 'dired-file-to-clip) 
(put 'upcase-region 'disabled nil)

