(setq url-proxy-services
   '(("no_proxy" . "^\\(localhost\\|10.*\\)")
     ("http" . "10.10.10.10:8080")
     ("https" . "10.10.10.10:8080")))

(require 'package)
;(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("elpy" . "http://jorgenschaefer.github.io/packages/"))
(package-initialize)

(add-to-list 'load-path "~/.emacs.d/lisp/")

;; CUSTOM-SET
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(bm-buffer-persistence t)
 '(bm-repository-size 1000)
 '(custom-enabled-themes (quote (zenburn)))
 '(custom-safe-themes
   (quote
    ("3617b11b1fd89fa6c77b5ed019b6124a22c156639bd45a8d230728f77caa20b5" default)))
 '(dired-listing-switches "-laGh1v")
 '(dired-sort-menu-saved-config
   (quote
    ((dired-actual-switches . "-al")
     (ls-lisp-ignore-case)
     (ls-lisp-dirs-first . t))))
 '(ls-lisp-dirs-first t)
 '(ls-lisp-format-time-list (quote ("%Y-%m-%d %H:%M" "%Y-%m-%d %H:%M")))
 '(ls-lisp-use-localized-time-format t)
 '(ls-lisp-verbosity nil)
 '(menu-bar-mode nil)
 '(package-selected-packages (quote (fill-column-indicator omnisharp magit slime bm guide-key neotree dired-launch nyan-mode elpy)))
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


;; BOOKMARKS - BM
(require 'bm)
(global-set-key (kbd "\C-ckk") 'bm-toggle)
(global-set-key (kbd "\C-ckp") 'bm-previous)
(global-set-key (kbd "\C-ckn") 'bm-next)
(global-set-key (kbd "\C-ckr") 'bm-bookmark-regexp)
(global-set-key (kbd "\C-cks") 'bm-save)
(global-set-key (kbd "\C-ckl") 'bm-load-and-restore)

;; ELPY
(elpy-enable)
(setq flycheck-highlighting-mode 'lines)

;; DIRED
(dired-launch-enable)

;; FCI
(setq fci-rule-color "grey")
(setq fci-rule-width 2)
(setq fci-rule-column 80)

(define-globalized-minor-mode global-fci-mode fci-mode
  (lambda ()
    (if (and
         (not (string-match "^\*.*\*$" (buffer-name)))
         (not (eq major-mode 'dired-mode)))
        (fci-mode 1))))
(global-fci-mode 1)

;; IDO
(require 'ido-vertical-mode)
(ido-mode 1)
(setq ido-enable-flex-matching t)
(ido-vertical-mode 1)
(setq ido-vertical-define-keys 'C-n-and-C-p-only)

;; .NET COMPAT
(autoload 'vbnet-mode "vbnet-mode" "Mode for editing VB.NET code." t)
(setq auto-mode-alist (append '(("\\.\\(frm\\|bas\\|cls\\|vb\\)$" .
                             vbnet-mode)) auto-mode-alist))

;; OMNISHARP
(require 'omnisharp)
(setq omnisharp-server-executable-path "C:/HomeFolder/omnisharp_server/OmniSharp.exe")
(add-hook 'csharp-mode-hook 'omnisharp-mode)
(add-hook 'vbnet-mode-hook 'omnisharp-mode)
(define-key omnisharp-mode-map (kbd "M-.") 'omnisharp-auto-complete)
(define-key omnisharp-mode-map (kbd ".") 'omnisharp-add-dot-and-auto-complete)
(define-key omnisharp-mode-map (kbd "<f12>") 'omnisharp-go-to-definition)
(define-key omnisharp-mode-map (kbd "[(shift f12)]") 'omnisharp-find-usages)
(define-key omnisharp-mode-map (kbd "\C-cou") 'omnisharp-find-usages)
(define-key omnisharp-mode-map (kbd "\C-coi") 'omnisharp-find-implementations)
(define-key omnisharp-mode-map (kbd "\C-cod") 'omnisharp-go-to-definition)
(define-key omnisharp-mode-map (kbd "\C-coq") 'omnisharp-run-code-action-refactoring)
(define-key omnisharp-mode-map (kbd "\C-cof") 'omnisharp-fix-code-issue-at-point)
(define-key omnisharp-mode-map (kbd "\C-cor") 'omnisharp-rename)
(define-key omnisharp-mode-map (kbd "\C-coti") 'omnisharp-current-type-information)
(define-key omnisharp-mode-map (kbd "\C-cotd") 'omnisharp-current-type-documentation)
(define-key omnisharp-mode-map (kbd "\C-cos") 'omnisharp-start-omnisharp-server)
;(define-key omnisharp-mode-map (kbd "<f5>") 'recompile)

;; MAGIT
(global-set-key (kbd "C-x g") 'magit-status)

;; ORG MODE
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)
(setq org-agenda-files (list "~/org/work.org"))

;; SQL MODE
(add-hook 'sql-interactive-mode-hook
          (lambda ()
            (toggle-truncate-lines t)))

;; GUIDE-KEY
(guide-key-mode 1)
(setq guide-key/guide-key-sequence t)

;; NYAN MODE
(nyan-mode)
(nyan-start-animation)
(nyan-toggle-wavy-trail)

;; NEOTREE
(global-set-key [f8] 'neotree-toggle)
(setq neo-smart-open t)

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

;; TRAMP

; From EmacsWiki, switch buffer (or open from dired) using sudo
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

(global-set-key (kbd "C-x C-r") 'find-alternative-file-with-sudo)

;; MISC
(setq-default indent-tabs-mode nil)  ; use only spaces and no tabs
(setq default-tab-width 4)
(global-linum-mode t)
(setq frame-title-format "%b - Emacs")
;(add-to-list 'default-frame-alist '(fullscreen . fullscreen))

;; modified version of the one in https://www.emacswiki.org/emacs/InsertDate
(defun insert-date (prefix)
  "Insert the current date. With prefix-argument, use ISO format. With
   two prefix arguments, write out the day and month name."
  (interactive "P")
  (let ((format (if prefix "%Y-%m-%dT%H:%M:%S" "%Y-%m-%d")))
    (insert (format-time-string format))))
(global-set-key (kbd "C-c d") 'insert-date)

(global-set-key (kbd "C-<f1>")
  (lambda ()
    (interactive)
    (dired "~/")))

(global-set-key (kbd "C-<f2>")
  (lambda ()
    (interactive)
    (dired "c:/WorkingFolder/")))

(global-set-key (kbd "C-<f3>")
  (lambda ()
    (interactive)
    (dired "c:/PythonModules/")))

(global-set-key (kbd "C-<f4>")
  (lambda ()
    (interactive)
    (dired "c:/Repo/")))

(defun go-org()
  (interactive)
  (find-file "~/org/Notes.org")
  (find-file "~/org")
  (find-file "~/org/Work.org"))

; terminal-only customizations
(unless (display-graphic-p) 
    (disable-theme 'zenburn) ; disable theme
  )
