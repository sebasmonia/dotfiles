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
 '(package-selected-packages (quote (magit slime bm guide-key neotree dired-launch nyan-mode elpy)))
   (quote
    (("hr-prod"
      (sql-product
       (quote ms))
      (sql-user "")
      (sql-password "")
      (sql-server "SQLPRD08")
      (sql-database "HumanResources"))
     ("continuity-prod"
      (sql-product
       (quote ms))
      (sql-user "")
      (sql-password "")
      (sql-server "SLS_Continuity_PROD_SQL_AG")
      (sql-database "SLS_Continuity"))
     ("continuity-uat"
      (sql-product
       (quote ms))
      (sql-user "")
      (sql-password "")
      (sql-server "UAT_SLS_Continuity_SQL_AG")
      (sql-database "SLS_Continuity_UAT"))
     ("irt-uat"
      (sql-product
       (quote ms))
      (sql-user "")
      (sql-password "")
      (sql-server "UAT_SLS_ImageRequest_SQL_AG")
      (sql-database "SLS_ImageRequest_UAT"))
     ("irt-prod"
      (sql-product
       (quote ms))
      (sql-user "")
      (sql-server "SLS_ImageRequest_Prod_PROD_SQL_AG")
      (sql-database "SLS_ImageRequest")))))
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

(autoload 'csharp-mode "csharp-mode" "Major mode for editing C# code." t)
(setq auto-mode-alist
  (append '(("\\.cs$" . csharp-mode)) auto-mode-alist))

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

;; MAGIT
(global-set-key (kbd "C-x g") 'magit-status)

;; MISC
(setq-default indent-tabs-mode nil)  ; use only spaces and no tabs
(setq default-tab-width 4)
(global-linum-mode t)
;(add-to-list 'default-frame-alist '(fullscreen . fullscreen))


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
