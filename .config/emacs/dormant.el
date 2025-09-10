;; purgatory.el --- Things that I used to use  -*- lexical-binding: t; -*-

;; Author: Sebastian Monia <code@sebasmonia.com>
;; URL: https://git.sr.ht/~sebasmonia/dotfiles
;; Version: 1.0
;; Keywords: tools maint

;; This file is not part of GNU Emacs.

;;; Commentary:

;; I know I can go hunt via "git log" to revive older configuration blocks (and
;; I have done it!). But I want to try a different approach as I move here
;; things that I suspect I might need, vs deleted code that I am trying not to
;; rely on anymore.

;;; Code:

(use-package csharp-mode
  :ensure t
  :mode "\\.cs$"
  :hook
  (csharp-mode-hook . subword-mode))

(use-package docker
  :ensure t
  :bind
  ("C-c d" . docker))

(use-package dockerfile-mode
  :ensure t
  :mode "Dockerfile\\'")

;; I didn't really click w/ using feeds - but I want to revisit the idea
(use-package elfeed
  :ensure t
  :custom
  (elfeed-sort-order 'ascending)
  (elfeed-db-directory (expand-file-name "elfeed" user-emacs-directory))
  (elfeed-feeds '("http://nullprogram.com/feed/"
                  "https://irreal.org/blog/?feed=rss2"
                  "https://lars.ingebrigtsen.no/feed/"
                  "https://feeds.npr.org/1001/rss.xml"
                  "https://batsov.com/atom.xml"
                  "https://manueluberti.eu/feed.xml"
                  "https://metaredux.com/feed.xml"
                  "https://lisp-journey.gitlab.io/index.xml"
                  "https://www.n16f.net/blog/index.xml"
                  "https://borretti.me/feed.xml"
                  "https://www.n16f.net/blog/index.xml"
                  "https://planet.emacslife.com/atom.xml"
                  "https://batimes.com.ar/feed"))
  ;;                  "https://feeds.npr.org/1002/rss.xml"))
  (elfeed-show-entry-switch 'pop-to-buffer)
  :bind
  ;; match eww/help bindings
  (:map elfeed-show-mode-map
        ("r" . elfeed-show-next)
        ("l" . elfeed-show-prev))
  (:map hoagie-second-keymap
        ("r" . elfeed)))

;; from https://emacs.stackexchange.com/a/36287 I want this for a
;; function to open the gcloud docs but I think it is useful as a
;; general tool to have around
;; UPDATE: never used this other than for the Go docs...so here it is,
;; in the purgatory
(defun hoagie-eww-readable (url &optional new-buffer)
  "Open URL, after the page loads, call `eww-readable'.
Optional argument NEW-BUFFER is passed to `eww' as prefix arg."
  ;;TIL letrec, too
  (letrec ((nonce (lambda ()
                    (unwind-protect
                        (eww-readable)
                      (remove-hook 'eww-after-render-hook nonce)))))
    (add-hook 'eww-after-render-hook nonce))
  (eww url new-buffer))

(use-package go-mode
  :ensure t
  :hook
  (go-mode-hook . subword-mode)
  :config
  (defun hoagie-golang-stdlib-reference ()
    "Open the Go stdlib reference in EWW."
    (interactive)
    (hoagie-eww-readable (format "https://pkg.go.dev/%s"
                                 (read-string "Package: ")))))

(use-package java-mode
  :hook
  (java-mode-hook . subword-mode))

(use-package kubernetes
  :ensure t
  :commands (kubernetes-overview)
  :bind
  ("C-c K" . kubernetes-overview)
  ;; experimental alternative
  (:map hoagie-keymap
        ("K" . kubernetes-overview))
  :custom
  ;; setting these means I have to manually
  ;; refresh the "main" screen
  (kubernetes-poll-frequency 3600)
  (kubernetes-redraw-frequency 3600)
  (kubernetes-pods-display-completed t))

(use-package php-mode
  :ensure t
  )

(use-package plantuml-mode
  :ensure t
  :commands plantuml-mode
  :mode
  (("\\.puml$" . plantuml-mode)
   ("\\.plantuml$" . plantuml-mode))
  :custom
  (plantuml-jar-path "~/plantuml/plantuml.jar")
  (plantuml-default-exec-mode 'jar)
  :bind
  (:map plantuml-mode-map
        ("C-c C-p" . hoagie-plantuml-generate-png))
  :config
  (defun hoagie-plantuml-generate-png ()
    (interactive)
    (when (buffer-modified-p)
      (error "There are unsaved changes!!!"))
    (let* ((input (expand-file-name (buffer-file-name)))
           (output (concat (file-name-sans-extension input) ".png"))
           (output-buffer (get-file-buffer output)))
    (call-process "java" nil t nil
                  ;; the jar file...
                  "-jar"
                  (expand-file-name plantuml-jar-path)
                  input
                  "-tpng")
    (if output-buffer
        (with-current-buffer output-buffer
          (revert-buffer-quick)
          (pop-to-buffer output-buffer))
      (find-file-other-window output)))))

(use-package rcirc
  :commands rcirc
  :custom
  (rcirc-server-alist '(("irc.libera.chat"
                         :port 7000
                         :encryption tls
                         :server-alias "libera"
                         :nick "hoagie"
                         :full-name "Sebastián Monía"
                         :user-name "seb.hoagie@outlook.com"
                         :channels ("#emacs" "#emacs-es" "#argentina"))))
  :hook
  (rcirc-mode-hook . (lambda () (rcirc-track-minor-mode 1))))

(use-package terraform-mode
  :ensure t
  :mode "\\.tf$")

(use-package tramp
  :custom
  (tramp-default-method "sshx"))

(use-package web-mode
  :ensure t
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
  :custom
  (web-mode-enable-css-colorization t)
  (web-mode-enable-sql-detection t)
  (web-mode-enable-current-element-highlight t)
  (web-mode-markup-indent-offset 2))

(use-package yaml-mode
  :ensure t
  :mode "\\.yml$")

;; Convenient to work with AWS timestamps
(defun hoagie-convert-timestamp (&optional timestamp)
"Convert a Unix TIMESTAMP (as string) to date.
If the parameter is not provided use word at point."
  (interactive (list (thing-at-point 'word t)))
  (let ((to-convert (if (< 10 (length timestamp))
                        (substring timestamp 0 10)
                      timestamp))
        (millis (if (< 10 (length timestamp))
                  "000")))
    (message "%s.%s"
             (format-time-string "%Y-%m-%d %H:%M:%S"
                                 (seconds-to-time
                                  (string-to-number to-convert)))
             millis)))
(define-key hoagie-keymap (kbd "t") #'hoagie-convert-timestamp)

;; Some day...
(use-package eshell
  :custom
  (eshell-prefer-lisp-functions t)
  (eshell-modules-list
   '(eshell-alias eshell-banner eshell-basic eshell-cmpl eshell-dirs
     eshell-elecslash eshell-extpipe eshell-glob eshell-hist eshell-ls
     eshell-pred eshell-prompt eshell-script eshell-term eshell-tramp
     eshell-unix eshell-xtra))
  :hook
  (eshell-mode-hook . hoagie-eshell-mode-setup)
  :config
  (defun hoagie-eshell-mode-setup ()
    "Configure eshell buffers."
    (toggle-truncate-lines t)))
