;; dormant.el --- Things that I might revisit  -*- lexical-binding: t; -*-

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
;; Includes some things I tried but didn't quite stick. Maybe they come back...

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

(use-package hi-lock
  :demand t
  :custom
  (hi-lock-auto-select-face t)
  :config
  (defvar-keymap hoagie-hi-lock-keymap
    :doc "Keymap for `hi-lock' commands."
    :name "Highlight..."
    "u" '("unhighlight" . unhighlight-regexp)
    "l" '("lines" . highlight-lines-matching-regexp)
    "p" '("phrase" . highlight-phrase)
    "r" '("regexp" . highlight-regexp)
    "." '("symbol at point" . highlight-symbol-at-point))
  :bind-keymap
  ("C-c h" . hoagie-hi-lock-keymap))

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
  (rcirc-server-alist '(("chat.sr.ht"
                         :port 6697
                         :encryption tls
                         :server-alias "sr.ht"
                         :nick "hoagie"
                         :full-name "Sebastián Monía"
                         :user-name "sebasmonia/liberachat"
                         :channels ("#emacs" "#emacs-es" "#argentina"))))
  :hook
  (rcirc-mode-hook . (lambda () (rcirc-track-minor-mode 1))))

(use-package restclient
  :ensure t
  :custom
  (restclient-same-buffer-response nil)
  (restclient-response-body-only nil)
  :mode
  ("\\.http\\'" . restclient-mode)
  :bind
  (:map restclient-mode-map
        ("C-c r" . rename-buffer)
        ("C-c h" . restclient-toggle-headers))
  :config
  (defun restclient-toggle-headers ()
    (interactive)
    (message "restclient-response-body-only=%s"
             (setf restclient-response-body-only
                   (not restclient-response-body-only)))))

(use-package sharper
  :load-path "~/github/sharper"
  :if (locate-library "sharper.el")
  :bind
  (:map hoagie-keymap
        ;; using "n" for another command now, moving
        ;; this to "c" for C#
        ("c" . sharper-main-transient))
  :custom
  (sharper-run-only-one t))

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

;; I used this command extensively for like, 4 days, and never again :shrug:
(defvar-local hoagie-narrow-toggle-markers nil
  "A cons cell (beginning . end) that is updated when using
`hoagie-narrow-toggle'.")
(defun hoagie-narrow-toggle (&optional arg)
  "Toggle widening/narrowing of the current buffer.
If the buffer is narrowed, store the boundaries in
`hoagie-narrow-toggle-markers' and widen.
If the buffer is widened, then narrow to region if
`hoagie-narrow-toggle-markers' is non nil (and then discard those
markers, resetting the state)."
  (interactive)
  (if (buffer-narrowed-p)
      (progn
        (setf hoagie-narrow-toggle-markers (cons (point-min)
                                                 (point-max)))
        (widen))
    ;; check for toggle markers
    (if (not hoagie-narrow-toggle-markers)
        (message "No narrow toggle markers.")
      ;; do the thing
      (narrow-to-region (car hoagie-narrow-toggle-markers)
                        (cdr hoagie-narrow-toggle-markers))
      (setf hoagie-narrow-toggle-markers nil))))
