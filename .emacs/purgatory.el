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

(use-package docker
  :ensure t
  :bind
  ("C-c d" . docker))

(use-package dockerfile-mode
  :ensure t
  :mode "Dockerfile\\'")

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

(use-package restclient
  :ensure t
  :custom
  (restclient-same-buffer-response t)
  (restclient-response-body-only nil)
  :mode
  ("\\.http\\'" . restclient-mode)
  :bind
  ("<f4>" . hoagie-open-restclient)
  (:map restclient-mode-map
        ("C-c r" . rename-buffer)
        ("C-c h" . restclient-toggle-headers))
  :hook
  (restclient-mode-hook . hoagie-restclient-imenu-index)
  :config
  (defun restclient-toggle-headers ()
    (interactive)
    (message "restclient-response-body-only=%s"
             (setf restclient-response-body-only
                   (not restclient-response-body-only))))
  (defun hoagie-open-restclient (arg)
    "Open a file from the restclient \"collection\"."
    (interactive "P")
    (let ((restclient-file (read-file-name "Open restclient file:"
                                           "~/restclient/"
                                           nil
                                           nil
                                           nil
                                           (lambda (name)
                                             (string-equal
                                              (file-name-extension name)
                                              "http")))))
      (if arg
          (find-file-other-window restclient-file)
        (find-file restclient-file))))
  (defun hoagie-restclient-imenu-index ()
    "Configure imenu on the convention \"### Title ###\"."
    (setq-local imenu-generic-expression '((nil "^### \\(.*\\) ###$" 1)))))

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

(use-package shell
  :config
  (defun hoagie-shell-mode-setup ()
    "Setup my `shell-mode' configuration."
    (toggle-truncate-lines t)
    (setf comint-process-echoes t))
  :hook
  (shell-mode-hook . hoagie-shell-mode-setup))

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
