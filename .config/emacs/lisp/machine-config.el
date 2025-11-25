;;; machine-config.el --- Local overloads -*- lexical-binding: t; -*-
;; Author: Sebastián Monía <sebastian@sebasmonia.com>
;; URL: https://git.sr.ht/~sebasmonia/dotfiles
;; Version: 1.0
;; Keywords: local

;; This file is not part of GNU Emacs.

;;; Commentary:

;; I am supposed to run
;;     git update-index --assume-unchanged machine-config.el
;; in every computer I cloned my config.
;;
;; I drop here things that I need to setup per-machine, as the name implies. I
;; used to do this kind of thing by OS (Windows = work, Linux = home), but
;; nowadays I also use Linux at work for some tasks.
;; I was going to use environment variables, but I suspect this can be simpler.
;;; Code:

(set-fontset-font t 'emoji (font-spec :family "Noto Emoji"))

(defun hoagie-adjust-font-size (frame)
  "Inspired by https://emacs.stackexchange.com/a/44930/17066.
FRAME is ignored."
  ;; 2021-05-22: now I use the pgtk branch everywhere, and the monitor name
  ;; has a meaningful value in all cases, so:
  (let* ((monitor-name (alist-get 'name (frame-monitor-attributes)))
         (monitor-font '(("0x0536" . 143) ;; laptop -- maybe 151?
                         ("LG Ultra HD" . 181))) ;; 173?
         (size (alist-get monitor-name monitor-font
                          180 ;; default size, "big just in case"
                          nil
                          'equal)))
    (set-face-attribute 'default (selected-frame) :height size)))
(add-hook 'window-size-change-functions #'hoagie-adjust-font-size)

(defun browse-url-vivaldi-flatpak (url &optional new-window)
  "Duplicate of `browse-url-firefox' that uses Flatpak and Vivaldi.
For details on URL, check the original function. NEW-WINDOW is ignored."
  (interactive (browse-url-interactive-arg "URL: "))
  (setq url (browse-url-encode-url url))
  (let* ((process-environment (browse-url-process-environment)))
    (apply #'start-process
           (concat "vivaldi " url) nil
           "flatpak-spawn"
           (list "--host" "flatpak" "run" "com.vivaldi.Vivaldi" url))))

(setopt browse-url-secondary-browser-function #'browse-url-vivaldi-flatpak)

(require 'cdsync)
(setopt cdsync-auth-source-host "caldav-fastmail")
(cdsync-setup-calendar-integration)

;;; machine-config.el ends here
