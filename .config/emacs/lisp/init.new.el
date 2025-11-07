;;; init.el --- My dot emacs file  -*- lexical-binding: t; -*-

;; Author: Sebastián Monía <code@sebasmonia.com>
;; URL: https://git.sr.ht/~sebasmonia/dotfiles
;; Version: 30.6
;; Keywords: local tools processes vc files

;; This file is not part of GNU Emacs.

;;; Commentary:

;; My dot Emacs file
;; Recent changes:
;; 2025-02-24: Update for Emacs 30 (new options, remove things now in core)
;; 2025-02-26: Bring back browse-kill-ring, remove json-mode, dired-narrow,
;;             re-builder. Remove instances of global-set-key.
;; 2025-03-10: Change F2, and add repeat-mode.
;; 2025-05-14: Clean up and standarize bindings: replace F1 with C-c g, and F2
;;             with C-c n. Use more <remap> when applicable.
;; 2025-11-06: Remove use-package. Load all packages on start up. And move all
;;             binding changes to their own file.
;;; Code:


(setf custom-file (locate-user-emacs-file "custom.el"))

(when (eq window-system 'pgtk)
  (pgtk-use-im-context t))

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(setf package-archive-priorities '(("gnu" . 10) ("nongnu" . 5) ("melpa" . 1))
      package-native-compile t)

(when (display-graphic-p)
  (custom-set-faces '(default ((t (:family "Berkeley Mono"
                                           :height 120
                                           :foundry "outline"))))))

(add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp/"))
(add-to-list 'load-path (expand-file-name "~/sourcehut/cambalache"))
(add-to-list 'load-path (expand-file-name "~/sourcehut/cdsync"))



(require 'hoagie-editing) ;; Custom commands for general text editing
;; TODO: part of misc.el, consider moving it.
(setopt duplicate-line-final-position -1)

(require 'hoagie-notes)

(require 'hoagie-pages)


(require 'ansi-color)
(defun ansi-color-apply-buffer ()
  "Colorize the entire buffer using `ansi-color-apply-on-region'."
  (interactive)
  (ansi-color-apply-on-region (point-min) (point-max)))


(require 'appt)
;; TODO: activate after diary
(appt-activate)
;; inspired by https://gitlab.com/jabranham/emacs/blob/master/init.el
(setopt appt-delete-window-function (lambda () t))
(setopt appt-disp-window-function #'hoagie-appt-notify)
(setopt appt-audible nil)
(setopt appt-display-diary nil)
(setopt appt-display-interval 5)
(setopt appt-message-warning-time 15)
;; override the built in function, as there is not way to customize this
(defun appt-mode-line (min-to-app &optional _abbrev)
  "Return an appointment string for the mode-line. Hoagie version.
MIN-TO-APP is a list of minutes, as strings. _ABBREV is ignored, always
abbreviate text."
  (let ((smaller-min (car (sort min-to-app #'<))))
    (format "Appt(s): %s"
            (if (equal smaller-min "0") "NOW"
              (format "%sm" smaller-min)))))
(defun hoagie-appt-notify (minutes-until _current-time appointment-text)
  "Show appointment reminders in the desktop.
See the documentation of appt.el for details on MINUTES-UNTIL,
_CURRENT-TIME and APPOINTMENT-TEXT."
  ;; args can be lists if multiple appointments are due at the same time
  (let ((notification-body (if (listp minutes-until)
                               (format "Multiple appts in %s minutes!!!"
                                       (car minutes-until))
                             (format "In %s minutes: %s"
                                     minutes-until
                                     appointment-text))))
    (notifications-notify :title "Emacs - Appointment"
                          :body notification-body
                          :app-name "Emacs"
                          ;; never expire
                          :timeout 0
                          ;; use 'low to add a notification without toast
                          :urgency 'normal)))


(require 'auth-source)
;; change order so TRAMP defaults to saving passwords in the encrypted gpg file
(setopt auth-sources '("~/.authinfo.gpg" "~/.authinfo" "~/.netrc"))


(require 'bookmark)


(require 'browse-kill-ring)
;; I can use built in M-y, which offers completion. But for longer text, it
;; isn't nearly as comfortable to use as this package
(browse-kill-ring-default-keybindings)


(require 'browse-url)

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

(setopt browse-url-secondary-browser-function (if (eq system-type 'gnu/linux)
                                                  #'browse-url-vivaldi-flatpak
                                                #'browse-url-firefox))
(setopt browse-url-browser-function #'eww-browse-url)
(setopt browse-url-new-window-flag t)


(require 'calendar)
(setopt calendar-date-style 'iso)
(setopt calendar-view-diary-initially-flag nil)
(setopt calendar-latitude 40.7)
(setopt calendar-longitude -73.9)
(setopt calendar-location-name "New York, NY")
(setopt calendar-setup 'one-frame)
;; show events for the next 3 days when the calendar opens
(setopt diary-number-of-entries 3)

(add-hook 'calendar-today-visible-hook #'calendar-mark-today)
(add-hook 'calendar-mode-hook #'diary-mark-entries)


(require 'cambalache t)
(setopt cambalache-root-url "https://myfiles.fastmail.com/")


(require 'comint)
(setopt comint-prompt-read-only t)
(setopt comint-scroll-to-bottom-on-input 'this)


(require 'dabbrev)
(setopt dabbrev-case-distinction nil)
(setopt dabbrev-case-fold-search t)
(setopt dabbrev-case-replace nil)


(require 'diary-lib)
(setopt diary-display-function 'diary-fancy-display)
(add-hook 'diary-mark-entries-hook #'diary-mark-included-diary-files)
(add-hook 'diary-list-entries-hook #'diary-include-other-diary-files)
(add-hook 'diary-list-entries-hook #'diary-sort-entries)


(require 'dictionary)
;; unless I run a dict server locally - which I am tempted to, but
;; it is definitely overkill
(setopt dictionary-server "dict.org")
(setopt dictionary-port 2628)


(require 'cdsync t)
(setopt cdsync-auth-source-host "caldav-fastmail")
(cdsync-setup-calendar-integration)


(require 'dired)
(setopt dired-vc-rename-file t)
(setopt dired-listing-switches "-labogGhvD")
(setopt dired-compress-directory-default-suffix ".7z")
(setopt dired-compress-file-default-suffix ".7z")
(setopt dired-do-revert-buffer t)
(setopt dired-movement-style 'cycle)
(add-hook 'dired-mode-hook #'dired-hide-details-mode)

(setf dired-compress-file-suffixes
      '(("\\.tar\\.gz\\'" #1="" "7z x -aoa -o%o %i")
        ("\\.tgz\\'" #1# "7z x -aoa -o%o %i")
        ("\\.zip\\'" #1# "7z x -aoa -o%o %i")
        ("\\.7z\\'" #1# "7z x -aoa -o%o %i")
        ("\\.tar\\'" ".tgz" nil)
        (":" ".tar.gz" "tar -cf- %i | gzip -c9 > %o"))
      dired-compress-files-alist
      '(("\\.7z\\'" . "7z a -r %o %i")
        ("\\.zip\\'" . "7z a -r %o  %i")))


(require 'ediff)
;; from https://emacs.stackexchange.com/a/9411/17066
(setopt ediff-forward-word-function 'forward-char)
(setopt ediff-highlight-all-diffs t)
(setopt ediff-keep-variants nil)
(setopt ediff-window-setup-function 'ediff-setup-windows-plain)

;; from https://stackoverflow.com/a/29757750
(defun ediff-copy-both-to-C ()
  "In ediff, copy A and then B to C."
  (interactive)
  (ediff-copy-diff ediff-current-difference nil 'C nil
                   (concat
                    (ediff-get-region-contents ediff-current-difference
                                               'A
                                               ediff-control-buffer)
                    (ediff-get-region-contents ediff-current-difference
                                               'B
                                               ediff-control-buffer))))
(defun add-d-to-ediff-mode-map ()
  "Add key 'd' for 'copy both to C' functionality in ediff."
  (keymap-set ediff-mode-map "d" #'ediff-copy-both-to-C))

;; Welp, don't like using internals but, the regular hook doesn't quite work
;; the window config is restored but then _stuff happens_, so:
(add-hook ediff-after-quit-hook-internal #'hoagie-ediff-restore-windows)
;; One minor annoyance of using ediff with built-in vc was
;; the window config being altered, so:
(defvar hoagie-pre-ediff-windows
  nil
  "Window configuration before starting ediff.")
(defun hoagie-ediff-store-windows ()
  "Store the pre-ediff window setup"
  (setf hoagie-pre-ediff-windows (current-window-configuration)))
(defun hoagie-ediff-restore-windows ()
  "Use `hoagie-pre-ediff-windows' to restore the window setup."
  (set-window-configuration hoagie-pre-ediff-windows))

(add-hook 'ediff-keymap-setup-hook #'add-d-to-ediff-mode-map)
(add-hook 'ediff-before-setup-hook #'hoagie-ediff-store-windows)
