;;; init.el --- My dot emacs file  -*- lexical-binding: t; -*-

;; Author: Sebastián Monía <sebastian@sebasmonia.com>
;; URL: https://git.sr.ht/~sebasmonia/dotfiles
;; Version: 30.6
;; Keywords: local tools processes vc files

;; This file is not part of GNU Emacs.

;;; Commentary:

;; My init file. Recent changes:
;; 2025-02-26: Bring back browse-kill-ring, remove json-mode, dired-narrow,
;;             re-builder. Remove instances of global-set-key.
;; 2025-03-10: Change F2, and add repeat-mode.
;; 2025-05-14: Clean up and standarize bindings: replace F1 with C-c g, and F2
;;             with C-c n. Use more <remap> when applicable.
;; 2025-11-06: Remove use-package. Load all packages on start up. And move all
;;             binding changes to their own file.
;;; Code:

(setopt custom-file (locate-user-emacs-file "custom.el"))

(when (eq window-system 'pgtk)
  (pgtk-use-im-context t))

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(setopt package-archive-priorities '(("gnu" . 10) ("nongnu" . 5) ("melpa" . 1))
        package-native-compile t)

(setopt package-selected-packages
        '(browse-kill-ring  ;; melpa
          csv-mode          ;; gnu
          debbugs           ;; gnu
          markdown-mode     ;; nongnu
          sly               ;; nongnu
          sly-quicklisp     ;; melpa
          vundo             ;; gnu
          ws-butler))       ;; nongnu

(add-to-list 'load-path (expand-file-name "~/sourcehut/dotfiles/.config/emacs/lisp"))
(add-to-list 'load-path (expand-file-name "~/sourcehut/cdsync"))
(add-to-list 'load-path (expand-file-name "~/github/datum"))
(add-to-list 'load-path (expand-file-name "~/sourcehut/stubvex"))

(when (display-graphic-p)
  (custom-set-faces
   '(default ((t (:family "Berkeley Mono" :height 120 :foundry "outline"))))))

(require 'hoagie-editing) ;; Custom commands for general text editing

(require 'hoagie-notes)

(require 'hoagie-pages)

(require 'hoagie-registers)

(require 'ansi-color)

(defun ansi-color-apply-buffer ()
  "Colorize the entire buffer using `ansi-color-apply-on-region'."
  (interactive)
  (ansi-color-apply-on-region (point-min) (point-max)))

(require 'appt)
(require 'notifications) ;; used by appt to display desktop notifications
;; override the built in function, as there is not way to customize this
(defun appt-mode-line (min-to-app &optional _abbrev)
  "Return an appointment string for the mode-line. Hoagie version.
MIN-TO-APP is a list of minutes, as strings. _ABBREV is ignored, always
abbreviate text."
  (let ((smaller-min (car (sort min-to-app #'<))))
    (format "Appt(s): %s" (if (equal smaller-min "0")
                              "NOW"
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
(appt-activate)
;; inspired by https://gitlab.com/jabranham/emacs/blob/master/init.el
(setopt appt-delete-window-function (lambda () t)
        appt-disp-window-function #'hoagie-appt-notify
        appt-audible nil
        appt-display-diary nil
        appt-display-interval 5
        appt-message-warning-time 15)

(require 'auth-source)
;; change order so TRAMP defaults to saving passwords in the encrypted gpg file
(setopt auth-sources '("~/.authinfo.gpg" "~/.authinfo" "~/.netrc"))

(require 'bookmark)

(require 'browse-kill-ring)
;; I can use built in M-y, which offers completion. But for longer text, it
;; isn't nearly as comfortable to use as this package
(browse-kill-ring-default-keybindings)

(require 'browse-url)
(setopt browse-url-secondary-browser-function #'browse-url-firefox
        browse-url-browser-function #'eww-browse-url
        browse-url-new-window-flag t)

(require 'calendar)
(setopt calendar-date-style 'iso
        calendar-view-diary-initially-flag t
        calendar-latitude 40.7
        calendar-longitude -73.9
        calendar-location-name "New York, NY"
        calendar-setup 'one-frame)
(add-hook 'calendar-today-visible-hook #'calendar-mark-today)
(add-hook 'calendar-mode-hook #'diary-mark-entries)

(require 'comint)
(setopt comint-prompt-read-only t
        comint-scroll-to-bottom-on-input 'this)

(require 'dabbrev)
(setopt dabbrev-case-distinction nil
        dabbrev-case-fold-search t
        dabbrev-case-replace nil)

(require 'diary-lib)
(setopt diary-display-function 'diary-fancy-display
        ;; show events for the next 3 days when the calendar opens
        diary-number-of-entries 3)
(add-hook 'diary-mark-entries-hook #'diary-mark-included-diary-files)
(add-hook 'diary-list-entries-hook #'diary-include-other-diary-files)
(add-hook 'diary-list-entries-hook #'diary-sort-entries)

(require 'dictionary)
;; unless I run a dict server locally - which I am tempted to, but
;; it is definitely overkill
(setopt dictionary-server "dict.org"
        dictionary-port 2628)

(require 'dired)
(require 'dired-aux) ;; for dired-compress-*
(setopt dired-vc-rename-file t
        dired-listing-switches "-labogGhvD"
        dired-compress-directory-default-suffix ".7z"
        dired-compress-file-default-suffix ".7z"
        dired-do-revert-buffer t
        dired-movement-style 'cycle
        dired-compress-file-suffixes ;; not a custom
        '(("\\.tar\\.gz\\'" #1="" "7z x -aoa -o%o %i")
          ("\\.tgz\\'" #1# "7z x -aoa -o%o %i")
          ("\\.zip\\'" #1# "7z x -aoa -o%o %i")
          ("\\.7z\\'" #1# "7z x -aoa -o%o %i")
          ("\\.tar\\'" ".tgz" nil)
          (":" ".tar.gz" "tar -cf- %i | gzip -c9 > %o"))
        dired-compress-files-alist ;; not a custom
        '(("\\.7z\\'" . "7z a -r %o %i")
          ("\\.zip\\'" . "7z a -r %o  %i")))
(add-hook 'dired-mode-hook #'dired-hide-details-mode)

(require 'ediff)
;; from https://emacs.stackexchange.com/a/9411/17066
(setopt ediff-forward-word-function 'forward-char
        ediff-highlight-all-diffs t
        ediff-keep-variants nil
        ediff-window-setup-function 'ediff-setup-windows-plain)

(defun ediff-copy-both-to-C ()
  "In ediff, copy A and then B to C.
From https://stackoverflow.com/a/29757750."
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

(add-hook 'ediff-keymap-setup-hook #'add-d-to-ediff-mode-map)

;; One minor annoyance of using ediff with built-in vc was
;; the window config being altered, so:
(defvar hoagie-pre-ediff-windows nil
  "Window configuration before starting ediff.")
(defun hoagie-ediff-store-windows ()
  "Store the pre-ediff window setup"
  (setf hoagie-pre-ediff-windows (current-window-configuration)))
(defun hoagie-ediff-restore-windows ()
  "Use `hoagie-pre-ediff-windows' to restore the window setup."
  (set-window-configuration hoagie-pre-ediff-windows))

(add-hook 'ediff-before-setup-hook #'hoagie-ediff-store-windows)
;; Welp, don't like using internals but, the regular hook doesn't quite work
;; the window config is restored but then _stuff happens_, so:
(add-hook 'ediff-after-quit-hook-internal #'hoagie-ediff-restore-windows)

(require 'eglot)
(setopt eglot-events-buffer-config '(:size 0 :format full)
        eglot-ignored-server-capabilities '(:codeLensProvider
                                            :documentHighlightProvider
                                            :documentFormattingProvider
                                            :documentRangeFormattingProvider
                                            :documentOnTypeFormattingProvider
                                            :foldingRangeProvider))
;; from https://dawranliou.com/blog/xref-with-eglot-and-project/
(defun xref-find-references-with-eglot (orig-fun &rest args)
  "An advice function that gives `xref-find-definitions' a unique
buffer name when eglot is enabled."
  (if (bound-and-true-p eglot--managed-mode)
      (let ((xref-buffer-name (format "*xref %s*"
                                      (symbol-at-point))))
        (apply orig-fun args))
    (apply orig-fun args)))

(advice-add 'xref-find-references :around #'xref-find-references-with-eglot)

(require 'eldoc)
(setopt eldoc-echo-area-use-multiline-p nil)
(global-eldoc-mode)

(defun hoagie-toggle-eldoc-buffer ()
  "Toggle the eldoc help buffer."
  (interactive)
  (let ((eldoc-window (get-buffer-window eldoc--doc-buffer)))
    (if eldoc-window (quit-window nil eldoc-window) (eldoc-doc-buffer t))))

(require 'elisp-mode)
(defun hoagie-elisp-mode-setup ()
  "Setup my `emacs-lisp-mode' configuration.
Sets `fill-column'."
  (setf fill-column 79)
  (display-fill-column-indicator-mode))
(add-hook 'emacs-lisp-mode-hook #'hoagie-elisp-mode-setup)

(require 'epg-config)
(setopt epg-pinentry-mode 'loopback)

(require 'eww)

(defun hoagie-eww-rename-buffer ()
  "Rename EWW buffers like \"title\", but put title last.
Function based on the same from the docstring for `eww-auto-rename-buffer'."
  (when (eq major-mode 'eww-mode)
    (when-let* ((string (or (plist-get eww-data :title)
                            (plist-get eww-data :url))))
      (format "*eww: %s*" string))))

(defun hoagie--collect-shr-links ()
  "Get an alist of all link targets in the current buffer.
The format returned is (link-text . link-position)
Inspired by a similar function in Elpher."
  (save-excursion
    (goto-char (point-min))
    (cl-loop for link = (text-property-search-forward 'shr-url nil nil t)
             while link
             for position = (prop-match-beginning link)
             for text = (buffer-substring position
                                          (prop-match-end link))
             collect
             (cons text position))))

(defun hoagie-eww-jump ()
  "Similar `elpher-jump', but for EWW.
It is based on the elpher code, but instead of opening the link,
it moves point to it, to take advantage of links'
`eww-follow-link' binding (using prefix or double prefix for
external browser and new eww buffer, respectively)."
  (interactive)
  (let ((all-links (hoagie--collect-shr-links)))
    (goto-char (alist-get (completing-read "Link: " all-links nil)
                          all-links nil nil #'string=))))

(setopt eww-auto-rename-buffer #'hoagie-eww-rename-buffer
        eww-download-directory  "~/eww-downloads/"
        eww-bookmarks-directory "~/sourcehut/dotfiles/.config/emacs/")
(add-hook 'eww-mode-hook #'toggle-word-wrap)
(add-hook 'eww-mode-hook #'visual-line-mode)

(require 'flymake)

(require 'grep)
(setopt grep-command
        "grep --color=always -nHi -r --include=*.* -e \"pattern\" ."
        grep-use-null-device nil)

(require 'hl-line)
;; EXPERIMENTAL - use setopt instead of after-init-hook
(setopt global-hl-line-mode t)

(require 'mhtml-mode)
(defun hoagie-mhtml-mode-setup ()
  "Setup my `mhtml-mode' configuration.
Set `fill-column' and related modes.'."
  (setf fill-column 100)
  (display-fill-column-indicator-mode)
  (auto-fill-mode)
  (setq-local page-delimiter " *<h[[:digit:]]"))
(add-hook 'mhtml-mode-hook #'hoagie-mhtml-mode-setup)

(require 'info)
(add-to-list 'Info-directory-list (expand-file-name "~/.local/share/info/"))

(require 'isearch)
(setopt search-exit-option 'edit
        isearch-lazy-count t
        isearch-lazy-highlight 'all-windows
        isearch-wrap-pause 'no
        isearch-repeat-on-direction-change t
        lazy-highlight-initial-delay 0.1
        regexp-search-ring-max 64
        search-ring-max 64)

(require 'lisp-mode)
(defun hoagie-lisp-mode-setup ()
  "Setup my `lisp-mode' configuration.
Set `fill-column' and setup indent to CL style."
  (set (make-local-variable lisp-indent-function)
	   'common-lisp-indent-function)
  (setf fill-column 100)
  (display-fill-column-indicator-mode))
(add-hook 'lisp-mode-hook #'hoagie-lisp-mode-setup)

(require 'markdown-mode)
(setopt markdown-command "pandoc")
(defun hoagie-markdown-mode-setup ()
  "Setup my `markdown-mode' configuration.
Set `fill-column', `truncate-lines'."
  (setf fill-column 100
        truncate-lines t)
  (display-fill-column-indicator-mode)
  (auto-fill-mode)
  (setq-local page-delimiter "^#\\{1,6\\} "))
(add-hook 'markdown-mode-hook #'hoagie-markdown-mode-setup)

(require 'minibuffer)
(setopt completions-format 'one-column
        completions-max-height 25
        completion-styles '(flex)
        read-buffer-completion-ignore-case t
        read-file-name-completion-ignore-case t
        completion-ignore-case t
        minibuffer-visible-completions t
        minibuffer-message-timeout 0.5
        completions-detailed t
        completion-auto-help 'always
        always-auto-select 'second-tab
        enable-recursive-minibuffers t)

(require 'paren)
(setopt show-paren-when-point-inside-paren t)

(require 'proced)
;; TODO: revisit configuration options
;; (setopt proced-auto-update-flag 'visible ;; default is 5 seconds
;;         proced-show-remote-processes t
;;         proced-filter 'all)
;; TODO: worth it? then I need to define faces =P
;; (proced-enable-color-flag t)

(require 'project)
(setopt project-vc-extra-root-markers '(".emacs-project")
        project-switch-commands '((project-find-file "Find file" nil)
                                  (project-find-regexp "Find regexp" nil)
                                  (project-dired "Dired" ?d)
                                  (project-vc-dir "VC-Dir" nil)
                                  (project-shell "Shell" nil)
                                  (project-eshell "Eshell" nil)))
;; from https://dawranliou.com/blog/xref-with-eglot-and-project/
(defun project-find-regexp-with-unique-buffer (orig-fun &rest args)
  "An advice function that gives project-find-regexp a unique buffer name"
  (require 'xref)
  (let ((xref-buffer-name (format "*xref %s*" (car args))))
    (apply orig-fun args)))
(advice-add 'project-find-regexp
            :around #'project-find-regexp-with-unique-buffer)

(require 'python)
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
(setopt python-shell-font-lock-enable nil
        python-shell-interpreter "ipython"
        python-shell-interpreter-args "--pprint --simple-prompt"
        python-check-command "python3 -m pylint")

(defun hoagie-python-mode-setup ()
  "Setup my `python-mode' configuration."
  (setf fill-column 100) ;; I prefer 79, but at work we use 100 :)
  ;; (And I rarely code Python at home)
  (display-fill-column-indicator-mode)
  (setq-local page-delimiter "^\\(def\\|class\\) "))

(add-hook 'python-mode-hook #'hoagie-python-mode-setup)

(require 're-builder)
(setq reb-re-syntax 'string)

(require 'register)
(setopt register-preview-delay 0.1)

(require 'repeat)
(setopt repeat-exit-key "RET")
(repeat-mode)

(require 'replace)

(defun hoagie-occur-symbol-or-region ()
  "Run occur for the symbol at point, or the active region.
By default, occur _limits the search to the region_ if it is active."
  (interactive)
  (with-region-or-thing 'symbol
    (occur (regexp-quote (buffer-substring-no-properties start end))
           (when current-prefix-arg
	         (prefix-numeric-value current-prefix-arg)))))

(defun hoagie-rename-and-select-occur-buffer ()
  "Renames the current buffer to *Occur: [term] [buffer]*.
Meant to be added to `occur-hook'."
  (cl-destructuring-bind (search-term _ (buffer-name &rest other-bufs))
      occur-revert-arguments
    (pop-to-buffer
     (rename-buffer (format "*Occur: %s %s*" search-term buffer-name) t))))

(add-hook 'occur-hook #'hoagie-rename-and-select-occur-buffer)

(require 'savehist)
(setopt history-delete-duplicates t
        savehist-additional-variables '(kill-ring search-ring
                                        regexp-search-ring eww-history))
(savehist-mode)

(require 'shell)
(setopt shell-get-old-input-include-continuation-lines t)

(defun hoagie-shell-mode-setup ()
  "Configure shell buffers."
  (toggle-truncate-lines t)
  (setq comint-process-echoes t))
(add-hook 'shell-mode-hook #'hoagie-shell-mode-setup)

(require 'shr)
(setopt shr-use-colors t
        shr-bullet "• "
        shr-discard-aria-hidden t
        shr-max-image-proportion 0.5)
;; not a public setting
(setf shr-indentation 2)

(defun hoagie-shr-link-open-or-kill (&optional arg)
  "Edit and open the link at point.
With prefig ARG, put it in the kill ring instead."
  (interactive "P")
  (if-let* ((target-url (get-text-property (point) 'shr-url)))
      (progn
        (message "Killed link: %s" target-url)
        (if arg
            (kill-new target-url)
          (eww (read-string "" target-url))))
    (error "No shr link under point")))

(require 'sly)
(require 'sly-quicklisp)
(setopt inferior-lisp-program "sbcl --dynamic-space-size 10240")

(require 'sql)
(setopt sql-display-sqli-buffer-function t)

(defun hoagie-sql-setup ()
  "Configure sql-mode."
  (setf truncate-lines t
        fill-column 100)
  (auto-fill-mode)
  (display-fill-column-indicator-mode))

(defun hoagie-sql-interactive-setup ()
  "Configure SQLi."
  (setf truncate-lines t)
  (setq-local page-delimiter "^>"))

(add-hook 'sql-interactive-mode-hook #'hoagie-sql-interactive-setup)
(add-hook 'sql-mode-hook #'hoagie-sql-setup)

(require 'sql-datum)
(setopt sql-datum-program "datum")

(require 'tramp)
;; This humble declaration triggered the move away from use-package.
;; I want the /sudo:user@computer:/ syntax available since startup,
;; and even :demand-ing the package sometimes didn't cut it.

(require 'time)
(setopt world-clock-time-format "%r - %F (%A)"
        world-clock-list '(("America/Denver" "Denver")
                           ("America/Buenos_Aires" "Buenos Aires")
                           ("Europe/Madrid" "España")
                           ("America/Mexico_City" "CDMX")
                           ("America/Chicago" "Central")
                           ("America/New_York" "Eastern")))

(require 'timer)
;; inspired by this code
;; https://www.reddit.com/r/emacs/comments/7wsnoi/comment/du3h11l/
(defun hoagie-notify-timer (message when)
  "Show MESSAGE at TIME, in a desktop notification.
Time can be anything accepted by `run-at-time'."
  (interactive
   "sMessage (default: \"Timer Elapsed\"): \nsSeconds (or \"#min\"): ")
  (when (string-empty-p message)
    (setf message "Timer elapsed"))
  (run-at-time when nil #'notifications-notify
               :title "Emacs - Timer" :body message
               :app-name "Emacs" :timeout 0 :urgency 'normal))

;; All vc-related packages
(require 'vc)
(require 'vc-git)
(require 'vc-dir)
(require 'vc-hooks)
(require 'stubvex)
(setopt vc-allow-rewriting-published-history t
        ;; vc-git-revision-complete-only-branches t
        ;; vc-git-shortlog-switches 'TBD
        vc-git-log-switches '("--date=iso-local" "--stat")
        vc-display-status 'no-backend
        ;; from https://emacs.stackexchange.com/a/37855, which links
        ;; to TRAMP's manual
        vc-ignore-dir-regexp (format "\\(%s\\)\\|\\(%s\\)"
                                     vc-ignore-dir-regexp
                                     tramp-file-name-regexp))

(defvar hoagie-vc-git-emails '("sebastian@sebasmonia.com"
                               "some.work@email.com :)")
  "List of email addresses that can be associated with a repository")

(defun hoagie-vc-git-clone (repository-url local-dir email)
  "Run \"git clone REPOSITORY-URL\" to LOCAL-DIR.
After cloning, EMAIL is set in the repo.
Interactively, reads the email from `hoagie-vc-git-emails', and then
calls `vc-dir' in the newly cloned directory."
  (interactive
   (let* ((url (read-string "Repository URL: "))
          (dir (read-directory-name "Target directory: " nil nil nil
                                    (file-name-base url)))
          (mail (completing-read "Email for this repo: "
                                 hoagie-vc-git-emails)))
     (list url dir mail)))
  (vc-git-clone repository-url (expand-file-name local-dir) nil)
  (let ((default-directory local-dir))
    (vc-git-command nil 0 nil "config" "user.email" email))
  (when (called-interactively-p 'any)
    (vc-dir local-dir)))

(require 'vundo)

(require 'window)

;; Simplified version of the idea at
;; https://erick.navarro.io/blog/save-and-restore-window-configuration-in-emacs/
(defvar hoagie-window-configuration nil
  "Window configuration saved manually, or before deleting other windows.")
(defun hoagie-store-window-configuration (&optional silent)
  (interactive "P")
  (setf hoagie-window-configuration (current-window-configuration))
  (unless silent
    (message "Stored current window configuration")))
(defun hoagie-restore-window-configuration ()
  "Use `hoagie-window-configuration' to restore the window setup."
  (interactive)
  (when hoagie-window-configuration
    (set-window-configuration hoagie-window-configuration)
    (setf hoagie-window-configuration nil)))
(defun hoagie-delete-other-windows ()
  "Custom `delete-other-windows' command.
It that stores the current setup in `hoagie-window-configuration'."
  (interactive)
  (hoagie-store-window-configuration t)
  (delete-other-windows))
(defun hoagie-toggle-frame-split ()
  "Toggle orientation, just like ediff's |.
See https://www.emacswiki.org/emacs/ToggleWindowSplit for sources, this
version is my own spin of the first two in the page."
  (interactive)
  (unless (= (count-windows) 2)
    (error "Can only toggle a frame split in two"))
  (let ((was-split-vertically (window-combined-p))
        (other-buffer (window-buffer (next-window))))
    (delete-other-windows) ; closes the other window
    (if was-split-vertically
        (split-window-horizontally)
      (split-window-vertically))
    (set-window-buffer (next-window) other-buffer)))

(require 'ws-butler)
(add-hook 'prog-mode-hook #'ws-butler-mode)

;; Generic commands, not tied to a package or feature
(defun hoagie-go-home (arg)
  "Open ~. Prefix ARG to open in other window."
  (interactive "P")
  (let ((home-dir (expand-file-name "~/")))
    (if arg
        (dired-other-window home-dir)
      (dired home-dir))))
(defun hoagie-open-init ()
  "Open my init file.
Instead of using `user-init-file', it goes to the \"dotfiles\"
repository."
  (interactive)
  (find-file
   (expand-file-name "~/sourcehut/dotfiles/.config/emacs/init.el")))
;; Inspired by
;; https://demonastery.org/2013/04/emacs-narrow-to-region-indirect/ and
;; modified to DWIM. Also use `pop-to-buffer' instead of `switch-to-buffer'
(defun hoagie-narrow-indirect-dwim (&optional arg)
  "Create an indirect buffer, narrow it to defun or active region.
If ARG, don't prompt for buffer name suffix."
  (interactive "P")
  (with-region-or-thing 'defun
    (let* ((new-name (unless arg
                       (format "%s<%s>"
                               (buffer-name)
                               (read-string "<suffix>: "))))
           ;; we'll pop the buffer manually to clear the region
           (buf (clone-indirect-buffer new-name nil)))
      (with-current-buffer buf
        (narrow-to-region start end)
        (deactivate-mark))
      (pop-to-buffer buf))))

(defun hoagie-split-window-right (&optional arg)
  "Like `split-window-right', but select the new window.
With prefix ARG, use `split-root-window-right' instead"
  (interactive "P")
  (select-window (if arg
                     (split-root-window-right)
                   (split-window-right))))

(defun hoagie-split-window-below (&optional arg)
  "Like `split-window-below', but select the new window.
With prefix ARG, use `split-root-window-below' instead"
  (interactive "P")
  (select-window (if arg
                     (split-root-window-below)
                   (split-window-below))))

(defun hoagie-kill-buffer-source ()
  "Sends the current buffer's \"source\" to the kill ring.
The source means: buffer-filename, URL (eww), dired filename, [more to come]."
  (interactive)
  (let ((source (cond ((derived-mode-p 'dired-mode)
                       (dired-copy-filename-as-kill 0))
                      ((derived-mode-p 'eww-mode) (eww-current-url))
                      (t (buffer-file-name)))))
    (if source
        (progn
          (kill-new source)
          (message "Killed source: %s" source))
      (error "No source for this buffer"))))

;; Customization options tied to a package or feature
(setopt
 create-lockfiles nil
 remote-file-name-inhibit-locks t  ;; from TRAMP's FAQ
 sentence-end-double-space nil
 tab-always-indent 'complete
 read-minibuffer-restore-windows nil
 tab-width 4
 ;; This feature was the culprit of the messages "No matching parenthesis
 ;; found" in the minibuffer, an annoyance in `zap-up-to-char' - and probably
 ;; other commands. With `show-paren-mode', it feels superfluous.
 blink-matching-paren nil
 recenter-positions '(1 middle -2)  ;; behaviour for C-l
 read-file-name-completion-ignore-case t  ;; useful in Linux

 help-window-select t  ;; via https://github.com/jacmoe/emacs.d/blob/master/jacmoe.org
 custom-safe-themes t
 indent-tabs-mode nil
 ;; from https://karthinks.com/software/batteries-included-with-emacs/
 ;; use view-mode for all read only files automatically
 view-read-only t
 delete-by-moving-to-trash t
 inhibit-startup-screen t
 initial-buffer-choice t
 save-interprogram-paste-before-kill t
 visible-bell nil
  ;; from https://gitlab.com/jessieh/dot-emacs
 backup-by-copying t   ; Don't delink hardlinks
 version-control t     ; Use version numbers on backups
 delete-old-versions t ; Do not keep old backups
 kept-new-versions 5   ; Keep 5 new versions
 kept-old-versions 5   ; Keep 3 old versions
 ;; from https://depp.brause.cc/dotemacs/
 echo-keystrokes 0.25
 use-short-answers t
 ;; this works for compile but also occur, grep etc
 next-error-message-highlight t
 duplicate-line-final-position -1)

;; TODO: Load from file?
(setopt initial-scratch-message
        (with-temp-buffer
          (insert-file-contents "scratch-message.txt")
          (buffer-string)))

;; see https://emacs.stackexchange.com/a/28746/17066 and
;; https://blog.danielgempesaw.com/post/129841682030/fixing-a-laggy-compilation-buffer
(setf disabled-command-function nil
      w32-use-native-image-API t
      inhibit-compacting-font-caches t
      auto-window-vscroll nil
      compilation-error-regexp-alist (delete 'maven compilation-error-regexp-alist))

(setq-default bidi-inhibit-bpa t)

;; Enable/disable some modes
(delete-selection-mode)
(blink-cursor-mode -1)
(column-number-mode 1)
(horizontal-scroll-bar-mode -1)
(global-so-long-mode 1)

;; Backup and autosaves
;; From https://stackoverflow.com/a/22176971, move auto saves and
;; back up files to a different folder so git or dotnet core won't
;; pick them up as changes or new files in the project
(let ((auto-save-dir (expand-file-name (concat
                                        user-emacs-directory
                                        "auto-save/")))
      (backup-dir (expand-file-name (concat
                                     user-emacs-directory
                                     "backups/"))))
  (make-directory auto-save-dir t)
  (make-directory backup-dir t)
  (setf auto-save-list-file-prefix auto-save-dir
        auto-save-file-name-transforms
        `((".*" ,auto-save-dir t)))
  (make-directory backup-dir t)
  (setf backup-directory-alistn
        `((".*" . ,backup-dir))))


;; Load theme, modeline and bindings, after all other packages are require'd
(load-file "~/sourcehut/dotfiles/.config/emacs/lisp/mode-line.el")
(load-file "~/sourcehut/dotfiles/.config/emacs/lisp/bindings.el")
(load-file "~/sourcehut/dotfiles/.config/emacs/lisp/hoagie-theme.el")
(load-theme 'hoagie t)

;; Load local configuration
(load-file "~/sourcehut/dotfiles/.config/emacs/lisp/machine-config.el")
