;; Attempt to get all email-related configuration in a single place

(use-package gnus
  :bind
  (:map hoagie-second-keymap
        ;; for "email"
        ("e" . gnus))
  (:map gnus-summary-mode-map
        ("v t" . hoagie-summary-trash)
        ("v a" . hoagie-summary-archive)
        ;; "b" for Basura. Using "s" is risky, as it is
        ;; next to "a"rchive :)
        ("v b" . hoagie-summary-spam)
        ("v g" . hoagie-summary-show-all)
        ;; originally T # - I can use it to mark whole threads
        ;; or individual messages
        ("v #" . gnus-uu-mark-thread)
        ;; originally T n or M-<down> or C-M-f
        ;; instead, get more Thread commands under "v"
        ("v n" . gnus-summary-next-thread))
  :init
  ;; let's do our best to keep Gnus files/dir outside of ~
  ;; some of these are not really Gnus vars, but declared in
  ;; nndraft, message, etc.
  (setq gnus-home-directory "~/.gnus.d/"
        gnus-directory "~/.gnus.d/News/"
        gnus-default-directory "~/.gnus.d"
        nndraft-directory "~/.gnus.d/News/drafts/"
        nnmh-directory "~/.gnus.d/News/drafts/"
        nnfolder-directory "~/.gnus.d/Mail/archive"
        nnml-directory "~/.gnus.d/Mail/"
        message-directory "~/.gnus.d/Mail/"
        gnus-article-save-directory "~/.gnus.d/News/"
        gnus-read-newsrc-file nil
        gnus-save-newsrc-file nil
        ;; the two values above mean I don't need this.
        ;; But, just in case:
        gnus-startup-file "~/.gnus.d/.newsrc"
        gnus-dribble-directory "~/.gnus.d/"
        gnus-use-dribble-file nil
        gnus-always-read-dribble-file nil)
  :custom
  ;; the first four are not really Gnus values, but this is a sensible place
  ;; to set them
  (user-full-name "Sebastián Monía")
  (user-mail-address "sebastian@sebasmonia.com")
  (mml-secure-openpgp-signers '("A65927B22A60F72A53D77CD7EF7DAC84163D7A83"))
  (mml-secure-openpgp-encrypt-to-self t)
  ;; -----
  (gnus-select-method '(nnnil ""))
  (gnus-secondary-select-methods '((nnimap "fastmail"
                                           (nnimap-address "imap.fastmail.com")
                                           (nnimap-server-port 993)
                                           (nnimap-stream ssl)
                                           (nnir-search-engine imap))))
  ;; Archive outgoing email in Sent folder, mark it as read
  (gnus-message-archive-method '(nnimap "imap.fastmail.com"))
  (gnus-message-archive-group "nnimap+fastmail:Sent")
  (gnus-gcc-mark-as-read t)
  (gnus-search-use-parsed-queries t)
  (gnus-auto-select-next nil)
  (gnus-paging-select-next nil)
  (gnus-summary-stop-at-end-of-message t)
  (gnus-mime-display-multipart-related-as-mixed t)
  (gnus-auto-select-first nil)
  (gnus-summary-display-arrow nil)
  ;; http://groups.google.com/group/gnu.emacs.gnus/browse_thread/thread/a673a74356e7141f
  (gnus-summary-line-format (concat
                             "%U%R | "
                             "%&user-date; │ " ;; date
                             "%-25,25f |" ;; name
                             "%B "
                             "%s\n"))
  (gnus-user-date-format-alist '((t . "%Y-%m-%d (%a) %H:%M")
                                 gnus-thread-sort-functions '(gnus-thread-sort-by-date)
                                 ))
  (gnus-user-date-format-alist '((t . "%Y-%m-%d %I:%M%p")))
  (gnus-thread-sort-functions '(gnus-thread-sort-by-date))
  (gnus-show-threads t)
  (gnus-sum-thread-tree-false-root nil) ;; use subject
  (gnus-sum-thread-tree-root nil)
  (gnus-sum-thread-tree-indent " ")
  (gnus-sum-thread-tree-vertical        "│")
  (gnus-sum-thread-tree-leaf-with-other "├─► ")
  (gnus-sum-thread-tree-single-leaf     "╰─► ")
  :config
  (defun hoagie-summary-spam ()
    "Move the current or marked mails to Spam in Fastmail."
    (interactive)
    (gnus-summary-move-article nil "nnimap+fastmail:Spam"))
  (defun hoagie-summary-archive ()
    "Move the current or marked mails to the Archive in Fastmail."
    (interactive)
    (gnus-summary-move-article nil "nnimap+fastmail:Archive"))
  (defun hoagie-summary-trash ()
    "Move the current or marked mails to Trash in Fastmail."
    (interactive)
    (gnus-summary-move-article nil "nnimap+fastmail:Trash"))
  (defun hoagie-summary-show-all ()
    "Show all messages, even the ones read.
This is C-u M-g but I figured I would put it in a simpler binding."
    (interactive)
    (gnus-summary-rescan-group t)))

(use-package message
  :bind
  (:map message-mode-map
        ;; shadows `message-elide-region' :shrug:
        ("C-c C-e" . hoagie-confirm-encrypt))
  :hook
  (message-setup-hook . hoagie-message-change-from)
  :custom
  ;; actually part of simple.el, but putting it here because
  ;; it is relevant to message.el behaviour for C-x m
  (mail-user-agent 'gnus-user-agent)
  ;; integrate with ecomplete. Press TAB to get email completion
  (message-mail-alias-type 'ecomplete)
  (message-self-insert-commands nil)
  (message-expand-name-standard-ui t)
  ;; This causes problems when Cc: is already present.
  ;; Need to either add a func to add a header, or internalize the
  ;; existing commands to "go to header" which add them
  ;; (message-default-mail-headers "Cc: \nBcc: \n")
  :config
  ;; From https://www.emacswiki.org/emacs/GnusTutorial#h5o-40
  (defvar hoagie-email-addresses '("sebastian@sebasmonia.com"
                                   "capsule@sebasmonia.com"
                                   "code@sebasmonia.com"
                                   "mailing@sebasmonia.com"
                                   "subscriptions@sebasmonia.com"
                                   "thingstopay@sebasmonia.com"
                                   "work@sebasmonia.com")
    "The list of aliases in my email setup.")
  (defun hoagie-message-change-from ()
    "Select the \"From:\" address when composing a new email."
    (interactive)
    (let* ((selected-address (completing-read "From: " hoagie-email-addresses))
           (address (concat user-full-name " <" selected-address ">"))
           (from-text (concat "From: " address)))
      (setq gnus-article-current-point (point))
      (goto-char (point-min))
      (while (re-search-forward "^From:.*$" nil t)
        (replace-match from-text))
      (goto-char gnus-article-current-point)))
  (defun hoagie-confirm-encrypt ()
    "Answer y/n to whether to send the message encrypted."
    (interactive)
    (when (y-or-n-p "Encrypt message?")
      (mml-secure-message-encrypt))))

;; Send email via Fastmail's SMTP:
(use-package smtpmail
  :custom
  (send-mail-function 'smtpmail-send-it)
  (smtpmail-default-smtp-server "smtp.fastmail.com")
  (smtpmail-stream-type 'ssl)
  (smtpmail-smtp-service 465))
