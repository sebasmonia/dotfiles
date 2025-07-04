;;; hoagie-notes.el --- Commands for note taking  -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2025 Sebastián Monía
;;
;; Author: Sebastián Monía <sebastian@sebasmonia.com>
;; URL: https://git.sr.ht/~sebasmonia/dotfiles
;; Keywords: local convenience text

;; This file is not part of GNU Emacs.

;;; Commentary:

;; My note taking "system":
;; 1. I prefer Markdown over org. It is more ubiquitous.
;; 2. I tried org-mode and howm. Both have great things going for them, but
;;    they are also complex and have way more features than I neeed.
;; 3. I am keeping what I used from howm: grepping notes, date in filename.
;; 4. I want to sync this more with the Emacs diary/calendar, eventually. Howm
;;    and org each have their own systems for agenda and notifications, and I
;;    never quite got them. The Emacs diary is simple.

;;; Code:

(defvar hoagie-notes-inbox "~/notes/inbox.md"
  "Location of the notes \"inbox\" file.
This file is meant to be used for quick, uncategorized notes.")

(defvar hoagie-notes-directory "~/notes"
  "Directory for all notes.")

;; (defvar hoagie-notes-diary-file "~/.emacs.d/diary-notes"
;;   "Location of the diary file to export note-related tasks.")

(defvar-keymap hoagie-notes-keymap
  :doc "Convenience keymap for note-taking commands."
  :name "Note actions"
  "g" '("grep" . hoagie-notes-grep)
  "f" '("find" . hoagie-notes-find-by-name)
  "n" '("new" . hoagie-notes-new-note)
  "i" '("inbox" . hoagie-notes-open-inbox))

(defun hoagie-notes-open-inbox ()
  "Open notes \"inbox\" file.
Meant to take quick, uncategorized notes."
  (interactive)
  (find-file hoagie-notes-inbox)
  (goto-char (point-max)))

(defun hoagie-notes-new-note (filename-title)
  "Create a new note, with FILENAME-TITLE.
The value of FILENAME-TITLE is used as-is for the title inside the note.
It is lowercased and with dashes replacing spaces in the filename."
  (interactive "sTitle (filename): ")
  (let* ((starting-text (concat "# " filename-title "\n"
                                (format-time-string "%Y-%m-%d") "\n\n"
                                "tags: #tags-here" "\n\n"))
         (note-file-name (file-name-concat
                          hoagie-notes-directory
                          (format-time-string "%Y%m")
                          (downcase (concat
                                     (string-replace " " "-" filename-title)
                                  ".md")))))
    (make-directory (file-name-directory note-file-name) t)
    (find-file note-file-name)
    (insert starting-text)))

(defun hoagie-notes-grep (regexp)
  "Search for REGEXP in the notes using grep."
  (interactive (list
                (read-string "Regexp: "
                             (when (use-region-p)
                               (buffer-substring-no-properties
                                (region-beginning) (region-end))))))
  (rgrep regexp "*" hoagie-notes-directory))

(defun hoagie-notes-find-by-name ()
  "Open a note, by filename, with completion."
  (interactive)
  (require 'project)
  ;; Want to reuse project.el's find file with completion, I like it :) for
  ;; that, bind `project-prompter' to a function that returns the notes
  ;; directory as if it were the current project, and change the default
  ;; directory so that it works even if I call the command from within another
  ;; project
  (let ((default-directory hoagie-notes-directory)
        (project-prompter (lambda () hoagie-notes-directory)))
    (project-find-file-in nil (list hoagie-notes-directory)
                          (project-current))))

(provide 'hoagie-notes)
;;; hoagie-notes.el ends here
