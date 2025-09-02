;;; hoagie-editing.el --- Generict text editing commands -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2025 Sebastián Monía
;;
;; Author: Sebastián Monía <sebastian@sebasmonia.com>
;; URL: https://git.sr.ht/~sebasmonia/dotfiles
;; Keywords: local convenience abbrev text

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Generic tools to aid editing, that aren't tied to a single language or major
;; mode. This is consumed from my init file, just trying to unclutter things a
;; bit.

;;; Code:

;;;###autoload
(defmacro with-region-or-thing (thing &rest body)
  "Execute the forms in BODY, binding \"start\" and \"end\" locally.
The variables are bound to either the region limits, or the
limits of the THING at point. This is such a common pattern
in my custom commands, that I turned it into a reusable macro."
  (declare (indent 1) (debug t)) ;; valuable learning here :100:
  `(let (start end)
     (if (use-region-p)
         (setf start (region-beginning)
               end (region-end))
       ;; try to get the limits of THING
       (let ((bounds (bounds-of-thing-at-point ,thing)))
         (if bounds
             (setf start (car bounds)
                   end (cdr bounds))
           (error "Couldn't find the %s at point" ,thing))))
     ,@body))

;; from https://www.emacswiki.org/emacs/BackwardDeleteWord because I
;; agree C-backspace shouldn't kill the word! It litters my kill ring
(defun hoagie-delete-word (arg)
  "Delete characters forward until encountering the end of a word.
With ARG, do this that many times."
  (interactive "*p")
  (if (use-region-p)
      (delete-region (region-beginning) (region-end))
    (delete-region (point) (progn
                             (forward-word arg)
                             (point)))))

(defun hoagie-backward-delete-word (arg)
  "Delete characters backward until encountering the end of a word.
With ARG, do this that many times."
  (interactive "*p")
  (hoagie-delete-word (- arg)))

(defun hoagie-escape-regexp (&optional arg)
  "Escape a regexp in the region or current line.
By default it escapes double quotes and backslashes. Call with prefix
ARG to be prompted a different regexp.
Use double prefix to ignore the first and last match, intended
for escaping a line that contains a string literal."
  (interactive "*P")
  (let ((regexp "\\([\\\"\\\\\\\\]\\)")
        (skip-first-last (equal '(16) arg))
        last-match)
    (when arg
      (setf regexp (read-string "Regexp: " "\\([\\\"\\\\\\\\]\\)")))
    (with-region-or-thing 'line
      (save-excursion
        (deactivate-mark)
        (with-restriction start end
          (goto-char start)
          (when skip-first-last
            ;; skip the first
            (re-search-forward regexp nil t))
          (while (re-search-forward regexp nil t)
            (setf last-match (match-string 1))
            (replace-match "\\\\\\1"))
          (when skip-first-last
            ;; go to the last char replacement and delete the \
            (search-backward last-match nil t)
            (delete-char -1)))))))

(defun hoagie-toggle-backslash ()
  "Toggle slashes-backslashes in the region or line."
  (interactive)
  (save-excursion
    (with-region-or-thing 'line
      (with-restriction start end
        (save-match-data 
          (let* ((from (if (search-forward "/" nil t) ?/ ?\\))
                 (to (if (= from ?\\) ?/ ?\\)))
            (goto-char start)
            (subst-char-in-region start end from to)))))))

(defun hoagie-split-by-sep (&optional arg)
  "Split the rest of the line using a separator.
By default the separator is a single space, and contiguous spaces
are collapsed to one. With prefix ARG prompt for separator, with
no collapsing.
Strings are not broken up. The resulting region is indented after
splitting.

This is based on the super useful package
\"fill-function-arguments\", which works well but sometimes gets
confused on how to split things (usually in the edges of comments
and strings). Also the way the package does fall-through to
`fill-paragraph' is a bit inconsistent. This function is a
fraction of its functionality, and isn't as smart, but it is more
predictable."
  (interactive "*P")
  (save-excursion
    (let ((start (point))
          (sep (if arg
                   (read-string "Separator: ")
                 " ")))
      (while (search-forward sep (pos-eol) t)
        (unless arg
          (just-one-space))
        ;; don't add newline when inside a string
        (unless (nth 3 (syntax-ppss))
          (newline)))
      ;; move to the line _right after_ the last new line character inserted
      (forward-line)
      (indent-region start (point)))))

(defun hoagie-insert-datetime (&optional arg)
  "Insert ISO8601 date at point.
With prefix ARG, don't use dashes (useful for backup files, for
example). With double prefix, include the time."
  (interactive "P")
  (insert (format-time-string (cond ((equal arg '(4)) "%Y%m%d")
                                    ((equal arg '(16)) "%F %T")
                                    (t "%F")))))
(provide 'hoagie-editing)

;;; hoagie-editing.el ends here
