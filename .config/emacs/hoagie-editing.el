;;; hoagie-editing.el --- Commands for basic editing  -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Sebastián Monía
;;
;; Author: Sebastián Monía <code@sebasmonia.com>
;; URL: https://git.sr.ht/~sebasmonia/dotfiles
;; Package-Requires: ((emacs "29.1"))
;; Version: 1.0
;; Keywords: convenience

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
       ;; try to get the limits THING
       (let ((bounds (bounds-of-thing-at-point ,thing)))
         (if bounds
             (setf start (car bounds)
                   end (cdr bounds))
           (error "Couldn't find the %s at point." ,thing))))
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

(defun hoagie-region-to-char (arg char &optional interactive)
  "A copy of `zap-to-char' that activates or expands the region."
  (interactive (list (prefix-numeric-value current-prefix-arg)
		             (read-char "Mark to char: " nil 'read-char-history)
                     t))
  (let ((direction (if (>= arg 0) 1 -1))
        (case-fold-search (if (and interactive (char-uppercase-p char))
                              nil
                            case-fold-search)))
    (unless (region-active-p)
      (set-mark-command nil))
    (goto-char
	 (progn
	   (forward-char direction)
	   (unwind-protect
		   (search-forward (char-to-string char) nil nil arg)
		 (backward-char direction))
	   (point)))))

(defun hoagie-escape-regexp (&optional arg)
  "Escape a regexp in the region or current line.
Call with prefix ARG to be prompted a different regexp.
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

(defvar hoagie-pair-chars
  '((?\" . ?\")
    (?\' . ?\')
    (?\` . ?\')
    ;; Silly me! I forgot to add Markdown pairs here :)
    (?\* . ?\*)
    (?\_ . ?\_)
    (?\~ . ?\~)
    (?\( . ?\))
    (?\[ . ?\])
    (?\{ . ?\})
    (?\<  ?\>))
  "Alist of pairs to insert for `hoagie-insert-pair'.")

(defun hoagie-insert-pair (&optional arg)
  "Wrap the region or sexp at point in a pair from `hoagie-pair-chars'.
Note that using sexp at point might wrap a symbol, depending on
point position.
Point is not modified, unless called with prefix ARG: a negative
prefix moves point to the opening delimiter; any other value to
the last one.

This is my own counterpart to `delete-pair' (which see). Emacs
has a built in mode for this, `electric-pair-mode', but it does
more than I want, it is more intrusive, and I couldn't get around
some of it's behaviours. There's also `insert-pair', but I
couldn't crack how to use it :("
  (interactive "*P")
  (with-region-or-thing 'sexp
    (let* ((opener (read-char "Opening char: "))
           (closer (alist-get opener hoagie-pair-chars)))
      ;; if the opener isn't from our list of chars, message and do nothing
      (if (not closer)
          (message "\"%c\" is not in the pair list" opener)
        (save-excursion
          (goto-char start)
          (insert opener)
          (goto-char (+ 1 end))
          (insert closer))
        (cond ((eq arg '-) (goto-char start))
              (arg (goto-char (+ 1 end))))))))

(defun hoagie-toggle-backslash ()
  "Toggle slashes-backslashes in the region or line."
  (interactive)
  (with-region-or-thing 'line
    (save-excursion
      (with-restriction start end
        (goto-char start)
        (if (save-excursion (search-forward "/" nil t))
            (while (search-forward "/" nil t) (replace-match "\\\\" 'literal))
          (while (search-forward "\\" nil t) (replace-match "/")))))))

(defun hoagie-split-by-newline (&optional arg)
  "Split using newlines by a separator, then indent.
By default the separator is a single space, and contiguous spaces
are collapsed to one. With prefix ARG prompt for separator, with
no collapsing.

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
With prefix ARG, include the time."
  (interactive "P")
  (insert (format-time-string (if arg
                                  "%F %T"
                                  "%F"))))

(provide 'hoagie-editing)
;;; hoagie-editing.el ends here
