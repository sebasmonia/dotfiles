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
  (interactive "p")
  (if (use-region-p)
      (delete-region (region-beginning) (region-end))
    (delete-region (point) (progn
                             (forward-word arg)
                             (point)))))

(defun hoagie-backward-delete-word (arg)
  "Delete characters backward until encountering the end of a word.
With ARG, do this that many times."
  (interactive "p")
  (delete-word (- arg)))

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

(defun hoagie-escape-quotes ()
    "Escape quotes in the region.
If there's no active region, it operates on the current line. It
simply adds a \\ to each \" found."
    (interactive)
    (with-region-or-thing 'line
      (save-excursion
        (with-restriction start end
          (goto-char start)
          (while (search-forward "\"" nil t)
            (replace-match "\\\\\""))))))

(defvar hoagie-pair-chars
  '((?\" . ?\")
    (?\' . ?\')
    (?\` . ?\')
    (?\( . ?\))
    (?\[ . ?\])
    (?\{ . ?\}))
  "Alist of pairs to insert for `hoagie-insert-pair'.")

(defun hoagie-insert-pair ()
  "Wrap the region or symbol at point in a pair from `hoagie-pair-chars'.
This is my own counterpart to `delete-pair' (which see). Emacs
has a built in mode for this, `electric-pair-mode', but it does
more than I want, it is more intrusive, and I couldn't get around
some of it's behaviours.
Note that using sexp at point might wrap a symbol, depending on
point position.
UPDATE: also look at `insert-pair', which seems to be a more
complete solution to this."
  (interactive)
  (with-region-or-thing 'sexp
    (let* ((opener (read-char "Opening char: "))
           (closer (alist-get opener hoagie-pair-chars)))
      ;; if the opener isn't from our list of chars, message and do nothing
      (if (not closer)
          (message "\"%c\" is not in the pair list" opener)
        (goto-char start)
        (insert opener)
        (goto-char (+ 1 end))
        (insert closer)))))

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

(provide 'hoagie-editing)
;;; hoagie-editing.el ends here
