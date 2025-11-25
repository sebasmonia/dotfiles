;;; hoagie-registers.el --- Registers extensions -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Sebastián Monía
;;
;; Author: Sebastián Monía <sebastian@sebasmonia.com>
;; URL: https://git.sr.ht/~sebasmonia/dotfiles
;; Keywords: local text

;; This file is not part of GNU Emacs.

;;; Commentary:
;; I've had this in my config for a while, I just felt like putting in its own
;; file, since I've been doing that for many other things lately.
;;
;; A little navigation system built on top on registers:

;; 1. DWIM command that stores text in a register if there's an active region,
;;    else it stores point, picking the new unused letter for it.
;; 2. Insert and jump commands that filter the register preview to show only
;;    text and position registers, respectively
;; 3. Text register preview that shows the first portion of the text

;;; Code:

;; BRITTLENESS WARNING: this re-defines a built-in method, there's
;; a high risk it breaks when moving Emacs versions
(cl-defmethod register-val-describe ((val marker) _verbose)
  (let ((buf (marker-buffer val)))
    (if (null buf)
	    (princ "a marker in no buffer")
      (princ (hoagie--text-around-marker val))
      (princ "   -- buffer ")
      (princ (buffer-name buf))
      (princ ", pos ")
      (princ (marker-position val)))))
(defun hoagie--text-around-marker (marker-val)
  "Get the line around MARKER.
Some inspiration from the package Consult."
  (with-current-buffer (marker-buffer marker-val)
    (save-excursion
      (without-restriction
        (goto-char marker-val)
        (string-trim (thing-at-point 'line))))))
;; There are a bunch of packages that do this or similar things,
;; but I have an interest on developing my own clunky, bug ridden,
;; and, I can only hope, flexible system
(defun hoagie-push-to-register-dwim ()
  "If the region is active, store it in the next register. Else push point.
See `hoagie-get-next-register' for \"next register\" selection."
  (interactive)
  (if (use-region-p)
      (hoagie-copy-to-next-register)
    (hoagie-point-to-next-register)))
(defvar hoagie-registers-order
  "asdfgqwertzxcvbyuiophjklnm;',./-=`?\"[](){}"
  "The order in which to walk the registers in `hoagie-next-register'")
(defun hoagie-next-register ()
  "Return the next char from `hoagie-registers-order' that is empty.
Silently returns nil if none is available."
  (cl-loop for reg-key across hoagie-registers-order
           unless (get-register reg-key)
           return reg-key))
(defun hoagie-copy-to-next-register ()
  "Copies the region to the register returned by `hoagie-next-register'.
The values passed to `copy-to-register' are based on its
interactive declaration."
  (interactive)
  (let ((register (hoagie-next-register)))
    (copy-to-register register
                      (region-beginning)
                      (region-end)
                      current-prefix-arg
                      t)
    (message "Text to register: %c" register)))
(defun hoagie-point-to-next-register ()
  "Stores point in the register returned by `hoagie-next-register'.
The values passed to `point-to-register' are based on its
interactive declaration."
  (interactive)
  ;; I never want to store frame configurations...but in a future version
  ;; I could do window configurations? My current setup for windows stores
  ;; ONE window config and that seems to be enough, though.
  (let ((register (hoagie-next-register)))
    (point-to-register (hoagie-next-register) nil)
    (message "Point to register: %c" register)))
(defun hoagie-jump-to-register (&optional arg)
  "Almost like `jump-to-register' but filters the alist for better preview.
It also deletes the register if called with prefix ARG."
  (interactive "P")
  (let* ((register-alist (cl-loop for reg in register-alist
                                  when (markerp (cdr reg))
                                  collect reg))
         (reg (register-read-with-preview "Jump to: ")))
    (jump-to-register reg)
    (when arg
      (set-register reg nil))))
(defun hoagie-insert-register (&optional arg)
  "Almost like `insert-register' but filters the alist for better preview.
It also deletes the register when called with prefix ARG."
  (interactive "P")
  (let* ((register-alist (cl-loop for reg in register-alist
                                  when (stringp (cdr reg))
                                  collect reg))
         (reg (register-read-with-preview "Insert text: ")))
    (when (use-region-p)
      ;; when there's an active region, delete it first
      ;; NOTE: maybe I want to consider _killing_ instead of _deleting_?
      (delete-region (region-beginning) (region-end)))
    (insert-register reg)
    (when arg
      (set-register reg nil))))
(defun hoagie-clean-registers (arg)
  "Remove data from a register.
With prefix ARG, delete all registers"
  (interactive "P")
  (if (not arg)
      (while t
        (set-register (register-read-with-preview
                       "Register to clear (quit to exit): ")
                      nil))
    ;; No need to ask, but show a message
    (setf register-alist nil)
    (message "All registers cleared.")))

(provide 'hoagie-registers)
;;; hoagie-registers.el ends here
