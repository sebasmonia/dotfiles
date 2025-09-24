;;; hoagie-mode-line.el --- My mode-line -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Sebastián Monía
;;
;; Author: Sebastián Monía <sebastian@sebasmonia.com>
;; URL: https://git.sr.ht/~sebasmonia/dotfiles
;; Keywords: local tools

;; This file is not part of GNU Emacs.

;;; Commentary:

;; My own mode-line.
;; It is based on my mood-line configuration.
;; The package broke a few times after updates, plus it kept growing in size,
;; and I had my own customizations on top of it. So I decided to setup my own
;; mode-line from scratch and stop using it.

;;; Code:

;; TODO: is there a way to get this concatenated without :eval?
(setq-default mode-line-modified
     '(:eval (propertize
              (concat (if buffer-read-only "×"
                        ;; since it is not read-only, show
                        ;; the modified flag
                        (if (and (buffer-modified-p)
                                 (not (derived-mode-p 'comint-mode)))
                            "!"
                          "-"))
                      ;; not really about "modified", but I like this
                      ;; in the first few chars of the mode line
                      (when (buffer-narrowed-p) "N"))
              'face 'mode-line-emphasis)))

(setq-default mode-line-remote
              '(:eval (propertize
                       (concat (when (file-remote-p default-directory) "R")
                               ;; trailing space here
                               " ")
                       'face 'mode-line-emphasis)))

;; same as default, with no tooltip, mouse menu nor face
(setq-default mode-line-buffer-identification '("%12b "))

(setq-default mode-line-position
  (list "%l:%c"
        '(:propertize " %p%  %I" face shadow)
        '(:eval
          (when (use-region-p)
            (propertize (apply #'format
                               " (%sL:%sC)"
                               (hoagie--mode-line-region-size))
                        'face 'shadow))
           " ")))

(defun hoagie--mode-line-region-size ()
  "Helper to calculate the region size to display in the mode line.
Returns a list with the number of lines, and the number of characters.
For the latter, it uses a more involved calculation for rectangle selections."
  (let ((lines (count-lines (region-beginning) (region-end))))
    (list lines
          (if (and (boundp rectangle-mark-mode) rectangle-mark-mode)
              ;; first line or last line, both have the same amount of
              ;; characters.
              (* (length (car (extract-rectangle (region-beginning)
                                                 (region-end))))
                 lines)
            ;; regular region
            (- (region-end) (region-beginning))))))

(setq-default mode-line-modes
     '(:propertize mode-name
                   help-echo (format-mode-line minor-mode-alist)))

(setq-default mode-line-format
     '(" " mode-line-modified mode-line-remote
       " " mode-line-buffer-identification
       " " mode-line-position
       " " (:eval (when overwrite-mode
                    (propertize "OVERWRITE"
                                'face 'mode-line-highlight)))
       " %[%] " ;; Experimental - show recursive edit level
       mode-line-format-right-align
       (:eval (when defining-kbd-macro
                (propertize "REC"
                            'face 'mode-line-highlight)))
       " " (vc-mode vc-mode)
       " " mode-line-misc-info
       " " mode-line-process
       " " mode-line-modes "   "))

;;; hoagie-mode-line.el ends here
