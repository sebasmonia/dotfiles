;; These are based on my mood-line configuration. The package broke a few times
;; after updates, plus it kept growing in size, and I had my own customizations
;; on top of it.
;; So I decided to setup my own mode-line from scratch.

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
                      (when (buffer-narrowed-p) "N")
                      (when (file-remote-p default-directory) "R")
                      " ")
              'face 'mode-line-emphasis)))

;; same as default, with no tooltip, mouse menu nor face
(setq-default mode-line-buffer-identification '("%12b"))

(setq-default mode-line-position
     (list "%l:%c"
           '(:propertize " %p%  %I" face shadow)
           '(:eval
             (when (use-region-p)
               (propertize (format " (%sL:%sC)"
                                   (count-lines (region-beginning)
                                                (region-end))
                                   (- (region-end) (region-beginning)))
                           'face 'shadow)))
           " "))

(setq-default mode-line-modes
     '(:propertize mode-name
                   help-echo (format-mode-line minor-mode-alist)))

(setq-default mode-line-format
     '(" " mode-line-modified
       "  " mode-line-buffer-identification
       "  " mode-line-position
       "  " (:eval (when overwrite-mode
                     (propertize "OVERWRITE"
                                 'face 'mode-line-highlight)))
       mode-line-format-right-align
       (:eval (when defining-kbd-macro
                (propertize "REC"
                            'face 'mode-line-highlight)))
       " " (vc-mode vc-mode)
       " " mode-line-misc-info
       "  " mode-line-process
       "  " mode-line-modes "   "))
