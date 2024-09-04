;; These are based on my mood-line configuration. The package broke a few times
;; after updates, plus it kept growing in size, and I had my own customizations
;; on top of it.
;; So I decided to setup my own mode-line from scratch.
(defun hoagie-mode-line-buffer-status ()
  "Return a mode-line indicator for buffer status.
It also shows whether the buffer is narrowed or remote."
  (propertize (concat (if buffer-read-only "/"
                        ;; since it's not read-only, show the modified flag
                        (if (and (buffer-modified-p)
                                 (not (derived-mode-p 'comint-mode)))
                            "!"
                          "-"))
                      (if (buffer-narrowed-p) "N" " ")
                      (if (file-remote-p default-directory) "R" " ")
                      " ")
              'face 'error))

(defun hoagie-mode-line-cursor-position ()
  "Mode line cursor position, includes percent indicator.
If the region is active, include its size in lines and characters."
    (let ((region-size (when (use-region-p)
                         (propertize (format " (%sL:%sC)"
                                             (count-lines (region-beginning)
                                                          (region-end))
                                             (- (region-end) (region-beginning)))
                                     'face 'shadow)))
          ;; experimental, show buffer size in human readable format
          (position (propertize " %p%  %I" 'face 'shadow)))
      (list "%l:%c" position region-size)))

(defun hoagie-mode-line-keybmacro-indicator ()
  "Show an indicator when macro recording is in progress.
Because I like \"REC\" better than \"Def\"."
  (when defining-kbd-macro
    (propertize "REC"
                'face 'warning
                'help-echo "Recording macro" )))

(defun hoagie-mode-line-buffer-name ()
  "Return a mode-line indicator for buffer name.
Shows file name in the echo area/a tooltip. From
http://emacs-fu.blogspot.com/2011/08/customizing-mode-line.html"
  (propertize "%b" 'face 'mode-line-buffer-id 'help-echo (buffer-file-name)))

(defun hoagie-mode-line-major-mode ()
  "Mode-line major mode segment.
Show minor modes in the echo area/a tooltip."
  (propertize (format-mode-line mode-name)
              'face 'mode-line-buffer-id
              'help-echo (format-mode-line minor-mode-alist)))

(defun hoagie-mode-line-overwrite-indicator ()
  "Show me when I fat fingered Insert.
I am not sure about this one, but I can remove if I need to."
  (when overwrite-mode
    (propertize "OVERWRITE"
                'face 'warning)))

(setq-default
 mode-line-format
 '(" "
   (:eval (hoagie-mode-line-buffer-status))
   "  "
   (:eval (hoagie-mode-line-buffer-name))
   "  "
   (:eval (hoagie-mode-line-cursor-position))
   "  "
   (:eval (hoagie-mode-line-overwrite-indicator))
   mode-line-format-right-align
   (:eval (hoagie-mode-line-keybmacro-indicator))
   " "
   (vc-mode vc-mode)
   " "
   mode-line-misc-info
   "  "
   mode-line-process
   "  "
   (:eval (hoagie-mode-line-major-mode))
   "   "))
   ;; (project-mode-line project-mode-line-format)
   ;; " "))
