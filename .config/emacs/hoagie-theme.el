(deftheme hoagie
  "My personal theme, just a customized Tok.
Only light variant.
https://github.com/topikettunen/tok-theme/:
Minimal monochromatic theme for Emacs in the spirit of Zmacs and
Smalltalk-80.")

(let* ((bg    "white")
       (fg    "black")
       (dim-1  "grey90")
       (dim-2  "grey80")
       (dim-3  "grey70")
       (dim-4  "grey60")
       (dim-5  "grey50")
       ;; hoagie
       (string "green4");; "dark green")
       (alt-fg "dark slate grey")
       (alt-bg "lavender")
       )

  (custom-theme-set-faces 'hoagie
  ;; for live testing
  ;; (custom-set-faces

   ;; Basic faces
   `(default ((t (:foreground ,fg :background ,bg))))
   `(cursor ((t (:background ,alt-fg)))) ;; hoagie
   `(hl-line ((t (:background ,alt-bg)))) ;; hoagie
   `(highlight ((t (:background ,dim-1))))
   `(trailing-whitespace ((t (:underline t))))
   `(region ((t (:extend t :background ,dim-2))))
   `(secondary-selection ((t (:inherit region))))
   ;; next three where bold in the original theme
   `(error ((t (:foreground "red"))))
   `(warning ((t (:foreground "orange"))))
   `(success ((t (:foreground ,string))))
   `(fringe ((t (nil))))
   `(button ((t (:box 1))))
   `(vertical-border ((t (:foreground ,dim-2))))
   `(minibuffer-prompt ((t (nil))))
   `(link ((t (:underline t))))
   `(link-visited ((t (:foreground ,alt-fg :inherit link)))) ;; hoagie

   ;; Line-numbes
   `(line-number ((t (:foreground ,dim-2))))
   `(line-number-current-line ((t (:foreground ,fg :background ,dim-1))))

   ;; Mode-line
   `(mode-line ((t (:foreground ,fg :background ,bg :box (:color ,fg)))))
   `(mode-line-active ((t (:inherit mode-line))))
   `(mode-line-inactive ((t (:weight light :foreground ,dim-5 :background ,bg :box (:color ,dim-1)))))
   `(mode-line-highlight ((t (nil))))
   `(mode-line-emphasis ((t (:weight bold))))
   `(mode-line-buffer-id ((t (:weight bold))))

   ;; Font-lock
   `(font-lock-comment-face ((t (:foreground ,dim-4)))) ;; hoagie
   `(font-lock-comment-delimiter-face ((t (:inherit font-lock-comment-face))))
   `(font-lock-string-face ((t (:foreground ,string))))
   `(font-lock-doc-face ((t (:foreground ,dim-5))))
   `(font-lock-doc-markup-face ((t (nil))))
   `(font-lock-keyword-face ((t (nil))))
   `(font-lock-builtin-face ((t (nil))))
   `(font-lock-function-name-face ((t (nil))))
   `(font-lock-variable-name-face ((t (nil))))
   `(font-lock-type-face ((t (nil))))
   `(font-lock-constant-face ((t (nil))))
   `(font-lock-warning-face ((t (:inherit warning)))) ;; used to inherit from error
   `(font-lock-negation-char-face ((t (nil))))
   `(font-lock-preprocessor-face ((t (:weight bold))))
   `(font-lock-regexp-grouping-backslash ((t (nil))))
   `(font-lock-regexp-grouping-construct ((t (nil))))

   ;; isearch
   `(isearch ((t (:foreground ,bg :background ,fg))))
   `(isearch-group-1 ((t (:background ,dim-5))))
   `(isearch-group-2 ((t (:background ,dim-4))))
   `(lazy-highlight ((t (:background ,dim-1))))

   ;; Dired
   `(dired-directory ((t (:weight bold))))
   `(dired-broken-symlink ((t (:inherit error))))

   ;; sh
   `(sh-heredoc ((t (nil))))
   `(sh-quoted-exec ((t (nil))))

   ;; Org
   `(org-agenda-structure ((t (nil))))
   `(org-block ((t (nil))))
   `(org-headline-done ((t (nil))))
   `(org-special-keyword ((t (:foreground ,dim-5))))

   ;; Terraform
   `(terraform--resource-name-face ((t (nil))))
   `(terraform--resource-type-face ((t (nil))))

   ;; Markdown
   `(markdown-header-face ((t (:inherit outline-1))))
   `(markdown-header-delimiter-face ((t (nil))))
   `(markdown-metadata-key-face ((t (:inherit font-lock-comment-face))))
   `(markdown-metadata-value-face ((t (:inherit font-lock-comment-face))))
   `(markdown-blockquote-face ((t (nil))))
   `(markdown-pre-face ((t (nil))))

   ;; not in Tok
   `(fixed-pitch ((t (:background ,dim-1))))
   `(completions-common-part ((t (:underline t :weight bold))))
   `(log-view-commit-body ((t (:foreground ,alt-fg))))

   ;; gnus
   `(gnus-group-mail-3-empty ((t (:inherit default)))) ;; email directories
   `(gnus-summary-normal-ticked ((t (:foreground ,string)))) ;; marked !
   `(gnus-summary-normal-read ((t (:inherit default)))) ;; read
   `(gnus-summary-normal-ancient ((t (:inherit default)))) ;; read
   `(gnus-summary-normal-unread ((t (:weight bold)))) ;; unread
   `(gnus-button ((t (:underline t))))
   `(gnus-header-name ((t (:inherit default)))) ;; all "Header:" text
   `(gnus-header-from ((t (:inherit default :underline t)))) ;; text of "From: "
   `(gnus-header-subject ((t (:inherit default)))) ;; text of "Subject: "
   `(gnus-header-content ((t (:inherit default)))) ;; other header text/values

   ;; maybe I do need some more "fg" colors...
   `(gnus-cite-1 ((t (:foreground ,alt-fg))))
   `(gnus-cite-2 ((t (:foreground ,string))))
   `(gnus-cite-3 ((t (:foreground "orange"))))
   `(gnus-cite-4 ((t (:foreground ,fg :slate t))))
   `(gnus-cite-5 ((t (:foreground ,alt-fg :slate t))))
   `(gnus-cite-6 ((t (:foreground ,string :slate t))))

   ;; message
   `(message-header-name ((t (:inherit default))))
   `(message-header-to ((t (:weight bold))))
   `(message-header-subject ((t (:inherit default))))
   `(message-header-other ((t (:inherit default))))
   `(message-header-cc ((t (:inherit default))))
   `(message-separator ((t (:inherit highlight))))
   `(message-signature-separator ((t (:inherit highlight))))
   ))

;;;###autoload
(when (and (boundp 'custom-theme-load-path) load-file-name)
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'hoagie)
