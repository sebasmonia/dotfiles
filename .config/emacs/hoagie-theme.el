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
       (string "dark green") ;; hoagie
       (alt-fg "dark slate grey")) ;; hoagie

  (custom-theme-set-faces 'hoagie

   ;; Basic faces
   `(default ((t (:foreground ,fg :background ,bg))))
   `(cursor ((t (:background ,alt-fg)))) ;; hoagie
   `(highlight ((t (:background ,dim-1))))
   `(trailing-whitespace ((t (:underline t))))
   `(region ((t (:extend t :background ,dim-2))))
   `(secondary-selection ((t (:inherit region))))
   `(error ((t (:weight bold :foreground "red"))))
   `(warning ((t (:weight bold :foreground "orange"))))
   `(success ((t (:weight bold :foreground "green"))))
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
   `(font-lock-warning-face ((t (:inherit error))))
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

   ;; completions
   `(completions-common-part ((t (:underline t :weight bold)))))
   ;; not in Tok
   `(fixed-pitch ((t (:background "white smoke"))))

   )

;;;###autoload
(when (and (boundp 'custom-theme-load-path) load-file-name)
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'hoagie)
