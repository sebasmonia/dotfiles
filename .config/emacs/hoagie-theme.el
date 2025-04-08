(deftheme hoagie
  "My personal theme, using Tok as a starting point.
See https://github.com/topikettunen/tok-theme
This is a variant that has some more colors, but as few as possible. And
tries to minimize the use of bolds and slate/italics too."
  :background-mode 'light
  :kind 'color-scheme)

(let* ((bg    "#ffffff")
       (fg    "#000000")
       (dim-0 "#f2f2f2") ;; grey95
       (dim-1 "#e5e5e5") ;; grey90
       (dim-2 "#cccccc") ;; grey80
       (dim-3 "#b3b3b3") ;; grey70
       (dim-4 "#999999") ;; grey60
       (dim-5 "#7f7f7f") ;; grey50
       ;; Additional colors
       (string "#006400") ;; dark green
       (alt-fg "#68228b") ;; dark orchid 4
       (alt-bg "#e6e6fa") ;; lavender
       (error "#8b0000") ;; dark red
       (warning "#ff8c00") ;; dark orange
       (bg-mode-line (if (display-graphic-p)
                         "#ffffff"
                       "#f2f2f2")))

  (custom-theme-set-faces 'hoagie
  ;; for live testing
  ;; (custom-set-faces

   ;; Basic faces
   `(default ((t (:foreground ,fg :background ,bg))))
   `(cursor ((t (:background ,alt-fg))))
   `(hl-line ((t (:background ,alt-bg))))
   `(highlight ((t (:background ,dim-1))))
   `(trailing-whitespace ((t (:underline t))))
   `(region ((t (:extend t :background ,dim-2))))
   `(secondary-selection ((t (:inherit region))))
   `(fixed-pitch ((t (:background ,dim-0))))
   `(shadow ((t (:foreground ,dim-2))))
   ;; next three where bold in the original theme
   `(error ((t (:foreground ,error))))
   `(warning ((t (:foreground ,warning))))
   `(success ((t (:foreground ,string))))
   `(fringe ((t (nil))))
   `(button ((t (:box t :background ,dim-0))))
   `(vertical-border ((t (:foreground ,dim-2))))
   `(link ((t (:underline t))))
   `(link-visited ((t (:underline t :foreground ,alt-fg))))

   `(minibuffer-prompt ((t (nil))))
   `(completions-common-part ((t (:underline t :weight bold))))

   `(log-view-commit-body ((t (:foreground ,alt-fg))))
   `(diary ((t (:foreground ,alt-fg))))

   ;; Mode-line
   `(mode-line ((t (:foreground ,fg :background ,bg-mode-line))))
   `(mode-line-active ((t (:inherit mode-line :box (:color ,fg)))))
   `(mode-line-inactive ((t (:foreground ,dim-3 :box (:color ,dim-1)))))
   `(mode-line-highlight ((t (:foreground ,error))))
   `(mode-line-emphasis ((t (:weight bold))))

   ;; Font-lock
   `(font-lock-comment-face ((t (:foreground ,dim-4))))
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
   `(font-lock-warning-face ((t (:inherit warning))))
   `(font-lock-negation-char-face ((t (nil))))
   `(font-lock-preprocessor-face ((t (:weight bold))))
   `(font-lock-regexp-grouping-backslash ((t (nil))))
   `(font-lock-regexp-grouping-construct ((t (nil))))

   ;; isearch
   `(isearch ((t (:foreground ,bg :background ,fg))))
   `(isearch-group-1 ((t (:background ,dim-2))))
   `(isearch-group-2 ((t (:background ,dim-3))))
   `(lazy-highlight ((t (:background ,dim-1))))
   `(match ((t (:background ,dim-2))))

   ;; Dired
   `(dired-directory ((t (:foreground ,string))))
   `(dired-broken-symlink ((t (:inherit error))))
   `(dired-marked ((t (:background ,dim-1))))

   ;; Markdown
   `(markdown-header-face ((t :inherit font-lock-function-name-face)))
   `(markdown-header-delimiter-face ((t (nil))))
   `(markdown-metadata-key-face ((t (:inherit font-lock-comment-face))))
   `(markdown-metadata-value-face ((t (:inherit font-lock-comment-face))))
   `(markdown-blockquote-face ((t (nil))))
   `(markdown-pre-face ((t (nil))))
   `(markdown-code-face ((t (:foreground ,string))))
   `(markdown-markup-face ((t (:inherit ,font-lock-comment-face))))

   ;; message, gnus (only email)
   `(gnus-group-mail-3-empty ((t (:inherit default)))) ;; email directories
   ;; email directories with unread items: don't use bold, directories with no
   ;; new items are hidden by default anyway
   `(gnus-group-mail-3 ((t (:inherit default))))
   `(gnus-summary-normal-ticked ((t (:foreground ,string)))) ;; marked !
   `(gnus-summary-normal-read ((t (:inherit default)))) 
   `(gnus-summary-normal-ancient ((t (:inherit default)))) ;; read too
   `(gnus-summary-cancelled ((t (:foreground ,dim-3)))) ;; deleted
   ;; experiment: no more bold for unread, use the mark on the left
   ;; `(gnus-summary-normal-unread ((t (:weight bold))))
   `(gnus-summary-normal-unread ((t (:weight normal))))
   ;; buttons that appear in email addresses. I never use them, so I might end
   ;; up removing even the underline
   `(gnus-button ((t (:underline t))))
   `(gnus-header-name ((t (:inherit default)))) ;; all "Header:" text
   `(gnus-header-from ((t (:inherit default :underline t)))) ;; value of "From: "
   `(gnus-header-subject ((t (:inherit default)))) ;; value of "Subject: "
   `(gnus-header-content ((t (:inherit default)))) ;; other header text/values
   `(gnus-cite-1 ((t (:foreground ,alt-fg))))
   `(gnus-cite-2 ((t (:foreground ,string))))
   `(gnus-cite-3 ((t (:foreground ,warning))))
   `(gnus-cite-4 ((t (:foreground ,error))))
   `(gnus-cite-5 ((t (:foreground ,fg :slate t))))
   `(gnus-cite-6 ((t (:foreground ,alt-fg :slate t))))
   `(message-header-name ((t (:inherit default))))
   `(message-header-to ((t (:weight bold))))
   `(message-header-subject ((t (:inherit default))))
   `(message-header-other ((t (:inherit default))))
   `(message-header-cc ((t (:inherit default))))
   `(message-separator ((t (:inherit highlight))))
   `(message-signature-separator ((t (:inherit highlight))))

   ;; comint (add ansi-color?)
   `(comint-highlight-input ((t (:foreground ,alt-fg))))

   ;; customize and widgets
   `(custom-button ((t (:inherit button))))
   `(widget-field ((t (:box t :background ,bg))))

   ;; eww & shr
   `(eww-valid-certificate ((t (:foreground ,fg))))
   `(eww-invalid-certificate ((t (:inherit error))))
   `(eww-form-text ((t (:inherit button :background ,dim-0))))
   `(eww-form-checkbox ((t (:inherit button :background ,dim-0))))
   `(eww-form-submit ((t (:inherit button :background ,dim-0))))
   ;; TODO: find a way to show a box around the text area, but not
   ;;       in every single line
   `(eww-form-textarea ((t (:background ,dim-0))))
   ;; for markdown, it is simpler to let the "#" characters state the header
   ;;  depth, but for rendered HTML, there's no visual indication. So use a
   ;;  little color and bold/slate properties
   `(shr-h1 ((t (:foreground ,fg :weight bold :underline t))))
   `(shr-h2 ((t (:foreground ,alt-fg :weight bold :underline t))))
   `(shr-h3 ((t (:foreground ,string :weight bold :underline t))))
   `(shr-h4 ((t (:foreground ,fg :weight bold))))
   `(shr-h5 ((t (:foreground ,alt-fg :weight bold))))
   `(shr-h6 ((t (:foreground ,string :weight bold))))

   ;; custom.el
   `(custom-group-tag ((t (:foreground ,fg :weight bold :underline t))))
   `(custom-variable-tag ((t (:foreground ,fg))))

   ;; diff
   `(diff-header ((t (:background ,dim-1))))
   `(diff-file-header ((t (:background ,dim-1))))
   ;; TODO: ediff

   ;; help - who knew
   `(help-key-binding ((t (:inherit fixed-pitch))))

   ;; make terminal look the same as GUI - for default faces
   ;; (I guess I could set these unconditionally too...since I am
   ;; going to use the default values anyway)
   (unless (display-graphic-p)
     `(diff-removed ((t (:background "ffeeee" :extend t))))
     `(diff-refine-removed ((t (:background "#ffcccc"))))
     `(diff-added ((t (:background "#eeffee" :extend t))))
     `(diff-refine-added ((t (:background "#bbddbb")))))
   ))
;;;###autoload
(when (and (boundp 'custom-theme-load-path) load-file-name)
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'hoagie)
