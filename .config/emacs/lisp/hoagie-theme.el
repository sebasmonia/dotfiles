;;; hoagie-theme.el --- My custom theme -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2025 Sebastián Monía
;;
;; Author: Sebastián Monía <sebastian@sebasmonia.com>
;; URL: https://git.sr.ht/~sebasmonia/dotfiles
;; Keywords: local faces

;; This file is not part of GNU Emacs.

;;; Commentary:
;; My personal theme, using Tok (https://github.com/topikettunen/tok-theme) as
;; a starting point.
;;
;; This is a variant that has some more colors, but as few as possible: dark
;; and light purple, dark green, dark orange, and dark red. Then a few greys
;; here and there. The theme's base is white background, and most faces are
;; simply plain black.

;;; Code:

(deftheme hoagie
  "My personal theme.
Uses five colors, a bunch of greys, and mostly black text on a white
background. It also tries to minimize the use of bolds and
slate/italics."
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
       (error "#ee0000") ;; red
       (warning "#ff8c00") ;; dark orange
       (bg-mode-line (if (display-graphic-p)
                         "#ffffff"
                       "#e5e5e5")))

  (custom-theme-set-faces 'hoagie
  ;; for live testing
  ;; (custom-set-faces
   `(default ((t (:foreground ,fg :background ,bg))))
   `(cursor ((t (:background ,alt-fg))))
   `(hl-line ((t (:background ,alt-bg))))
   `(highlight ((t (:background ,dim-1))))
   `(trailing-whitespace ((t (:underline t))))
   `(region ((t (:extend t :background ,dim-2))))
   `(secondary-selection ((t (:inherit region))))
   `(fixed-pitch ((t (:background ,dim-0))))
   `(shadow ((t (:foreground ,dim-4))))
   ;; next three where bold in the original theme
   `(error ((t (:foreground ,error))))
   `(warning ((t (:foreground ,warning))))
   `(success ((t (:foreground ,string))))
   `(fringe ((t (nil))))
   `(button ((t (:box t :background ,dim-0))))
   `(vertical-border ((t (:foreground ,dim-2))))
   `(link ((t (:underline t))))
   `(link-visited ((t (:underline t :foreground ,alt-fg))))
   `(escape-glyph ((t (:inherit error))))
   `(homoglyph ((t (:inherit error))))
   ;; It seems this one is only used in the Terminal
   `(separator-line ((t (:inherit default))))

   `(minibuffer-prompt ((t (nil))))
   `(completions-common-part ((t (:underline t :weight bold))))
   `(completions-annotations ((t (:inherit shadow))))
   `(change-log-date ((t (:inherit default))))
   `(diary ((t (:foreground ,alt-fg))))

   `(org-table ((t (:foreground ,alt-fg))))

   `(show-paren-match ((t (:background ,dim-2))))
   ;; next face won't be used unless I set again
   ;; (show-paren-when-point-inside-paren 'mixed)
   ;; in my init file
   `(show-paren-match-expression ((t (:background ,dim-1))))
   `(show-paren-mismatch ((t (:foreground ,warning :background ,dim-2))))

   `(mode-line ((t (:foreground ,fg :background ,bg-mode-line))))
   `(mode-line-active ((t (:inherit mode-line :box (:color ,fg)))))
   `(mode-line-inactive ((t (:foreground ,dim-3 :box (:color ,dim-1)))))
   `(mode-line-highlight ((t (:foreground ,error))))
   `(mode-line-emphasis ((t (:weight bold))))

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

   `(isearch ((t (:foreground ,bg :background ,fg))))
   `(isearch-fail ((t (:inherit warning))))
   `(isearch-group-1 ((t (:foreground ,alt-fg :background ,dim-0))))
   `(isearch-group-2 ((t (:foreground ,warning :background ,dim-0))))
   `(lazy-highlight ((t (:foreground ,fg :background ,dim-1))))
   `(match ((t (:foreground ,fg :background ,dim-1))))

   `(dired-directory ((t (:foreground ,alt-fg))))
   `(dired-broken-symlink ((t (:inherit error))))
   `(dired-marked ((t (:background ,dim-1))))

   `(markdown-header-face ((t :inherit font-lock-function-name-face)))
   `(markdown-header-delimiter-face ((t (nil))))
   `(markdown-metadata-key-face ((t (:inherit font-lock-comment-face))))
   `(markdown-metadata-value-face ((t (:inherit font-lock-comment-face))))
   `(markdown-blockquote-face ((t (nil))))
   `(markdown-pre-face ((t (nil))))
   `(markdown-code-face ((t (:foreground ,alt-fg))))
   `(markdown-markup-face ((t (:inherit font-lock-function-name-face))))
   ;; message, gnus (only email)
   `(gnus-group-mail-3-empty ((t nil))) ;; email directories
   ;; email directories with unread items: don't use bold, directories with no
   ;; new items are hidden by default anyway
   `(gnus-group-mail-3 ((t nil)))
   `(gnus-summary-normal-ticked ((t (:foreground ,string)))) ;; marked !
   `(gnus-summary-normal-read ((t nil)))
   `(gnus-summary-normal-ancient ((t nil))) ;; read too
   `(gnus-summary-cancelled ((t (:foreground ,dim-3)))) ;; deleted
   `(gnus-summary-normal-unread ((t (:weight normal))))
   ;; buttons that appear in email addresses. I never use them, so I might end
   ;; up removing even the underline
   `(gnus-button ((t (:underline t))))
   `(gnus-header-name ((t nil))) ;; all "Header:" text
   `(gnus-header-from ((t (:inherit default)))) ;; value of "From: "
   `(gnus-header-subject ((t nil))) ;; value of "Subject: "
   `(gnus-header-content ((t nil))) ;; other header text/values
   `(gnus-cite-attribution ((t nil)))
   `(gnus-cite-1 ((t (:foreground ,alt-fg))))
   `(gnus-cite-2 ((t (:foreground ,string))))
   `(gnus-cite-3 ((t (:foreground ,warning))))
   `(gnus-cite-4 ((t (:foreground ,error))))
   `(gnus-cite-5 ((t (:foreground ,fg))))
   `(gnus-cite-6 ((t (:foreground ,alt-fg))))
   `(gnus-cite-7 ((t (:foreground ,string))))
   `(gnus-cite-8 ((t (:foreground ,warning))))
   `(gnus-cite-9 ((t (:foreground ,error))))
   `(gnus-cite-10 ((t (:foreground ,fg))))
   `(gnus-cite-11 ((t (:foreground ,alt-fg))))

   `(message-header-name ((t nil)))
   `(message-header-to ((t (:weight bold))))
   `(message-header-subject ((t nil)))
   `(message-header-other ((t nil)))
   `(message-header-cc ((t nil)))
   `(message-separator ((t (:inherit highlight))))
   `(message-signature-separator ((t (:inherit highlight))))

   ;; TODO: add ansi-color?
   `(comint-highlight-input ((t (:foreground ,fg))))

   `(eww-valid-certificate ((t (:foreground ,fg))))
   `(eww-invalid-certificate ((t (:inherit error))))
   `(eww-form-text ((t (:inherit button :background ,dim-0))))
   `(eww-form-checkbox ((t (:inherit button :background ,dim-0))))
   `(eww-form-submit ((t (:inherit button :background ,dim-0))))
   ;; TODO: find a way to show a box around the text area, but not
   ;;       in every single line
   `(eww-form-textarea ((t (:background ,dim-0))))
   ;; For markdown, it is simple: let the "#" characters state the header
   ;; depth. But for rendered HTML, there's no visual indication. So use a
   ;; little color (for "stronger" to "weaker"). Add bold and underline to
   ;; differentiate it from regular text.
   ;; TODO: There's overlap between shr-h5/shr-h6 and links/visited links.
   ;;       We'll see if it's a problem in the future...
   `(shr-h1 ((t (:foreground ,string :bold t))))
   `(shr-h2 ((t (:foreground ,alt-fg :bold t))))
   `(shr-h3 ((t (:foreground ,fg :bold t))))
   `(shr-h4 ((t (:foreground ,string :underline t))))
   `(shr-h5 ((t (:foreground ,alt-fg :underline t))))
   `(shr-h6 ((t (:foreground ,fg :underline t))))

   `(custom-group-tag ((t (:foreground ,fg :weight bold :underline t))))
   `(custom-variable-tag ((t (:foreground ,fg))))
   `(custom-button ((t (:inherit button))))
   `(widget-field ((t (:box t :background ,bg))))

   `(diff-header ((t (:background ,dim-1))))
   `(diff-file-header ((t (:background ,dim-1))))
   ;; make terminal diff look the same as GUI - for default faces
   ;; (I guess I could set these unconditionally too...since I am
   ;; going to use the default values anyway)
   (unless (display-graphic-p)
     `(diff-removed ((t (:background "ffeeee" :extend t))))
     `(diff-refine-removed ((t (:background "#ffcccc"))))
     `(diff-added ((t (:background "#eeffee" :extend t))))
     `(diff-refine-added ((t (:background "#bbddbb")))))

   `(ediff-even-diff-A ((t (:background ,dim-1 :extend t))))
   `(ediff-even-diff-B ((t (:background ,dim-1 :extend t))))
   `(ediff-even-diff-C ((t (:background ,dim-1 :extend t))))
   `(ediff-even-diff-Ancestor ((t (:background ,dim-1 :extend t))))
   `(ediff-odd-diff-A ((t (:background ,dim-2 :extend t))))
   `(ediff-odd-diff-B ((t (:background ,dim-2 :extend t))))
   `(ediff-odd-diff-C ((t (:background ,dim-2 :extend t))))
   `(ediff-odd-diff-Ancestor ((t (:background ,dim-2 :extend t))))

   `(help-key-binding ((t (:inherit fixed-pitch))))

   `(dictionary-word-definition-face ((t (:inherit variable-pitch))))
   `(dictionary-word-entry-face ((t (:inherit dictionary-word-definition-face :background ,dim-1))))
   `(dictionary-button-face ((t (:inherit button))))
   `(dictionary-reference-face ((t (:inherit dictionary-word-definition-face :underline t :background ,dim-0))))

   `(epa-validity-high ((t (:inherit success))))
   `(epa-validity-medium ((t (:inherit warning))))
   `(epa-validity-low ((t (:inherit default))))
   `(epa-validity-disabled ((t (:inherit shadow))))

   `(eshell-prompt ((t (:foreground ,fg))))
   `(eshell-ls-executable ((t (:foreground ,alt-fg))))
   `(eshell-ls-archive ((t (:foreground ,fg))))
   `(eshell-ls-readonly ((t (:foreground ,fg))))
   `(eshell-ls-special ((t (:foreground ,fg))))
   `(eshell-ls-unreadable ((t (:inherit warning))))
   ;; it is used for "missing", so it inherited from "error"
   ;; but was also used to mark symlink targets, so...?
   `(eshell-ls-missing ((t (:foreground ,alt-fg))))

   `(sh-quoted-exec ((t (:foreground ,fg))))
   `(sh-heredoc ((t (:foreground ,fg))))

   `(log-view-commit-body ((t (:inherit default))))
   `(log-view-file ((t (:background ,dim-1))))
   `(log-view-message ((t (:background ,dim-1))))

   `(pulse-highlight-start-face ((t (:background ,error))))
   ))
;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'hoagie)
;;; hoagie-theme.el ends here
