;;; phoenix-dark-mono-theme.el --- Monochromatic version of the Phoenix theme
;;; Commentary:

;; Copyright 2013 J Irving

;; Author: J Irving <j@lollyshouse.ca>
;; URL: http://github.com/j0ni/phoenix-dark-mono
;; Version: 20130306.1215
;; X-Original-Version: 1.0

;;; Code:

(deftheme fiatui-dark "Phoenix Dark Mono color theme")

(let ((fd-blue "#49B6FC"))
  (custom-theme-set-faces
   'fiatui-dark
   '(default ((t (:inherit nil :stipple nil :background "#292929" :foreground "#ddd" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :width normal))))
   '(cursor ((t (:background "#ddd" :foreground "#ddd" :underline nil :weight bold))))
   `(info ((t (:foreground "#292929" :background ,fd-blue))))
   '(error ((t (:foreground "#c0392b" :background "#292929" :weight bold))))
   '(warning ((t (:foreground "#292929" :background "#E67E22"))))
   '(diff-added ((t (:foreground "#20854A" ))))
   '(diff-removed ((t (:foreground "#c0392b" ))))
   '(diff-hl-change ((t (:background "#393939" :foreground "#393939"))))
   '(diff-hl-delete ((t (:background "#191919" :foreground "#191919"))))
   '(diff-hl-insert ((t (:background "#757575" :foreground "##757575"))))
   '(fixed-pitch ((t (:underline nil :weight normal :family "PragmataPro"))))
   '(variable-pitch ((t (:family "Sans Serif" :weight normal :underline nil))))
   '(escape-glyph ((t (:weight normal :underline nil :foreground "#ddd"))))
   '(minibuffer-prompt ((t (:weight normal :underline nil :foreground "#ddd"))))
   `(highlight ((t (:background ,fd-blue :underline nil :weight normal))))
   '(hl-sexp-face ((t (:background "#272727"))))
   '(hl-line ((t (:background "#393939" :underline nil :weight normal))))
   '(region ((t (:weight normal :underline nil :background "#dfdfdf"))))
   '(shadow ((t (:weight normal :underline nil :foreground "grey70"))))
   '(secondary-selection ((t (:weight normal :underline nil :background "#3f3f3f"))))
   '(trailing-whitespace ((t (:background "#757575" :underline nil :weight normal))))

   '(font-lock-builtin-face ((t (:weight normal :underline nil :foreground "#ddd"))))
   '(font-lock-comment-delimiter-face ((t (:weight normal :underline nil :foreground "#555" :inherit font-lock-comment-face))))
   '(font-lock-comment-face ((t (:foreground "#757575" :background "#292929":underline nil :weight normal))))
   '(font-lock-constant-face ((t (:foreground "#BBB" :underline nil :weight normal))))
   '(font-lock-doc-face ((t (:weight normal :underline nil :858585 "#" :background "#292929" :inherit (font-lock-string-face)))))
   `(font-lock-function-name-face ((t (:foreground ,fd-blue :underline nil :weight normal))))
   `(font-lock-keyword-face ((t (:foreground "#757575" :underline nil :weight bold))))
   '(font-lock-negation-char-face ((t (:weight normal :underline nil :foreground "#ddd"))))
   '(font-lock-preprocessor-face ((t (:weight normal :underline nil :foreground "#ddd" :inherit (font-lock-builtin-face)))))
   '(font-lock-regexp-grouping-backslash ((t (:weight normal :underline nil :inherit (bold)))))
   '(font-lock-regexp-grouping-construct ((t (:weight normal :underline nil :inherit (bold)))))
   '(font-lock-string-face ((t (:foreground "#a5a5a5" :background "#2e2e2e" :underline nil :weight normal))))
   '(font-lock-type-face ((t (:foreground "#c0c0c0" :underline nil :weight normal))))
   '(font-lock-variable-name-face ((t (:foreground "#dfdfdf" :underline nil :weight normal))))
   '(font-lock-warning-face ((t (:weight normal :underline nil :foreground "#c0c0c0" :inherit (error)))))

   '(sp-pair-overlay-face ((t (:background "#292929"))))
   '(compilation-info ((t (:weight normal :foreground "#ddd"))))
   '(link ((t (:weight normal :underline nil :foreground "#ddd"))))
   '(link-visited ((t (:weight normal :underline nil :foreground "#dfdfdf" :inherit (link)))))
   '(button ((t (:foreground "#ddd" :underline nil :weight normal))))
   '(fringe ((t (:background "#262626" :foreground "#292929" :underline nil :weight normal))))
   '(header-line ((t (:weight normal :underline nil :inherit (mode-line)))))
   '(tooltip ((t (:weight normal :underline nil :foreground "#ddd" :background "#292929" :inherit (variable-pitch)))))
   `(persp-selected-face ((t (:foreground ,fd-blue))))
   '(mode-line ((t (:weight normal :underline nil :box nil :foreground "#ddd" :background "#393939"))))
   '(mode-line-buffer-id ((t (:weight normal :underline nil :foreground "#ddd"))))
   '(mode-line-emphasis ((t (:weight normal :underline nil))))
   '(mode-line-highlight ((t (:weight normal :underline nil :box nil))))
   '(mode-line-inactive ((t (:weight normal :underline nil :box nil :foreground "#757575" :background  "#232323" :inherit (mode-line)))))
   '(isearch ((t (:weight normal :underline nil :foreground "#ddd" :background "#2980b9"))))
   '(isearch-fail ((t (:weight normal :underline nil :foreground "#ddd" :background "#858585"))))
   '(lazy-highlight ((t (:weight normal :underline nil :foreground "#2980b9" :background "#2f2f2f"))))

   '(grep-context-face ((t (:foreground "#cccccc"))))
   '(grep-error-face ((t (:foreground "#dfdfdf" :underline t))))
   '(grep-hit-face ((t (:foreground "#dfdfdf"))))
   '(grep-match-face ((t (:foreground "#dfdfdf"))))
   '(match ((t (:weight normal :underline nil :foreground "#dfdfdf" :background "#2b2b2b"))))

   '(next-error ((t (:weight normal :underline nil :inherit (region)))))
   '(query-replace ((t (:weight normal :underline nil :inherit (isearch)))))

   '(ido-first-match ((t (:foreground "#dfdfdf" :weight bold))))
   '(ido-only-match ((t (:foreground "#dfdfdf" :weight bold))))
   '(ido-subdir ((t (:foreground "#a5a5a5"))))

   '(linum ((t (:foreground "#757575"))))

   '(flycheck-warning ((t (:inherit warning))))
   '(flycheck-warning ((t (:inherit error))))
   '(ac-candidate-face ((t (:background "#292929" :foreground "#a5a5a5"))))
   '(ac-candidate-mouse-face ((t (:background "#393939" :foreground "#ddd"))))
   '(ac-selection-face ((t (:background "#393939" :foreground "#ddd"))))
   '(ac-yasnippet-selection-face ((t (:background "#393939" :foreground "#ddd"))))
   '(ac-yasnippet-candidate-face ((t (:background "#292929" :foreground "#a5a5a5"))))
   '(popup-tip-face ((t (:background "#292929" :foreground "#a5a5a5"))))
   '(popup-scroll-bar-foreground-face ((t (:background "#393939"))))
   '(popup-scroll-bar-background-face ((t (:background "#393939"))))

   '(eval-sexp-fu-flash ((t (:background "#393939" :foreground "white"))))
   '(nrepl-eval-sexp-fu-flash ((t (:background "#393939" :foreground "white"))))

   '(magit-header ((t (:foreground "#dfdfdf" :background "#292929" :box (:line-width 1 :color "grey20")))))
   '(magit-log-sha1 ((t (:foreground "#dfdfdf" :background "#292929"))))
   '(magit-section-title ((t (:foreground "#dfdfdf" :background "#393939"))))
   '(magit-branch ((t (:foreground "#ddd"))))
   '(magit-item-highlight ((t (:inherit (highlight-parentheses)))))

   '(highlight-parentheses ((t (:inherit highlight))))
   `(show-paren-match-face ((t (:inherit highlight :foreground ,fd-blue :background "#292929"))))

   '(erb-face ((t (:foreground "#ddd" :background "#393939"))))
   '(erb-exec-face ((t (:inherit erb-face))))
   '(erb-out-face ((t (:inherit erb-face))))
   '(erb-delim-face ((t (:inherit erb-face :foreground "#efefef" :background "#393939"))))
   '(erb-exec-delim-face ((t (:inherit erb-delim-face))))
   '(erb-out-delim-face ((t (:inherit erb-delim-face :foreground "#efefef" :background "#393939"))))
   '(erb-comment-face ((t (:inherit erb-face :foreground "#757575" :background "#393939"))))
   '(erb-comment-delim-face ((t (:inherit erb-face :foreground "#555" :background "#393939"))))

   '(rainbow-delimiters-depth-8-face ((t (:foreground "#efefef"))))
   '(rainbow-delimiters-depth-7-face ((t (:foreground "#dfdfdf"))))
   '(rainbow-delimiters-depth-6-face ((t (:foreground "#ddd"))))
   '(rainbow-delimiters-depth-5-face ((t (:foreground "#cfcfcf"))))
   '(rainbow-delimiters-depth-4-face ((t (:foreground "#bfbfbf"))))
   '(rainbow-delimiters-depth-3-face ((t (:foreground "#afafaf"))))
   '(rainbow-delimiters-depth-2-face ((t (:foreground "#9f9f9f"))))
   '(rainbow-delimiters-depth-1-face ((t (:foreground "#8f8f8f"))))

   '(js2-warning ((t (:foreground "#dfdfdf"))))
   '(js2-error ((t (:foreground "#efefef"))))
   '(js2-jsdoc-tag ((t (:foreground "#585858"))))
   '(js2-jsdoc-type ((t (:foreground "#757575"))))
   '(js2-jsdoc-value ((t (:foreground "#757575"))))
   '(js2-function-param ((t (:foreground "#bbb"))))
   '(js2-external-variable ((t (:foreground "#ddd"))))

   '(erc-action-face ((t (:inherit erc-default-face))))
   '(erc-bold-face ((t (:weight bold))))
   '(erc-current-nick-face ((t (:foreground "#ddd" :weight bold))))
   '(erc-dangerous-host-face ((t (:inherit font-lock-warning))))
   '(erc-default-face ((t (:foreground "#ddd"))))
   '(erc-direct-msg-face ((t (:inherit erc-default))))
   '(erc-error-face ((t (:inherit font-lock-warning))))
   '(erc-fool-face ((t (:inherit erc-default))))
   '(erc-highlight-face ((t (:inherit hover-highlight))))
   '(erc-input-face ((t (:foreground "#ddd"))))
   '(erc-keyword-face ((t (:foreground "#dfdfdf" :weight bold))))
   '(erc-nick-default-face ((t (:foreground "#dfdfdf" :weight bold))))
   '(erc-my-nick-face ((t (:foreground "#efefef" :weight bold))))
   '(erc-nick-msg-face ((t (:inherit erc-default))))
   '(erc-notice-face ((t (:foreground "#a8a8a8" :background "#393939"))))
   '(erc-pal-face ((t (:foreground "#efefef" :weight bold))))
   '(erc-prompt-face ((t (:foreground "#ddd" :background "#393939" :weight bold))))
   '(erc-timestamp-face ((t (:foreground "#757575"))))
   '(erc-underline-face ((t (:underline t))))

   '(org-level-8 ((t (:foreground "#efefef"))))
   '(org-level-7 ((t (:foreground "#dfdfdf"))))
   '(org-level-6 ((t (:foreground "#ddd"))))
   '(org-level-5 ((t (:height 1.1 :foreground "#cfcfcf"))))
   '(org-level-4 ((t (:height 1.2 :foreground "#bfbfbf"))))
   '(org-level-3 ((t (:height 1.3 :foreground "#afafaf"))))
   '(org-level-2 ((t (:height 1.4 :foreground "#9f9f9f"))))
   '(org-level-1 ((t (:height 1.5 :foreground "#8f8f8f"))))

   '(w3m-anchor ((t (:inherit link))))
   '(w3m-arrived-anchor ((t (:foreground "#ddd"))))
   '(w3m-form ((t (:foreground "#a5a5a5" :underline t))))
   '(w3m-header-line-location-title ((t (:foreground "#efefef"
                                                     :underline t :weight bold))))
   '(w3m-history-current-url ((t (:inherit match))))
   '(w3m-lnum ((t (:foreground "#757575"))))
   '(w3m-lnum-match ((t (:background "#a5a5a5"
                                     :foreground "#393939"))))
   '(w3m-lnum-minibuffer-prompt ((t (:foreground "#ddd"))))

   `(helm-header ((t (:background "#292929" :foreground "#757575"))))
   `(helm-source-header ((t (:background "#292929" :foreground "#ddd"))))
   `(helm-selection ((t (:background ,"#393939"))))
   `(helm-ff-directory ((t (:foreground ,fd-blue :background "#292929"))))
   `(helm-ff-file ((t (:foreground "#ddd" :background "#292929"))))
   `(helm-buffer-size ((t (:foreground "#757575"))))
   `(helm-buffer-directory ((t (:foreground "#757575"))))
   `(helm-ff-executable ((t (:foreground "#fdfdfd" :background "#292929"))))
   
   '(highlight-indentation-face ((t (:inherit highlight))))
   '(highlight-indentation-current-column-face ((t (:inherit highlight))))))


;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'fiatui-dark)

;;; phoenix-dark-mono-theme.el ends here
