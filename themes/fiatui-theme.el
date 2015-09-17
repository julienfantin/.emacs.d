;;; fiatui-theme.el --- Custom face theme for Emacs

;; Copyright (C) 2010, 2011, 2012 Darksair.

;; This file is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(deftheme fiatui
  "Inspired by the color scheme from flatuicolors.com.")

;; Colors
(setq fui-white "#f9fafa")
(setq fui-turquoise "#d12089")
(setq fui-emerald "#2ecc71")
(setq fui-river "#0086b3")
(setq fui-amethyst "#6e5494")
(setq fui-asphalt2 "#34495e")
(setq fui-asphalt "#333")
(setq fui-sunflower "#f1c40f")
(setq fui-carrot "#e67e22")
(setq fui-alizarin "#e74c3c")
(setq fui-clouds "##F4F9FE")
(setq fui-concrete "#95a5a6")
(setq fui-dark-turquoise "#16a085")
(setq fui-dark-emerald "#27ae60")
(setq fui-dark-river "#2980b9")
(setq fui-dark-amethyst "#8e44ad")
(setq fui-dark-asphalt "#2c3e50")
(setq fui-dark-sunflower "#f39c12")
(setq fui-dark-carrot "#d35400")
(setq fui-dark-alizarin "#a31515")
(setq fui-dark-clouds "#bdc3c7")
(setq fui-dark-concrete "#7f8c8d")
(setq fui-bg "#E4F1FE")
(setq fui-fg fui-asphalt)

(custom-theme-set-faces
 'fiatui
 `(default ((t (:background ,fui-bg :foreground ,fui-fg))))
 `(cursor ((t (:background ,fui-alizarin :foreground ,fui-fg))))
 `(region ((t (:background ,fui-river :foreground ,fui-bg))))

 `(persp-selected-face ((t (:foreground ,fui-dark-sunflower))))

 `(fringe ((t (:background ,fui-bg))))
 `(show-paren-match ((t (:background ,fui-emerald))))
 `(show-paren-mismatch ((t (:background ,fui-alizarin))))
 `(hl-line ((t (:background ,fui-white))))
 `(hl-sexp-face ((t (:background "#EEF6FE"))))
 `(minibuffer-prompt ((t (:foreground ,fui-dark-concrete))))
 `(font-lock-builtin-face ((t (:foreground ,fui-dark-river))))
 `(tuareg-font-lock-operator-face ((t (:foreground ,fui-river))))
 `(font-lock-type-face ((t (:foreground ,fui-amethyst))))
 `(font-lock-doc-face ((t (:foreground ,fui-amethyst))))
 `(font-lock-comment-face ((t (:foreground ,fui-asphalt2 :background "#EEF6FE"))))
 `(font-lock-constant-face ((t (:foreground ,fui-river))))
 `(font-lock-function-name-face ((t (:foreground ,fui-dark-amethyst :weight normal))))
 `(font-lock-keyword-face ((t (:foreground ,fui-asphalt2 :weight normal))))
 `(font-lock-string-face ((t (:foreground ,fui-amethyst))))
 `(font-lock-variable-name-face ((t (:foreground ,fui-dark-asphalt))))
 `(font-lock-warning-face ((t (:foreground ,fui-dark-carrot))))
 `(isearch ((t (:background ,fui-amethyst
                            :foreground ,fui-bg))))
 `(lazy-highlight ((t (:background ,fui-dark-turquoise))))
 `(link ((t (:foreground ,fui-dark-river :underline t))))
 `(link-visited ((t (:foreground ,fui-dark-asphalt :underline t))))
 `(button ((t (:background ,fui-dark-asphalt :foreground ,fui-clouds :underline t))))
 `(header-line ((t (:background ,fui-bg :foreground ,fui-fg))))
 `(shadow ((t (:foreground ,fui-concrete))))

 `(highlight-symbol-face ((t (:background ,fui-sunflower))))

 ;; Whitespace
 `(whitespace-trailing ((t (:background ,fui-dark-clouds))))
 `(whitespace-line ((t (:background ,fui-dark-concrete :foreground ,fui-bg))))

 ;; Eshell
 `(eshell-prompt ((t (:foreground ,fui-river :weight bold))))
 `(eshell-ls-directory ((t (:inherit dired-directory))))
 `(eshell-prompt ((t (:foreground ,fui-river :weight bold))))
 `(eshell-ls-directory ((t (:inherit dired-directory))))

 ;; ERC
 `(erc-notice-face ((t (:foreground ,fui-dark-river :weight unspecified))))
 `(erc-header-line ((t (:foreground ,fui-bg :background ,fui-dark-clouds))))
 `(erc-timestamp-face ((t (:foreground ,fui-turquoise :weight unspecified))))
 `(erc-current-nick-face ((t (:foreground ,fui-dark-carrot :weight unspecified))))
 `(erc-prompt-face ((t (:foreground ,fui-dark-concrete :background nil :weight unspecified))))
 `(erc-my-nick-face ((t (:foreground ,fui-dark-carrot))))
 `(outline-1 ((t (:foreground ,fui-fg))))
 `(outline-2 ((t (:foreground ,fui-river))))
 `(outline-3 ((t (:foreground ,fui-amethyst))))
 `(outline-4 ((t (:foreground ,fui-emerald))))
 `(outline-5 ((t (:foreground ,fui-carrot))))
 `(outline-6 ((t (:foreground ,fui-turquoise))))

 ;; Rainbow delimiters
 `(rainbow-delimiters-depth-1-face ((t (:foreground ,fui-fg))))
 `(rainbow-delimiters-depth-2-face ((t (:foreground ,fui-river))))
 `(rainbow-delimiters-depth-3-face ((t (:foreground ,fui-amethyst))))
 `(rainbow-delimiters-depth-4-face ((t (:foreground ,fui-emerald))))
 `(rainbow-delimiters-depth-5-face ((t (:foreground ,fui-carrot))))
 `(rainbow-delimiters-depth-6-face ((t (:foreground ,fui-turquoise))))
 `(rainbow-delimiters-depth-7-face ((t (:foreground ,fui-dark-amethyst))))
 `(rainbow-delimiters-unmatched-face ((t (:foreground ,fui-alizarin))))
 `(rainbow-blocks-depth-1-face ((t (:foreground ,fui-fg))))
 `(rainbow-blocks-depth-2-face ((t (:foreground ,fui-river))))
 `(rainbow-blocks-depth-3-face ((t (:foreground ,fui-amethyst))))
 `(rainbow-blocks-depth-4-face ((t (:foreground ,fui-emerald))))
 `(rainbow-blocks-depth-5-face ((t (:foreground ,fui-carrot))))
 `(rainbow-blocks-depth-6-face ((t (:foreground ,fui-turquoise))))
 `(rainbow-blocks-depth-7-face ((t (:foreground ,fui-dark-amethyst))))
 `(rainbow-blocks-unmatched-face ((t (:foreground ,fui-alizarin))))

 ;; Magit
 `(magit-branch ((t (:foreground ,fui-river :background ,fui-bg))))
 `(magit-tag ((t (:foreground ,fui-river :background ,fui-bg))))
 `(magit-section-title ((t (:foreground ,fui-dark-emerald :background ,fui-bg))))
 `(magit-item-highlight ((t (:foreground ,fui-fg :background ,fui-white))))

 ;; Company
 `(company-preview ((t (:foreground ,fui-fg :background ,fui-sunflower))))
 `(company-preview-common ((t (:foreground ,fui-river :background ,fui-bg))))
 `(company-tooltip ((t (:foreground ,fui-fg :background ,fui-white))))
 `(company-tooltip-common ((t (:foreground ,fui-dark-river))))
 `(company-tooltip-selection ((t (:foreground ,fui-river))))
 `(company-tooltip-common-selection ((t (:foreground ,fui-dark-river))))
 `(company-scrollbar-fg ((t (:background ,fui-river))))
 `(company-scrollbar-bg ((t (:background ,fui-bg))))

 ;; Pos-tip
 `(pos-tip-foreground-color ((t (:foreground ,fui-river :background ,fui-river))))

 ;; Magit
 `(magit-diff-add ((t (:background ,fui-bg :foreground ,fui-emerald))))
 `(magit-diff-del ((t (:background ,fui-bg :foreground ,fui-alizarin))))

 `(mode-line ((t (:background ,fui-river :foreground ,fui-bg :box nil))))
 `(mode-line-buffer-id ((t (:foreground ,fui-clouds))))
 `(mode-line-inactive ((t (:background ,fui-concrete :foreground ,fui-bg nil))))

 ;; Helm
 `(helm-header ((t (:background ,fui-bg))))
 `(helm-source-header ((t (:background ,fui-clouds :foreground ,fui-fg))))
 `(helm-selection ((t (:foreground ,fui-bg :background ,fui-river))))
 `(helm-ff-directory ((t (:foreground ,fui-river :background ,fui-bg))))
 `(helm-ff-file ((t (:foreground ,fui-dark-asphalt :background ,fui-bg))))
 `(helm-ff-executable ((t (:foreground ,fui-emerald :background ,fui-bg))))

 `(helm-ls-git-added-modified-face ((t (:foreground ,fui-amethyst))))
 `(helm-ls-git-added-copied-face ((t (:foreground ,fui-sunflower))))
 `(helm-ls-git-deleted-and-staged-face ((t (:foreground ,fui-amethyst))))
 `(helm-ls-git-deleted-not-staged-face ((t (:foreground ,fui-alizarin))))
 `(helm-ls-git-modified-and-staged-face ((t (:foreground ,fui-dark-emerald))))
 `(helm-ls-git-modified-not-staged-face ((t (:foreground ,fui-carrot))))
 `(helm-ls-git-untracked-face ((t (:foreground ,fui-amethyst))))

 ;; Cider
 `(cider-stacktrace-face ((t (:background ,fui-fg)))))

(provide-theme 'fiatui)

;; Local Variables:
;; no-byte-compile: t
;; End:


