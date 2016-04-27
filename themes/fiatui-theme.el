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

(defun colir-join (r g b)
  "Build a color from R G B.
Inverse of `color-values'."
  (format "#%02x%02x%02x"
          (ash r -8)
          (ash g -8)
          (ash b -8)))

(defun colir-blend (c1 c2 &optional alpha)
  "Blend the two colors C1 and C2 with ALPHA.
C1 and C2 are in the format of `color-values'.
ALPHA is a number between 0.0 and 1.0 which corresponds to the
influence of C1 on the result."
  (setq alpha (or alpha 0.5))
  (apply #'colir-join
         (cl-mapcar
          (lambda (x y)
            (round (+ (* x alpha) (* y (- 1 alpha)))))
          c1 c2)))

(defun blend (a b &optional alpha)
  (colir-blend (color-values a) (color-values b) alpha))


(deftheme fiatui
  "Inspired by the color scheme from flatuicolors.com.")

;; Colors
(setq fui-light "#f9fafa")
(setq fui-turquoise "#d12089")
(setq fui-emerald "#2ecc71")
(setq fui-river "#2980b9")
(setq fui-amethyst "#6e5494")
(setq fui-asphalt2 "#34495e")
(setq fui-asphalt "#333")
(setq fui-sunflower "#f1c40f")
(setq fui-carrot "#e67e22")
(setq fui-alizarin "#e74c3c")
(setq fui-clouds "#F6fdfd")
(setq fui-concrete "#95a5a6")
(setq fui-dark-turquoise "#16a085")
(setq fui-dark-emerald "#27ae60")
(setq fui-dark-river "#1C587F")
(setq fui-dark-asphalt "#2c3e50")
(setq fui-dark-sunflower "#f39c12")
(setq fui-dark-carrot "#d35400")
(setq fui-dark-alizarin "#a31515")
(setq fui-dark-clouds "#bdc3c7")
(setq fui-dark-concrete "#7f8c8d")
(setq fui-bg fui-clouds)
(setq fui-fg fui-asphalt)

(defun blend-bg (a &optional alpha)
  (colir-blend (color-values fui-bg) (color-values a) (or alpha 0.97)))

(defun blend-bg+ (a &optional alpha)
  (blend-bg a 0.68))

(custom-theme-set-faces
 'fiatui
 `(default ((t (:background ,fui-bg :foreground ,fui-fg))))
 `(cursor ((t (:background ,fui-amethyst :foreground ,fui-fg))))
 `(region ((t (:background ,(blend-bg+ fui-river) :foreground nil))))
 `(persp-selected-face ((t (:background ,fui-bg :foreground ,fui-river))))
 `(fringe ((t (:background ,fui-light :foreground ,fui-light))))
 `(show-paren-match-face ((t (:foreground ,fui-turquoise :weight bold :underline nil))))
 `(show-paren-mismatch ((t (:background ,fui-alizarin))))
 `(hl-line ((t (:background ,fui-light))))
 `(minibuffer-prompt ((t (:foreground ,fui-dark-concrete))))
 `(font-lock-builtin-face ((t (:foreground ,fui-river :background ,fui-bg))))
 `(tuareg-font-lock-operator-face ((t (:foreground ,fui-river))))
 `(font-lock-type-face ((t (:foreground ,fui-amethyst))))
 `(font-lock-doc-face ((t (:foreground ,fui-amethyst :background ,(blend-bg fui-amethyst 0.95)))))
 `(font-lock-comment-face ((t (:foreground ,fui-asphalt2 :background nil))))
 `(font-lock-function-name-face ((t (:foreground ,fui-amethyst :background ,(blend-bg fui-amethyst)))))
 `(font-lock-constant-face ((t (:foreground ,fui-dark-river))))
 `(font-lock-keyword-face ((t (:foreground ,fui-river :weight normal :background ,(blend-bg fui-river)))))
 `(font-lock-string-face ((t (:foreground ,fui-amethyst))))
 `(font-lock-variable-name-face ((t (:foreground ,fui-dark-river :background ,(blend-bg fui-dark-river)))))
 `(font-lock-warning-face ((t (:foreground ,fui-dark-carrot))))
 `(isearch ((t (:foreground ,fui-turquoise :background ,(blend-bg+ fui-turquoise)))))
 `(lazy-highlight ((t (:foreground ,fui-amethyst :background ,(blend-bg+ fui-amethyst)))))
 `(link ((t (:foreground ,fui-dark-river :underline t))))
 `(link-visited ((t (:foreground ,fui-dark-asphalt :underline t))))
 `(button ((t (:background ,fui-dark-asphalt :foreground ,fui-clouds :underline t))))
 `(header-line ((t (:background ,fui-bg :foreground ,fui-fg))))
 `(shadow ((t (:foreground ,fui-concrete))))
 `(beacon-color ((t (:background ,fui-river))))
 `(beacon-fallback-background ((t (:background ,fui-river))))
 `(eval-sexp-fu-flash ((t (:background ,fui-bg :foreground ,fui-river))))
 `(page-break-lines ((t (:foreground ,fui-dark-clouds))))

 `(iedit-occurrence ((t (:background ,(blend-bg+ fui-turquoise)))))

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
 `(outline-1 ((t (:foreground ,fui-amethyst))))
 `(outline-2 ((t (:foreground ,fui-river))))
 `(outline-3 ((t (:foreground ,fui-fg))))
 `(outline-4 ((t (:foreground ,fui-emerald))))
 `(outline-5 ((t (:foreground ,fui-carrot))))
 `(outline-6 ((t (:foreground ,fui-turquoise))))

 ;; Rainbow delimiters
 `(rainbow-delimiters-depth-1-face ((t (:foreground ,fui-fg))))
 `(rainbow-delimiters-depth-2-face ((t (:foreground ,fui-turquoise))))
 `(rainbow-delimiters-depth-3-face ((t (:foreground ,fui-river))))
 `(rainbow-delimiters-depth-4-face ((t (:foreground ,fui-carrot))))
 `(rainbow-delimiters-depth-5-face ((t (:foreground ,fui-dark-river))))
 `(rainbow-delimiters-depth-6-face ((t (:foreground ,fui-carrot))))
 `(rainbow-delimiters-depth-7-face ((t (:foreground ,fui-dark-concrete))))
 `(rainbow-delimiters-unmatched-face ((t (:foreground ,fui-alizarin))))

 ;; Magit
 `(magit-branch ((t (:foreground ,fui-river :background ,fui-bg))))
 `(magit-tag ((t (:foreground ,fui-river :background ,fui-bg))))
 `(magit-section-title ((t (:foreground ,fui-dark-emerald :background ,fui-bg))))
 `(magit-item-highlight ((t (:foreground ,fui-fg :background ,fui-light))))

 ;; Company
 `(company-preview ((t (:foreground ,fui-fg :background ,fui-sunflower))))
 `(company-preview-common ((t (:foreground ,fui-river :background ,fui-bg))))
 `(company-tooltip ((t (:foreground ,fui-fg :background ,fui-light))))
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
 `(powerline-active1 ((t (:background ,fui-river :foreground ,fui-bg :box nil))))
 `(powerline-active2 ((t (:background ,fui-dark-clouds :foreground ,fui-bg :box nil))))
 `(powerline-inactive1 ((t (:background ,fui-dark-clouds :foreground ,fui-bg :box nil))))
 `(powerline-inactive2 ((t (:background ,fui-dark-clouds :foreground ,fui-bg :box nil))))
 `(spaceline-highlight-face ((t (:background ,fui-amethyst :foreground ,fui-bg))))

 ;; Helm
 `(helm-header ((t (:background ,fui-bg))))
 `(helm-source-header ((t (:background ,fui-clouds :foreground ,fui-fg))))
 `(helm-selection ((t (:foreground nil :background ,(blend-bg+ fui-amethyst)))))
 `(helm-ff-dotted-directory ((t (:foreground ,fui-river :background ,fui-bg))))
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


