;;; FlatUI-theme.el --- Custom face theme for Emacs

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

(deftheme FlatUI
  "Inspired by the color scheme from flatuicolors.com.")

;; Colors
(defvar fui-turquoise "#1abc9c")
(defvar fui-emerald "#2ecc71")
(defvar fui-river "#3498db")
(defvar fui-amethyst "#9b59b6")
;; (defvar fui-asphalt "#34495e")
(defvar fui-asphalt "#425d78")
(defvar fui-sunflower "#f1c40f")
(defvar fui-carrot "#e67e22")
(defvar fui-alizarin "#e74c3c")
(defvar fui-clouds "#ecf0f1")
(defvar fui-concrete "#95a5a6")
(defvar fui-dark-turquoise "#16a085")
(defvar fui-dark-emerald "#27ae60")
(defvar fui-dark-river "#2980b9")
(defvar fui-dark-amethyst "#8e44ad")
(defvar fui-dark-asphalt "#2c3e50")
(defvar fui-dark-sunflower "#f39c12")
(defvar fui-dark-carrot "#d35400")
(defvar fui-dark-alizarin "#c0392b")
(defvar fui-dark-clouds "#bdc3c7")
(defvar fui-dark-concrete "#7f8c8d")
(defvar fui-bg fui-clouds)
(defvar fui-fg fui-asphalt)

(custom-theme-set-faces
 'FlatUI
 `(default ((t (:background ,fui-bg
                            :foreground ,fui-fg))))
 `(cursor ((t (:background ,fui-alizarin
                           :foreground ,fui-fg))))
 `(region ((t (:background ,fui-dark-sunflower
                           :foreground ,fui-fg))))
 `(mode-line ((t (:background ,fui-emerald
                              :foreground ,fui-bg
                              :box nil))))
 `(mode-line-buffer-id ((t (:foreground ,fui-clouds))))
 `(mode-line-inactive ((t (:background ,fui-concrete
                                       :foreground ,fui-bg))))
 `(fringe ((t (:background ,fui-bg))))
 `(show-paren-match ((t (:background ,fui-emerald))))
 `(show-paren-mismatch ((t (:background ,fui-alizarin))))
 `(hl-line ((t (:background ,fui-dark-clouds))))
 `(minibuffer-prompt ((t (:foreground ,fui-dark-concrete))))
 `(font-lock-builtin-face ((t (:foreground ,fui-asphalt))))
 `(font-lock-type-face ((t (:foreground ,fui-river))))
 `(font-lock-doc-face ((t (:foreground ,fui-dark-river))))
 `(font-lock-comment-face ((t (:foreground ,fui-asphalt))))
 `(font-lock-constant-face ((t (:foreground ,fui-river))))
 `(font-lock-function-name-face ((t (:foreground ,fui-amethyst))))
 `(font-lock-keyword-face ((t (:foreground ,fui-asphalt))))
 `(font-lock-string-face ((t (:foreground ,fui-asphalt))))
 `(font-lock-variable-name-face ((t (:foreground ,fui-dark-emerald))))
 `(font-lock-warning-face ((t (:foreground ,fui-dark-carrot))))
 `(isearch ((t (:background ,fui-dark-concrete
                            :foreground ,fui-fg))))
 `(lazy-highlight ((t (:background ,fui-concrete))))
 `(link ((t (:foreground ,fui-dark-river :underline t))))
 `(link-visited ((t (:foreground ,fui-dark-asphalt :underline t))))
 `(button ((t (:background ,fui-dark-asphalt :foreground ,fui-clouds :underline t))))
 `(header-line ((t (:background ,fui-dark-concrete
                                :foreground ,fui-fg))))
 `(shadow ((t (:foreground ,fui-concrete))))
 
 ;; Whitespace
 `(whitespace-trailing ((t (:background ,fui-dark-clouds))))
 `(whitespace-line ((t (:background ,fui-dark-concrete :foreground ,fui-bg))))
 
 ;; ERC
 `(erc-notice-face ((t (:foreground ,fui-dark-river
                                    :weight unspecified))))
 `(erc-header-line ((t (:foreground ,fui-bg
                                    :background ,fui-dark-clouds))))
 `(erc-timestamp-face ((t (:foreground ,fui-emerald
                                       :weight unspecified))))
 `(erc-current-nick-face ((t (:foreground ,fui-dark-carrot
                                          :weight unspecified))))
 `(erc-prompt-face ((t (:foreground ,fui-dark-concrete
                                    :background nil
                                    :slant italic
                                    :weight unspecified))))
 `(erc-my-nick-face ((t (:foreground ,fui-dark-carrot))))
 `(outline-1 ((t (:foreground ,fui-bg :background ,fui-emerald))))
 `(outline-2 ((t (:foreground ,fui-bg :background ,fui-amethyst))))
 `(outline-3 ((t (:foreground ,fui-bg :background ,fui-river))))
 ;; Rainbow delimiters
 `(rainbow-delimiters-depth-1-face ((t (:foreground ,fui-fg))))
 `(rainbow-delimiters-depth-2-face ((t (:foreground ,fui-turquoise))))
 `(rainbow-delimiters-depth-3-face ((t (:foreground ,fui-dark-river))))
 `(rainbow-delimiters-depth-4-face ((t (:foreground ,fui-dark-amethyst))))
 `(rainbow-delimiters-depth-5-face ((t (:foreground ,fui-dark-sunflower))))
 `(rainbow-delimiters-depth-6-face ((t (:foreground ,fui-dark-emerald))))
 `(rainbow-delimiters-depth-7-face ((t (:foreground ,fui-dark-concrete))))
 `(rainbow-delimiters-unmatched-face ((t (:foreground ,fui-alizarin))))
 
 `(persp-selected-face ((t (:foreground ,fui-dark-river))))
 ;; Magit
 `(magit-branch ((t (:foreground ,fui-river :background ,fui-bg))))
 `(magit-tag ((t (:foreground ,fui-river :background ,fui-bg))))
 `(magit-section-title ((t (:foreground ,fui-dark-emerald :background ,fui-bg))))
 `(magit-item-highlight ((t (:foreground ,fui-fg :background ,fui-dark-clouds))))
 
 ;; Git-gutter-fringe
 `(git-gutter-fr:modified ((t (:foreground ,fui-amethyst))))
 `(git-gutter-fr:added ((t (:foreground ,fui-emerald))))
 `(git-gutter-fr:deleted ((t (:foreground ,fui-alizarin))))
 
 ;; Company
 `(company-preview ((t (:foreground ,fui-fg :background ,fui-sunflower))))
 `(company-preview-common ((t (:foreground ,fui-fg :background ,fui-carrot))))
 `(company-tooltip ((t (:foreground ,fui-fg :background ,fui-dark-clouds))))
 `(company-tooltip-common ((t (:foreground ,fui-dark-carrot))))
 `(company-tooltip-selection ((t (:background ,fui-concrete))))
 `(company-tooltip-common-selection ((t (:foreground ,fui-dark-carrot))))

 ;; Magit
 `(magit-diff-add ((t (:background ,fui-emerald :foreground ,fui-bg))))
 `(magit-diff-del ((t (:background ,fui-alizarin :foreground ,fui-bg))))
 ;; Helm
 `(helm-header ((t (:background ,fui-bg))))
 `(helm-source-header ((t (:background ,fui-dark-asphalt :foreground ,fui-bg))))
 `(helm-selection ((t (:background ,fui-dark-clouds))))

 ;; Powerline
 `(powerline-active2 ((t (:foreground ,fui-fg :background ,fui-dark-clouds))))
 `(powerline-active1 ((t (:foreground ,fui-bg :background ,fui-emerald))))
 `(powerline-inactive2 ((t (:foreground ,fui-bg :background ,fui-concrete))))
 `(powerline-inactive1 ((t (:foreground ,fui-fg :background ,fui-dark-clouds)))))

;; Moinmoin mode
(defun dv-moinmoin-set-face ()
  (set-face-attribute
   'moinmoin-wiki-link nil
   :foreground (dv-color dv-link-fg)
   :weight 'normal)
  (set-face-attribute
   'moinmoin-url nil
   :foreground (dv-color dv-link-fg)
   :height 1)
  (set-face-attribute
   'moinmoin-url-title nil
   :foreground (dv-color dv-link-fg))
  (set-face-attribute
   'moinmoin-h4 nil
   :foreground (dv-color dv-region-bg))
  (set-face-attribute
   'moinmoin-item nil
   :foreground (dv-color dv-builtin-fg)
   :weight 'normal)
  (set-face-attribute
   'moinmoin-tt nil
   :foreground (dv-color dv-string-fg))
  (set-face-attribute
   'moinmoin-code-braces nil
   :foreground (dv-color dv-default-fg))
  (set-face-attribute
   'moinmoin-code nil
   :foreground (dv-color dv-string-fg))
  (set-face-attribute
   'moinmoin-rule nil
   :foreground (dv-color dv-builtin-fg)
   :weight 'normal)
  (set-face-attribute
   'moinmoin-blockquote-text nil
   :foreground (dv-color dv-default-fg))
  (set-face-attribute
   'moinmoin-blockquote-indent nil
   :background (dv-color dv-default-bg)))

;; CUA
(defun dv-cua-set-face ()
  (set-face-attribute
   'cua-rectangle nil
   :foreground (dv-color dv-region-fg)
   :background (dv-color dv-region-bg)))
(if cua-mode
    (dv-cua-set-face)
  (add-hook 'cua-mode-hook 'dv-cua-set-face))

;; (setq powerline-color2 (dv-color dv-default-bg)))

(provide-theme 'FlatUI)

;; Local Variables:
;; no-byte-compile: t
;; End:

;;; Deviant-theme.el  ends here
