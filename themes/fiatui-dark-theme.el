;;; phoenix-dark-mono-theme.el --- Monochromatic version of the Phoenix theme
;;; Commentary:

;; Copyright 2013 J Irving

;; Author: J Irving <j@lollyshouse.ca>
;; URL: http://github.com/j0ni/phoenix-dark-mono
;; Version: 20130306.1215
;; X-Original-Version: 1.0

;;; Code:
(require 'color)

(defvar default-blend-factor 0.95)
(defvar default-blend-factor+1 0.68)

;; * Colors

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
  (colir-blend (color-values a) (color-values b) (or alpha default-blend-factor)))

(defun blend+1 (a b)
  (blend a b default-blend-factor+1))

(defun morph-blend (a b n)
  (mapcar #'(lambda (vals) (apply #'color-rgb-to-hex vals))
          (color-gradient (color-name-to-rgb a)
                          (color-name-to-rgb b) n)))

(deftheme fiatui-dark "Phoenix Dark Mono color theme")

(defvar fd-blue      "#49B6FC")
(defvar fd-blue+1    "#9cd3f6")
(defvar fd-yellow    "#FFFB0D")
(defvar fd-yellow+1  "#f4f3a4")
(defvar fd-red       "#c0392b")
(defvar fd-orange    "#E67E22")
(defvar fd-magenta   "#b92980")
(defvar fd-magenta-1 "#6a224e")
(defvar fd-green     "#29b962")
(defvar fd-white+1   "#fff")
(defvar fd-white     "#e3e3e3")
(defvar fd-white-1   "#bbbbbb")
(defvar fd-white-2   "#939393")
(defvar fd-black+2   "#6b6b6b" )
(defvar fd-black+1   "#434343")
(defvar fd-black     "#1c1c1c")
(defvar fd-black-1   "#0f0f0f")

(custom-theme-set-faces
 'fiatui-dark
 ;; * Default
 `(default                                   ((t (:background ,fd-black :foreground ,fd-white :slant normal :weight normal :width normal))))
 `(cursor                                    ((t (:foreground nil :background ,fd-blue :underline nil :weight bold))))
 `(info                                      ((t (:foreground ,fd-black+1 :background ,fd-blue))))
 `(fixed-pitch                               ((t (:underline nil :weight normal :family "PragmataPro"))))
 `(variable-pitch                            ((t (:family "Sans Serif" :weight normal :underline nil))))
 `(escape-glyph                              ((t (:weight normal :underline nil :foreground ,fd-white))))
 `(minibuffer-prompt                         ((t (:weight normal :underline nil :foreground ,fd-white))))

 ;; * Font lock
 `(font-lock-builtin-face                    ((t (:weight normal :underline nil :foreground ,fd-white))))
 `(font-lock-comment-delimiter-face          ((t (:foreground ,fd-black+1 :inherit font-lock-comment-face))))
 `(font-lock-comment-face                    ((t (:foreground ,fd-white-1 :background ,(blend fd-black fd-black+2) :underline nil :weight normal))))
 `(font-lock-constant-face                   ((t (:foreground ,fd-white-1 :background ,(blend fd-black fd-white-1) nil :underline nil :weight normal))))
 `(font-lock-doc-face                        ((t (:weight normal :underline nil :foreground ,fd-white-1 :background ,(blend fd-black fd-white+1) :inherit nil))))
 `(font-lock-variable-name-face              ((t (:foreground ,fd-white-2 :background ,(blend fd-black fd-white-2) :underline nil :weight normal))))
 `(font-lock-function-name-face              ((t (:foreground ,fd-white :background ,(blend fd-black fd-white) :weight normal))))
 `(font-lock-keyword-face                    ((t (:foreground ,fd-black+2 :underline nil :weight bold))))
 `(font-lock-negation-char-face              ((t (:weight normal :underline nil :foreground ,fd-white))))
 `(font-lock-preprocessor-face               ((t (:weight normal :underline nil :foreground ,fd-white :inherit (font-lock-builtin-face)))))
 `(font-lock-regexp-grouping-backslash       ((t (:weight normal :underline nil :inherit (bold)))))
 `(font-lock-regexp-grouping-construct       ((t (:weight normal :underline nil :inherit (bold)))))
 `(font-lock-string-face                     ((t (:foreground ,fd-white :background ,(blend fd-black fd-white) :underline nil :weight normal))))
 `(font-lock-type-face                       ((t (:foreground ,fd-white-1 :underline nil :weight normal))))
 `(font-lock-warning-face                    ((t (:weight normal :underline nil :foreground ,fd-white-1 :inherit (error)))))

 ;; * Highlights
 `(error                                     ((t (:foreground ,fd-red :background ,fd-black+1 :weight bold))))
 `(warning                                   ((t (:foreground ,fd-orange :background ,(blend fd-black fd-orange)))))
 `(highlight                                 ((t (:background nil :foreground ,fd-yellow :underline nil :weight normal))))
 `(hl-sexp-face                              ((t (:background "#272727"))))
 `(hl-line                                   ((t (:foreground nil :background ,(blend fd-black fd-blue)  :box nil))))
 `(region                                    ((t (:weight normal :underline nil :background ,(blend+1 fd-blue fd-black)))))
 `(shadow                                    ((t (:weight normal :underline nil :foreground ,fd-white-1))))
 `(secondary-selection                       ((t (:weight normal :underline nil :background ,fd-black+1))))
 `(trailing-whitespace                       ((t (:background ,fd-black+2 :underline nil :weight normal))))
 `(highlight-parentheses                     ((t (:inherit highlight))))
 `(show-paren-match-face                     ((t (:inherit highlight :foreground ,fd-blue :background ,fd-black+1))))
 `(rainbow-delimiters-depth-8-face           ((t (:foreground ,fd-white+1))))
 `(rainbow-delimiters-depth-7-face           ((t (:foreground ,fd-white))))
 `(rainbow-delimiters-depth-6-face           ((t (:foreground ,fd-white))))
 `(rainbow-delimiters-depth-5-face           ((t (:foreground "#cfcfcf"))))
 `(rainbow-delimiters-depth-4-face           ((t (:foreground "#bfbfbf"))))
 `(rainbow-delimiters-depth-3-face           ((t (:foreground "#afafaf"))))
 `(rainbow-delimiters-depth-2-face           ((t (:foreground "#9f9f9f"))))
 `(rainbow-delimiters-depth-1-face           ((t (:foreground "#8f8f8f"))))
 `(highlight-indentation-face                ((t (:inherit highlight))))
 `(highlight-indentation-current-column-face ((t (:inherit highlight))))

 ;; * Diff
 `(diff-added                                ((t (:foreground ,fd-green :background ,(blend fd-black fd-green) ))))
 `(diff-removed                              ((t (:foreground ,fd-red :background ,(blend fd-black fd-red)))))
 `(diff-hl-change                            ((t (:background ,fd-black+2 :foreground ,fd-black+2))))
 `(diff-hl-delete                            ((t (:background ,fd-black-1 :foreground ,fd-black))))
 `(diff-hl-insert                            ((t (:background ,fd-white :foreground ,fd-black+2))))

 ;; * Chrome
 `(fringe                                    ((t (:background ,fd-black+1 :foreground ,fd-black+1 :underline nil :weight normal))))
 `(tooltip                                   ((t (:weight normal :underline nil :foreground ,fd-white :background ,fd-black+1 :inherit (variable-pitch)))))

 ;; * Navigation
 `(compilation-info                          ((t (:weight normal :foreground ,fd-white))))
 `(link                                      ((t (:weight normal :underline nil :foreground ,fd-white))))
 `(link-visited                              ((t (:weight normal :underline nil :foreground ,fd-white :inherit (link)))))
 `(button                                    ((t (:foreground ,fd-white :underline nil :weight normal))))

 ;; * Mode line
 `(mode-line                                 ((t (:weight normal :underline nil :box nil :foreground ,fd-white :background ,fd-black+2))))
 `(mode-line-inactive                        ((t (:weight normal :underline nil :box nil :foreground ,fd-black+1 :background ,fd-black+1 :inherit (mode-line)))))
 `(mode-line-buffer-id                       ((t (:weight normal :foreground ,fd-white))))
 `(mode-line-emphasis                        ((t (:weight normal :underline nil))))
 `(mode-line-highlight                       ((t (:weight normal :underline nil :box ))))
 `(powerline-active1                         ((t (:background ,fd-black))))
 `(powerline-active2                         ((t (:background ,fd-black+1))))
 `(powerline-inactive1                       ((t (:background ,fd-black+2))))
 `(powerline-inactive2                       ((t (:background ,fd-white-2))))
 `(spaceline-highlight-face                  ((t (:foreground ,fd-black :background ,fd-blue))))
 `(spaceline-flycheck-error                  ((t (:inherit nil :foreground ,fd-red :background nil))))
 `(spaceline-flycheck-warning                ((t (:inherit nil :foreground ,fd-orange :background nil))))
 `(spaceline-flycheck-info                   ((t (:foreground ,fd-blue :background nil))))

 `(persp-selected-face                       ((t (:inherit nil :foreground ,fd-black+1 ))))
 `(header-line                               ((t (:weight normal :underline nil :foreground ,fd-white :background ,fd-black+1 :inherit mode-line))))

 ;; * Grep
 `(grep-context-face                         ((t (:foreground "#cccccc"))))
 `(grep-error-face                           ((t (:foreground ,fd-white :underline t))))
 `(grep-hit-face                             ((t (:foreground ,fd-white))))
 `(grep-match-face                           ((t (:foreground ,fd-white))))

 ;; * Search
 `(isearch                                   ((t (:weight normal :underline nil :foreground ,fd-white :background ,fd-magenta))))
 `(isearch-fail                              ((t (:weight normal :underline nil :foreground ,fd-white :background "#858585"))))
 `(lazy-highlight                            ((t (:weight normal :underline nil :background ,fd-magenta-1 :foreground nil))))
 `(match                                     ((t (:weight normal :underline nil :foreground ,fd-white :background "#2b2b2b"))))
 `(next-error                                ((t (:weight normal :underline nil :inherit (region)))))
 `(query-replace                             ((t (:weight normal :underline nil :inherit (isearch)))))

 ;; * Eshell
 `(eshell-prompt                             ((t (:inherit font-lock-function-name-face))))
 `(eshell-ls-directory                       ((t (:inherit dired-directory))))

 ;; * Lines
 `(linum                                     ((t (:foreground ,fd-black+2))))

 ;; * Flycheck
 `(flycheck-info                             ((t (:foreground ,fd-blue))))
 `(flycheck-warning                          ((t (:inherit warning))))
 `(flycheck-error                            ((t (:inherit error))))

 ;; * Eval sexp
 `(eval-sexp-fu-flash                        ((t (:background ,fd-blue :foreground nil))))
 `(nrepl-eval-sexp-fu-flash                  ((t (:background ,fd-blue :foreground nil))))

 ;; * UIs
 `(hydra-face-red                            ((t (:foreground ,fd-magenta :weight bold))))
 `(hydra-face-blue                           ((t (:foreground ,fd-blue :weight bold))))
 `(hydra-face-teal                           ((t (:foreground ,fd-blue+1 :weight bold))))
 `(hydra-face-pink                           ((t (:foreground "pink" :weight bold))))
 `(avy-lead-face                             ((t (:background nil :foreground ,fd-yellow :weight normal))))
 `(avy-lead-face-0                           ((t (:background nil :foreground ,fd-blue :weight normal))))
 `(avy-lead-face-1                           ((t (:background nil :foreground ,fd-yellow :weight normal))))
 `(avy-lead-face-2                           ((t (:background nil :foreground ,fd-yellow :weight normal))))
 `(popup-tip-face                            ((t (:background ,fd-black+1 :foreground "#a5a5a5"))))
 `(popup-scroll-bar-foreground-face          ((t (:background ,fd-black+2))))
 `(popup-scroll-bar-background-face          ((t (:background ,fd-black+2))))

 ;; * Magit
 `(magit-header                              ((t (:foreground ,fd-white :background ,fd-black+1 :box (:line-width 1 :color "grey20")))))
 `(magit-log-sha1                            ((t (:foreground ,fd-white :background ,fd-black+1))))
 `(magit-section-heading                     ((t (:foreground  ,fd-blue :background ,fd-black+1))))
 `(magit-branch                              ((t (:foreground ,fd-white))))

 ;; * ERC
 `(erc-action-face                           ((t (:inherit erc-default-face))))
 `(erc-bold-face                             ((t (:weight bold))))
 `(erc-current-nick-face                     ((t (:foreground ,fd-white :weight bold))))
 `(erc-dangerous-host-face                   ((t (:inherit font-lock-warning))))
 `(erc-default-face                          ((t (:foreground ,fd-white))))
 `(erc-direct-msg-face                       ((t (:inherit erc-default))))
 `(erc-error-face                            ((t (:inherit font-lock-warning))))
 `(erc-fool-face                             ((t (:inherit erc-default))))
 `(erc-highlight-face                        ((t (:inherit hover-highlight))))
 `(erc-input-face                            ((t (:foreground ,fd-white))))
 `(erc-keyword-face                          ((t (:foreground ,fd-white :weight bold))))
 `(erc-nick-default-face                     ((t (:foreground ,fd-white :weight bold))))
 `(erc-my-nick-face                          ((t (:foreground ,fd-white+1 :weight bold))))
 `(erc-nick-msg-face                         ((t (:inherit erc-default))))
 `(erc-notice-face                           ((t (:foreground "#a8a8a8" :background ,fd-black+2))))
 `(erc-pal-face                              ((t (:foreground ,fd-white+1 :weight bold))))
 `(erc-prompt-face                           ((t (:foreground ,fd-white :background ,fd-black+2 :weight bold))))
 `(erc-timestamp-face                        ((t (:foreground ,fd-black+2))))
 `(erc-underline-face                        ((t (:underline t))))

 ;; * org
 `(org-level-8                               ((t (:foreground ,fd-white+1))))
 `(org-level-7                               ((t (:foreground ,fd-white))))
 `(org-level-6                               ((t (:foreground ,fd-white))))
 `(org-level-5                               ((t (:height 0.9 :foreground ,fd-white-2))))
 `(org-level-4                               ((t (:height 1.0 :foreground ,fd-white-2))))
 `(org-level-3                               ((t (:height 1.0 :foreground ,fd-white-1))))
 `(org-level-2                               ((t (:height 1.0 :foreground ,fd-white))))
 `(org-level-1                               ((t (:height 1.0 :foreground ,fd-blue))))

 ;; * company
 `(company-preview                           ((t (:foreground ,fd-white :background ,fd-black+1))))
 `(company-preview-common                    ((t (:foreground ,fd-white :background ,fd-black+1))))
 `(company-tooltip                           ((t (:foreground ,fd-white :background ,fd-black+1))))
 `(company-tooltip-common                    ((t (:foreground ,fd-black+2 :background ,fd-black+1))))
 `(company-tooltip-selection                 ((t (:foreground ,fd-blue :background ,fd-black+1 ))))
 `(company-tooltip-common-selection          ((t (:foreground ,fd-blue :background ,fd-black+1))))
 `(company-scrollbar-fg                      ((t (:background ,fd-black+1))))
 `(company-scrollbar-bg                      ((t (:background ,fd-black+2))))

 ;; * helm
 `(helm-source-header                        ((t (:background ,fd-black-1 :foreground ,fd-white :box nil))))
 `(helm-match                                ((t (:foreground ,fd-blue))))
 `(helm-selection                            ((t (:foreground nil :background ,(blend fd-black fd-blue)  :box nil))))
 `(helm-buffer-not-saved                     ((t (:foreground ,fd-white))))
 `(helm-buffer-process                       ((t (:foreground ,fd-white-1 :background ,fd-black))))
 `(helm-ff-dotted-directory                  ((t (:foreground ,fd-black+2 :background nil))))
 `(helm-ff-directory                         ((t (:foreground ,fd-white-1 :background nil))))
 `(helm-ff-file                              ((t (:foreground ,fd-white :background ,fd-black))))
 `(helm-buffer-size                          ((t (:foreground ,fd-black+2))))
 `(helm-buffer-directory                     ((t (:foreground ,fd-black+2))))
 `(helm-ff-executable                        ((t (:foreground ,fd-blue :background nil)))))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'fiatui-dark)

;;; phoenix-dark-mono-theme.el ends here
