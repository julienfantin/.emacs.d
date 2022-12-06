;;; config-frame.el --- GUI frame configuration      -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Julien Fantin

;; Author: Julien Fantin <julienfantin@gmail.com>
;; Version: 1.3.2
;; Package-Requires: ((emacs "28") (use-package "2.4.4"))
;; Keywords: frames

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

;;; Config

(defvar config-frame-border-width 0)

;; TODO font config like
;; (:family "Iosevka Custom" :height 140 :ligatures t :line-spacing 0.2)
(defvar config-frame-mono-fonts
  '("JetBrains Mono-13"
    "Iosevka Custom-14"
    "SF Mono-13"
    "Hack"
    "Input"
    "Fira Code Retina"
    "Menlo"
    "DejaVu Sans Mono"
    "Source Code Pro"))

(defvar config-frame-variable-fonts
  '("DejaVu Sans" "ETBembo"))

(defvar config-frame-default-mono-font-height 130)

(defvar config-frame-default-variable-font-height config-frame-default-mono-font-height)

(defun config-frame-font-exists-p (font)
  "Existing 'FONT' predicate."
  (if (null (x-list-fonts (if (consp font) (car font) font))) nil t))

(defun config-frame-mono-font ()
  (cl-find-if #'config-frame-font-exists-p config-frame-mono-fonts))

(defun config-frame-variable-font ()
  (cl-find-if #'config-frame-font-exists-p config-frame-variable-fonts))

(defvar config-frame-frame-alist
  `((fullscreen              . maximized)
    (tool-bar-lines          . nil)
    (vertical-scroll-bars    . nil)
    (scroll-bars             . nil)
    (ns-transparent-titlebar . t)
    (internal-border-width   . ,config-frame-border-width)
    (font                    . ,(config-frame-mono-font))
    (line-spacing            . 0.2))
  "The default and initial frame alist.")

(when-let ((font (config-frame-mono-font)))
  (let ((font (if (consp font) (car font) font))
        (height (if (consp font) (cdr font) config-frame-default-mono-font-height)))
    (dolist (face '(default fixed-pitch))
      (set-face-attribute face nil :font font :height height))))

(when-let ((font (config-frame-variable-font)))
  (let ((font (if (consp font) (car font) font))
        (height (if (consp font) (cdr font) config-frame-default-variable-font-height)))
    (set-face-attribute 'variable-pitch nil :font font :height height)))

;;; Built-ins

;; Increase line-height with vertical centering.
;;
;; Setting both line-height and line-spacing we can work around the alignment
;; bug, but it does not work perfectly and the last line in a buffer does not
;; get the proper height (and/or spacing?). This is especially annoying when
;; narrowing in the mini-buffer where the candidate line's height changes when
;; so make sure not to enable that there.
;;
;; Someone wrote an emacs patch for this, but unfortunately it does not look
;; like it's going to get merged:
;; https://lists.gnu.org/archive/html/emacs-devel/2019-08/msg00659.html

(use-package emacs
  :config
  (setq-default cursor-type '(bar . 2))
  :custom
  ;; Use font-level line-spacing to work around emacs bug
  (default-text-properties '(line-spacing 0.2))
  (x-underline-at-descent-line nil))

(use-package frame
  :custom
  ;; don't resize when changing margins etc
  (frame-inhibit-implied-resize t)
  (frame-resize-pixelwise t)
  :init
  (when (boundp 'pixel-scroll-precision-mode)
    (pixel-scroll-precision-mode 1))
  (modify-all-frames-parameters config-frame-frame-alist))

;;; Third-party

(use-package ligature
  :straight t
  :hook (after-init . global-ligature-mode)
  :config
  (ligature-set-ligatures
   'prog-mode
   '("www" "**" "***" "**/" "*>" "*/" "\\\\" "\\\\\\" "{-" "::"
     ":::" ":=" "!!" "!=" "!==" "-}" "----" "-->" "->" "->>"
     "-<" "-<<" "-~" "#{" "#[" "##" "###" "####" "#(" "#?" "#_"
     "#_(" ".-" ".=" ".." "..<" "..." "?=" "??" ";;" ";;;" "/*" "/**"
     "/=" "/==" "/>" "//" "///" "&&" "||" "||=" "|=" "|>" "^=" "$>"
     "++" "+++" "+>" "=:=" "==" "===" "==>" "=>" "=>>" "<="
     "=<<" "=/=" ">-" ">=" ">=>" ">>" ">>-" ">>=" ">>>" "<*"
     "<*>" "<|" "<|>" "<$" "<$>" "<!--" "<-" "<--" "<->" "<+"
     "<+>" "<=" "<==" "<=>" "<=<" "<>" "<<" "<<-" "<<=" "<<<"
     "<~" "<~~" "</" "</>" "~@" "~-" "~>" "~~" "~~>" "%%")))

(use-package default-text-scale
  :straight t
  :bind (("s-=" . default-text-scale-increase)
         ("s--" . default-text-scale-decrease)
         ("s-0" . default-text-scale-reset)))

(provide 'config-frame)
;;; config-frame.el ends here
