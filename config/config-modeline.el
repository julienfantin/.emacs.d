;;; config-modeline.el --- Modeline config           -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Julien Fantin

;; Author: Julien Fantin <julienfantin@gmail.com>
;; Keywords: faces

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
(require 'use-config)

;; * Packages

(defvar config-modeline-eyebrowse-mode-line
  '(:eval
    (when (bound-and-true-p eyebrowse-mode)
      (let* ((num (eyebrowse--get 'current-slot))
             (tag (when num (nth 2 (assoc num (eyebrowse--get 'window-configs)))))
             (tag-or-num (if (and tag (< 0 (length tag))) tag (when num (int-to-string num))))
             (str (concat " " tag-or-num " ")))
        str)))
  "Mode line format for Eyebrowse.")
(put 'config-modeline-eyebrowse-mode-line 'risky-local-variable t)

(defvar config-modeline-window-mode-line
  '(:eval
    (when (featurep 'ace-window)
      (when-let ((pos (cl-position (selected-window) (aw-window-list)))
                 (key (nth pos aw-keys)))
        (char-to-string key)))))
(put 'config-modeline-window-mode-line 'risky-local-variable t)

(setq-default
 mode-line-format
 '("%e"
   config-modeline-eyebrowse-mode-line
   config-modeline-window-mode-line
   ;; mode-line-front-space
   ;; mode-line-mule-info
   ;; mode-line-client
   ;; mode-line-modified
   ;; mode-line-remote
   mode-line-frame-identification
   mode-line-buffer-identification "   " mode-line-position
   (vc-mode vc-mode)
   (multiple-cursors-mode mc/mode-line)
   "  " mode-line-modes
   mode-line-misc-info
   mode-line-end-spaces))

(use-package moody
  :ensure t
  :init
  (moody-replace-mode-line-buffer-identification)
  (moody-replace-vc-mode)
  :custom
  (x-underline-at-descent-line t)
  (moody-slant-function #'moody-slant-apple-rgb))

(use-package rich-minority
  :ensure t
  :init (rich-minority-mode 1)
  :custom (rm-whitelist '(" λ")))

(provide 'config-modeline)
;;; config-modeline.el ends here
