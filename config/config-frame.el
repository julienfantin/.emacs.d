;;; config-frame.el --- GUI frame configuration      -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Julien Fantin

;; Author: Julien Fantin <julienfantin@gmail.com>
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

(require 'use-package)

;;; Config

(defvar config-frame-border-width 18)

(defvar config-frame-mono-fonts
  '("DejaVu Sans Mono" "Menlo" "Fira Code" "SF Mono" "IBM Plex Mono" "Source Code Pro"))

(defvar config-frame-variable-fonts
  '("DejaVu Sans" "ETBembo"))

(defvar config-frame-default-mono-font-height 120)

(defvar config-frame-default-variable-font-height 130)

(defvar config-frame-frame-alist
  `((fullscreen              . maximized)
    (menu-bar-lines          . nil)
    (tool-bar-lines          . nil)
    (vertical-scroll-bars    . nil)
    (scroll-bars             . nil)
    (ns-transparent-titlebar . t)
    (internal-border-width   . ,config-frame-border-width))
  "The default and initial frame alist.")

(defun config-frame-font-exists-p (font)
  "Existing 'FONT' predicate."
  (if (null (x-list-fonts font)) nil t))

(defun config-frame-mono-font ()
  (cl-find-if #'config-frame-font-exists-p config-frame-mono-fonts))

(defun config-frame-variable-font ()
  (cl-find-if #'config-frame-font-exists-p config-frame-variable-fonts))

(when-let ((font (config-frame-mono-font)))
  (dolist (face '(default fixed-pitch))
    (set-face-attribute face nil :font font :height config-frame-default-mono-font-height)))

(when-let ((font (config-frame-variable-font)))
  (set-face-attribute 'variable-pitch nil :font font :height config-frame-default-variable-font-height))

;;; Built-ins

(use-package emacs
  :custom
  (default-text-properties '(line-spacing 0.25 line-height 1.25))
  (default-truncate-lines t)
  (cursor-type '(bar . 1))
  (x-underline-at-descent-line t))


(use-package frame
  :custom
  (default-frame-alist config-frame-frame-alist)
  (initial-frame-alist config-frame-frame-alist)
  (frame-resize-pixelwise t)
  (window-divider-default-places 'right-only)
  (window-divider-default-right-width 1)
  :config
  (modify-frame-parameters (selected-frame) config-frame-frame-alist)
  (set-face-attribute 'window-divider nil :foreground (face-background 'mode-line))
  (set-face-attribute 'window-divider-first-pixel nil :foreground (face-background 'default))
  (set-face-attribute 'window-divider-last-pixel nil :foreground (face-background 'default)))

(use-package mwheel
  :custom
  (mouse-wheel-flip-direction t)
  ;; Use the trackpad to scroll the buffer horizontally
  (mouse-wheel-tilt-scroll t))

(use-package default-text-scale
  :straight t
  :bind (("s-=" . default-text-scale-increase)
         ("s--" . default-text-scale-decrease)
         ("s-0" . default-text-scale-reset)))

(provide 'config-frame)
;;; config-frame.el ends here
