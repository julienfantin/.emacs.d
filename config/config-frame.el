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
(require 'use-config)

;; * Customs

(defvar config-frame-border-width 15)

(defvar config-frame-mono-fonts
  '("-*-Menlo-normal-normal-normal-*-*-*-*-*-m-0-iso10646-1"
    "-*-Fira Code-normal-normal-normal-*-*-*-*-*-m-0-iso10646-1"
    "-*-DejaVu Sans Mono-normal-normal-normal-*-*-*-*-*-m-0-iso10646-1"
    "-*-Source Code Pro-normal-normal-normal-*-*-*-*-*-m-0-iso10646-1"
    "-*-IBM Plex Mono-normal-normal-normal-*-*-*-*-*-m-0-iso10646-1"
    "-*-Iosevka-normal-normal-normal-*-*-*-*-*-m-0-iso10646-1"
    "-*-SF Mono-normal-normal-ultracondensed-*-*-*-*-*-m-0-iso10646-1"))

(defvar config-frame-fonts
  '("-*-SF UI Text-normal-normal-normal-*-*-*-*-*-p-0-iso10646-1"))


;; * Fonts

(defun font-exists-p (font)
  "Existing 'FONT' predicate."
  (if (null (x-list-fonts font)) nil t))

(defun config-frame-mono-font ()
  (cl-find-if #'font-exists-p config-frame-mono-fonts))

(defun config-frame-font ()
  (cl-find-if #'font-exists-p config-frame-fonts))

(when-let ((font (config-frame-mono-font)))
  (set-face-attribute 'default nil :font font))

(when-let ((font (config-frame-font)))
  (set-face-attribute 'variable-pitch nil :font font))


;; * Frame

(defvar config-frame-default-frame-alist
  `((menu-bar-lines        . nil)
    (tool-bar-lines        . nil)
    (vertical-scroll-bars  . nil)
    (scroll-bars           . nil)
    (internal-border-width . ,config-frame-border-width)))

(defun config-frame-frame-alist ()
  "Compute the default and initial frame alist."
  (append
   (when window-system
     (let* ((width (if (> (nth 2 (frame-monitor-geometry)) 1920) 240 120))
            (height (nth 3 (frame-monitor-geometry)))
            (margin (/ (- (nth 2 (frame-monitor-geometry)) (* width (frame-char-width))) 2)))
       `((width  . ,width)
         (height . ,height)
         (left   . ,margin)
         (top    . 0))))
   config-frame-default-frame-alist))

(use-package frame
  ;; :init (after-init #'window-divider-mode)
  :config
  (progn
    (scroll-bar-mode -1)
    (tool-bar-mode -1)
    (blink-cursor-mode -1)
    (pixel-scroll-mode 1))
  :custom
  (default-frame-alist (config-frame-frame-alist))
  (initial-frame-alist (config-frame-frame-alist))
  (frame-resize-pixelwise t)
  ;; The window dividers give some nice padding to the layout but look ugly in
  ;; themes where the face is different from the default background
  (window-divider-default-places t)
  (window-divider-default-right-width config-frame-border-width)
  (window-divider-default-bottom-width 10))

(use-package mwheel
  :custom
  ;; Use the trackpade to scroll the buffer horizontally
  (mouse-wheel-flip-direction t)
  (mouse-wheel-tilt-scroll t))

(when (eq 'darwin system-type)
  ;; Enable standard retina font rendering
  (setq ns-use-thin-smoothing t))


;; * Commands

;;;###autoload
(defun -text-scale-increase ()
  "Increase height of default face."
  (interactive)
  (let ((height (face-attribute 'default :height)))
    (set-face-attribute 'default nil :height (+ height 10))))

(global-set-key [remap text-scale-increase] #'-text-scale-increase)

;;;###autoload
(defun -text-scale-decrease ()
  "Decrease height of default face."
  (interactive)
  (let ((height (face-attribute 'default :height)))
    (set-face-attribute 'default nil :height (- height 10))))

(global-set-key [remap text-scale-decrease] #'-text-scale-decrease)

;;;###autoload
(defun -transparency-increase ()
  "Increase frame transparence."
  (interactive)
  (let ((alpha (or (car (frame-parameter (selected-frame) 'alpha)) 100)))
    (set-frame-parameter (selected-frame) 'alpha (list  (- alpha 5)  (- alpha 5)))))

;;;###autoload
(defun -transparency-decrease ()
  "Decrease frame trnasparency."
  (interactive)
  (let ((alpha (or (car (frame-parameter (selected-frame) 'alpha)) 100)))
    (set-frame-parameter (selected-frame) 'alpha (list  (+ alpha 5)  (+ alpha 5)))))

(provide 'config-frame)
;;; config-frame.el ends here
