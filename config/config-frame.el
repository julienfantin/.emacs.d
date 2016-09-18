;;; config-frame.el --- GUI frame configuration      -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Julien Fantin

;; Author: Julien Fantin(require 'use-config) <julienfantin@gmail.com>
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

(defvar config-frame-center t)
(defvar config-frame-width-pct 80)
(defvar config-frame-height-pct 80)
(defvar config-frame-text-scale-step 10)
(defvar config-frame-line-spacing-ratio .38) ; (round (/ (* height config-frame-line-spacing-ratio) 10))

(defvar config-frame-mono-fonts
  '("-*-Consolas-normal-normal-normal-*-*-*-*-*-m-0-iso10646-1"
    "-*-Source Code Pro-normal-normal-normal-*-*-*-*-*-m-0-iso10646-1"
    "-*-DejaVu Sans Mono-normal-normal-normal-*-*-*-*-*-m-0-iso10646-1"
    "-*-Fira Code-normal-normal-normal-*-*-*-*-*-m-0-iso10646-1"
    "-*-Ubuntu Mono-normal-normal-normal-*-*-*-*-*-m-0-iso10646-1"))


;; * Fonts

(defun config-frame-mono-font ()
  "Return the first available font in 'config-frame-mono-fonts'."
  (when (eq 'darwin system-type)
    (cl-find-if #'font-exists-p config-frame-mono-fonts)))

(defun font-exists-p (font)
  "Existing 'FONT' predicate."
  (if (null (x-list-fonts font)) nil t))

(defun config-frame-set-line-spacing ()
  "Set `line-spacing' relative to the default font size."
  (let* ((height (face-attribute 'default :height))
         (value  (/ (* height config-frame-line-spacing-ratio) 10)))
    (setq line-spacing (round value))))


;; * Frame

(defun config-frame-size (width-pct height-pct)
  "Compute a frame size as a list of (width, height) in pixels.

'WIDTH-PCT' and 'HEIGHT-PCT' are integer percentages of the display size."
  (list
   (* (display-pixel-width) (/ width-pct 100.0))
   (* (display-pixel-height) (/ height-pct 100.0))))

(defun config-frame-origin (size)
  "Compute a list of (x, y) as the origin for a frame of 'SIZE'."
  (let ((width (car size))
        (height (cadr size)))
    (list
     (* 0.5 (- (display-pixel-width) width))
     (* 0.5 (- (display-pixel-height) height)))))

(defun config-frame-config ()
  "Return a frame alist compatible with 'initial-frame-alist'."
  (let* ((size (config-frame-size config-frame-width-pct config-frame-height-pct))
         (origin (if config-frame-center (config-frame-origin size) '(0 0))))
    `(;; in pixels
      (left   . ,(round (car origin)))
      (top    . ,(round (cadr origin)))
      ;; in chars
      (width  . ,(round (/ (car size) (frame-char-width))))
      (height . ,(round (/ (cadr size) (frame-char-height)))))))

(use-package frame
  :init
  (let ((config (append
                 (config-frame-config)
                 `((menu-bar-lines       . nil)
                   (tool-bar-lines       . nil)
                   (vertical-scroll-bars . nil)
                   (font                 . ,(config-frame-mono-font))))))
    (setq default-frame-alist config
          initial-frame-alist config))
  :config
  (progn
    (setq frame-resize-pixelwise t)
    (menu-bar-mode -1)
    (scroll-bar-mode -1)
    (tool-bar-mode -1)
    (blink-cursor-mode 1)
    (config-frame-set-line-spacing)))

(use-package mwheel
  :defer t
  :config
  ;; Smooth-ish mouse scrolling
  (setq mouse-wheel-progressive-speed nil
        mouse-wheel-scroll-amount '(1 ((shift) . 1) ((control) . nil))))


;; * Commands

;;;###autoload
(defun -text-scale-increase ()
  "Increase height of default face by 'CONFIG-FRAME-TEXT-SCALE-STEP'."
  (interactive)
  (let ((old-face-attribute (face-attribute 'default :height)))
    (set-face-attribute 'default nil :height (+ old-face-attribute 10))
    (config-frame-set-line-spacing)))

;;;###autoload
(defun -text-scale-decrease ()
  "Decrease height of default face by 'CONFIG-FRAME-TEXT-SCALE-STEP'."
  (interactive)
  (let ((old-face-attribute (face-attribute 'default :height)))
    (set-face-attribute 'default nil :height (- old-face-attribute 10))
    (config-frame-set-line-spacing)))

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
