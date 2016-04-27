;;; config-defaults.el --- Emacs defaults            -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Julien Fantin

;; Author: Julien Fantin(require 'use-config) <julienfantin@gmail.com>
;; Keywords: internal

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


;; * Garbage collection
;;
;; GC Threshold is temporarily increased during initialization as well as when
;; using the mini-buffer. The latter seemed to help with Helm.

(defvar config-defaults-gc-cons-threshold (* 80 1024 1024))

(defun config-defaults-gc-up ()
  "Increase the GC threshold."
  (setq gc-cons-threshold (* 100 config-defaults-gc-cons-threshold)))

(defun config-defaults-gc-max ()
  "Maximize the GC threshold."
  (setq gc-cons-threshold most-positive-fixnum))

(defun config-defaults-gc-default ()
  "Reset default GC threshold."
  (setq gc-cons-threshold config-defaults-gc-cons-threshold))

(progn
  (add-hook 'minibuffer-setup-hook #'config-defaults-gc-up)
  (add-hook 'minibuffer-exit-hook #'config-defaults-gc-default)
  (config-defaults-gc-max)
  (after-init #'config-defaults-gc-default))


;; * Misc
;; ** Scolling

(setq scroll-step 0
      scroll-margin 0
      scroll-conservatively 10000
      scroll-preserve-screen-position 'always)

;; ** Initial state

(setq initial-scratch-message ""
      inhibit-startup-message t)

(setq echo-keystrokes 0.1
      ring-bell-function 'ignore)

;; ** Files encoding

(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(setq default-process-coding-system '(utf-8 . utf-8))
(setq-default buffer-file-coding-system 'utf-8)

;; ** Windows handling

(setq-default cursor-in-non-selected-windows nil)

(fset 'yes-or-no-p 'y-or-n-p)

(provide 'config-defaults)
;;; config-defaults.el ends here
