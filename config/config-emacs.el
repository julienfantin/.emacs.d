;;; config-emacs.el --- Emacs defaults            -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Julien Fantin

;; Author: Julien Fantin <julienfantin@gmail.com>
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
(require 'use-package)

(use-package comp
  :custom
  (native-comp-async-report-warnings-errors nil))

(use-package startup
  :custom
  (initial-scratch-message "")
  (inhibit-startup-message t))

(use-package emacs
  :custom
  (bidi-inhibit-bpa t)
  (bidi-redisplay-reordering nil)
  (y-or-n-p-use-read-key t)
  (scroll-conservatively 101)
  (scroll-margin 3)
  (ring-bell-function 'ignore)
  (default-process-coding-system '(utf-8 . utf-8))
  (buffer-file-coding-system 'utf-8))

(use-package gcmh
  :straight t
  :hook ((after-init . gcmh-mode)
         (focus-out . garbage-collect))
  ;; Config taken from doom
  :custom
  (gcmh-idle-delay 'auto)
  (gcmh-auto-idle-delay-factor 10)
  (gcmh-high-cons-threshold (* 16 1024 1024))
  :preface
  (defvar config-emacs--default-read-process-output-max nil)
  (defvar config-emacs--default-gcmh-high-cons-threshold nil)
  (defvar config-emacs--optimization-init-p nil)

  (define-minor-mode config-emacs-optimization-mode
    "Deploys universal GC and IPC optimizations for `lsp-mode' and `eglot'."
    :global t
    :init-value nil
    (if (not config-emacs-optimization-mode)
        (setq-default read-process-output-max config-emacs--default-read-process-output-max
                      gcmh-high-cons-threshold config-emacs--default-gcmh-high-cons-threshold
                      config-emacs--optimization-init-p nil)
      (unless config-emacs--optimization-init-p
        (setq config-emacs--default-read-process-output-max (default-value 'read-process-output-max)
              config-emacs--default-gcmh-high-cons-threshold (default-value 'gcmh-high-cons-threshold))
        (setq-default read-process-output-max (* 1024 1024))
        (setq-default gcmh-high-cons-threshold (* 2 config-emacs--default-gcmh-high-cons-threshold))
        (gcmh-set-high-threshold)
        (setq config-emacs--optimization-init-p t))))
  (with-eval-after-load "lsp-mode"
    (add-hook 'lsp-mode-hook #'config-emacs-optimization-mode)) )

(provide 'config-emacs)
;;; config-emacs.el ends here
