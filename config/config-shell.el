;;; config-shell.el --- Interactive shells           -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Julien Fantin

;; Author: Julien Fantin <julienfantin@gmail.com>
;; Keywords: tools, unix, terminals

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

;;; Built-ins

(use-package comint
  :custom (comint-prompt-read-only t))

(use-package esh-mode
  :custom
  (eshell-scroll-to-bottom-on-output 'this)
  (eshell-scroll-show-maximum-output t))

(use-package em-unix
  :custom
  (eshell-cp-interactive-query t)
  (eshell-ln-interactive-query t)
  (eshell-mv-interactive-query t)
  (eshell-rm-interactive-query t)
  (eshell-mv-overwrite-files nil))

(use-package em-cmpl
  :after eshell
  :custom (eshell-cmpl-ignore-case t))

(use-package em-term
  :after eshell
  :custom
  (eshell-destroy-buffer-when-process-dies t))

(use-package em-hist
  :after eshell
  :custom (eshell-hist-ignoredups t))

;;; Third-party

(use-package eshell-z
  :straight t
  :after eshell
  :init (require 'eshell-z nil t))

(use-package vterm
  :straight t
  :custom
  (vterm-always-compile-module t)
  (vterm-environment '("VTERM=1")))

(provide 'config-shell)
;;; config-shell.el ends here
