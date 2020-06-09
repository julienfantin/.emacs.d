;;; config-outlines.el --- Outlines in source files  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Julien Fantin

;; Author: Julien Fantin <julienfantin@gmail.com>
;; Keywords: outlines, languages, convenience

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


;; * Built-ins

(use-package outline
  :hook ((prog-mode . outline-minor-mode)
         (text-mode . outline-minor-mode)))


;; * Packages

(use-package outshine
  :straight t
  :commands
  (outshine-hook-function outshine-insert-heading)
  :hook (outline-minor-mode . config-outline-outshine-enable)
  :preface
  (defun config-outline-outshine-enable ()
    (outshine-hook-function)
    ;; With this config and the lispy remapping, navigating with n, p in lispy
    ;; special will treat outlines as part of the normal
    (when (bound-and-true-p lispy-outline)
      (set (make-local-variable 'lispy-outline) (outshine-calc-outline-regexp))))
  (defun config-outline-lispy-compat ()
    (when outline-minor-mode
      (outline-minor-mode -1)
      (outline-minor-mode 1)))
  (defun -swiper-outlines ()
    (interactive)
    (swiper (outshine-calc-outline-regexp)))
  :config
  (after 'lispy
    (add-hook 'lispy-mode-hook 'config-outline-lispy-compat))
  :custom
  (outshine-fontify-whole-heading-line t)
  (outline-cycle-emulate-tab t))

(use-package outline-magic :straight t)

(provide 'config-outlines)
;;; config-outlines.el ends here
