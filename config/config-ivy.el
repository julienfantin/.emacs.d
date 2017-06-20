;;; config-ivy.el --- Ivy completion                 -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Julien Fantin

;; Author: Julien Fantin(require 'use-config) <julienfantin@gmail.com>
;; Keywords: convenience, internal

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

(use-package swiper :ensure t :defer t)

(use-package uniquify
  :config
  (setq-default uniquify-buffer-name-style 'forward))

(use-package ivy
  :init (after-init #'ivy-mode)
  :commands (ivy-set-actions)
  :config
  (progn
    (validate-setq
     ivy-initial-inputs-alist nil
     ivy-re-builders-alist '((t . ivy--regex-ignore-order))
     ivy-use-virtual-buffers t
     ivy-virtual-abbreviate 'full)))

(use-package counsel
  :ensure t
  :init (after-init #'counsel-mode)
  :preface
  (progn
    (defun config-counsel-delete-file (x)
      (delete-file (expand-file-name x ivy--directory)))
    (defun config-counsel-find-file-other-window (x)
      (find-file-other-window (expand-file-name x ivy--directory))))
  :config
  (progn
    (ivy-set-actions
     'counsel-find-file
     `(("x" config-counsel-delete-file ,(propertize "delete" 'face 'font-lock-warning-face))
       ("4" config-counsel-find-file-other-window "other-window")))
    (validate-setq
     counsel-find-file-at-point t
     ivy-extra-directories nil)))

;; Counsel makes use of smex
(use-package smex
  :ensure t
  :defer t
  :init
  (progn
    (defvar smex-history-length 100)
    (defvar smex-save-file (user-var-file "smex"))))

(use-package counsel-projectile
  :ensure t
  :defer t
  :after projectile
  :init (counsel-projectile-on))

(use-package ivy-historian
  :ensure t
  :after ivy
  :config (ivy-historian-mode 1))

;; * Commands

(after 'counsel
  (defvar counsel-git-grep-todos-cmd
    "git --no-pager grep --full-name -n --no-color -e TODO -e FIXME -e HACK -e XXX -e XXXX -e ??? -e FAIL")

  (defun -counsel-git-grep-project-todos ()
    (interactive)
    (let ((default-directory (projectile-project-root)))
      (counsel-git-grep counsel-git-grep-todos-cmd))))

(provide 'config-ivy)
;;; config-ivy.el ends here

;;  LocalWords:  flx
