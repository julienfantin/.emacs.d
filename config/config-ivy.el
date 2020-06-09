;;; config-ivy.el --- Ivy completion                 -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Julien Fantin

;; Author: Julien Fantin <julienfantin@gmail.com>
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
(require 'use-package)


;; * Packages

(use-package uniquify
  :custom
  (uniquify-buffer-name-style 'forward))

(use-package ivy
  :init (after-init #'ivy-mode)
  :commands (ivy-set-actions)
  :custom
  (ivy-extra-directories nil)
  (ivy-fixed-height-minibuffer t)
  (ivy-initial-inputs-alist nil)
  (ivy-re-builders-alist '((counsel-rg . ivy--regex)))
  (ivy-use-virtual-buffers t))

(use-package magit
  :after ivy
  :custom
  (magit-completing-read-function 'ivy-completing-read))

(use-package counsel
  :straight t
  :init (after-init #'counsel-mode)
  :custom
  (counsel-rg-base-command "rg -S -M 200 --no-heading --line-number --color never %%s .")
  (counsel-find-file-at-point t)
  :config
  (progn
    (defun -counsel-todos  ()
      (interactive)
      (counsel-rg "TODO|FIXME|HACK|XXX" (projectile-project-root) nil "TODOs:"))))

(use-package counsel-projectile
  :straight t
  :after (counsel projectile)
  :init (after-init #'counsel-projectile-mode)
  :custom
  (counsel-projectile-remove-current-project t)
  (counsel-projectile-remove-current-buffer t))

(provide 'config-ivy)
;;; config-ivy.el ends here
