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
  (ivy-use-virtual-buffers t)
  :config
  (after 'magit
    (setq magit-completing-read-function 'ivy-completing-read))
  (after 'projectile
    (setq projectile-completion-system 'ivy)))

(use-package counsel
  :straight t
  :init (after-init #'counsel-mode)
  :custom
  (counsel-rg-base-command "rg -S -M 200 --no-heading --line-number --color never %%s .")
  (counsel-find-file-at-point t))

(use-package counsel-projectile
  :straight t
  :after (counsel projectile)
  :init (after-init #'counsel-projectile-mode)
  :custom
  (counsel-projectile-remove-current-project t)
  (counsel-projectile-remove-current-buffer t))

(use-package ivy-prescient
  :straight t
  :after ivy
  :init (after-init #'ivy-prescient-mode))

(use-package ivy-rich
  :straight t
  :after ivy
  :init (after-init #'ivy-rich-mode))

;; * Commands

(after 'counsel
  (defun -counsel-todos  ()
    (interactive)
    (counsel-rg "TODO|FIXME|HACK|XXX" (projectile-project-root) nil "TODOs:"))

  (defun -counsel-git-grep-project-history ()
    (interactive)
    (let ((default-directory (projectile-project-root)))
      (counsel-git-grep (concat "git log -p -S%s --no-pager --no-color -- " default-directory)))))

(provide 'config-ivy)
;;; config-ivy.el ends here
