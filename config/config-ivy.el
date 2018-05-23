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

(use-package swiper :ensure t)

(use-package uniquify
  :config
  (setq-default uniquify-buffer-name-style 'forward))

(use-package ivy
  :init (after-init #'ivy-mode)
  :commands (ivy-set-actions)
  :custom
  (ivy-fixed-height-minibuffer nil)
  (ivy-initial-inputs-alist nil)
  (ivy-re-builders-alist '((t . ivy--regex-plus)))
  (ivy-use-virtual-buffers t)
  (ivy-virtual-abbreviate 'full)
  (ivy-display-style nil))

(use-package counsel
  :ensure t
  :init (after-init #'counsel-mode)
  :preface
  :custom
  (counsel-rg-base-command "rg -S -M 200 --no-heading --line-number --color never %s .")
  (ivy-extra-directories nil))

;; Counsel makes use of smex
(use-package smex
  :ensure t
  :after (no-littering)
  :custom (smex-history-length 1000))

(use-package counsel-projectile
  :ensure t
  :after (counsel projectile)
  :init (counsel-projectile-mode))

(use-package ivy-historian
  :ensure t
  :after (ivy no-littering)
  :hook (ivy-mode . ivy-historian-mode)
  :custom (historian-history-length 1000))

;; * Commands


;; NOTE use .dir-locals.el to ignore folders:
;;
;; (counsel-todos-cmd
;;  . "git --no-pager grep --full-name -n --no-color -i -e TODO -e FIXME -e HACK
;; -e XXX -- './*' ':!resources/public/js/**' ':!resources/public/vs/**'")

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

;;  LocalWords:  flx
