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
  :defer t
  :init (after-init #'ivy-mode)
  :commands (ivy-set-actions)
  :config
  (progn
    (validate-setq
     ivy-initial-inputs-alist nil
     ivy-re-builders-alist '((t . ivy--regex-plus))
     ivy-use-virtual-buffers t
     ivy-virtual-abbreviate 'full)))

(use-package counsel
  :ensure t
  :defer t
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
     counsel-rg-base-command "rg -i -M 120 --no-heading --line-number --color never '%s'"
     counsel-grep-base-command "rg -i -M 120 --no-heading --line-number --color never '%s' %s"
     counsel-find-file-at-point t
     ivy-extra-directories nil)))

;; Counsel makes use of smex
(use-package smex
  :ensure t
  :defer t
  :after (ivy no-littering)
  :config
  (validate-setq smex-history-length 1000))

(use-package counsel-projectile
  :ensure t
  :defer t
  :after (counsel projectile)
  :init (counsel-projectile-mode))

(use-package ivy-historian
  :ensure t
  :after (ivy no-littering)
  :hook (ivy-mode . ivy-historian-mode)
  :config
  (validate-setq historian-history-length 1000))

;; * Commands


;; NOTE use .dir-locals.el to ignore folders:
;;
;; (counsel-git-grep-todos-cmd
;;  . "git --no-pager grep --full-name -n --no-color -i -e TODO -e FIXME -e HACK
;; -e XXX -- './*' ':!resources/public/js/**' ':!resources/public/vs/**'")

(after 'counsel
  (defvar counsel-git-grep-todos-cmd
    "git --no-pager grep --full-name -n --no-color -e TODO -e FIXME -e HACK -e XXX")

  (defun -counsel-git-grep-project-todos ()
    (interactive)
    (let ((default-directory (projectile-project-root)))
      (counsel-git-grep counsel-git-grep-todos-cmd))))

(provide 'config-ivy)
;;; config-ivy.el ends here

;;  LocalWords:  flx
