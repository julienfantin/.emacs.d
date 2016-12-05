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

(use-package ivy
  :init (after-init #'ivy-mode)
  :commands (ivy-set-actions)
  :config
  (progn
    (validate-setq
     ivy-initial-inputs-alist nil
     ivy-re-builders-alist '((t . ivy--regex-plus))
     ivy-use-virtual-buffers t)))

(use-package counsel
  :ensure t
  :init (after-init #'counsel-mode)
  :preface
  (defun config-counsel-delete-file (x)
    (delete-file (expand-file-name x ivy--directory)))
  :config
  (progn
    (defcustom counsel-rg-base-command "rg --vimgrep --no-heading %s"
      "Format string to use in `counsel-ag-function' to construct the
command. The %s will be replaced by optional extra ag arguments followed by the
regex string. The default is \"ag --nocolor --nogroup %s\"."
      :type 'string
      :group 'ivy)

    (defun counsel-rg (&optional initial-input)
      "Grep for a string in the current directory using ripgrep.
This uses `counsel-ag' with `counsel-rg-base-command' replacing
`counsel-ag-base-command'."
      (interactive)
      (let ((counsel-ag-base-command counsel-rg-base-command)
            (proj (or (projectile-project-p)
                      (locate-dominating-file default-directory ".git"))))
        (counsel-ag initial-input proj)))

    (ivy-set-actions
     'counsel-find-file
     `(("x" #'config-counsel-delete-file ,(propertize "delete" 'face 'font-lock-warning-face))))
    (validate-setq counsel-find-file-at-point t
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

(provide 'config-ivy)
;;; config-ivy.el ends here

;;  LocalWords:  flx
