;;; config-project.el --- Project management         -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Julien Fantin

;; Author: Julien Fantin <julienfantin@gmail.com>
;; Keywords: convenience, tools

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
(require 'config-path)

;; * Direnv

(use-package direnv
  :disabled t
  :ensure-system-package direnv
  :straight t
  :init (after-init #'direnv-mode))

;; * Editorconfig

(use-package editorconfig
  :straight t
  :commands (editorconfig-mode)
  :init (after-init 'editorconfig-mode))

;; * Projectile

(use-package projectile
  :straight t
  :after no-littering
  :init (after-init 'projectile-global-mode)
  :commands projectile-golbal-mode
  :functions (projectile-load-known-projects)
  :hook (projectile-idle-timer . projectile-invalidate-cache)
  :config
  (progn
    (add-to-list 'projectile-globally-ignored-files ".DS_Store")
    (add-to-list 'projectile-globally-ignored-directories "elpa")
    (advice-add #'projectile-replace :before #'projectile-save-project-buffers)
    (projectile-load-known-projects))
  :custom
  (projectile-mode-line nil)
  (projectile-enable-caching nil)
  (projectile-use-git-grep t)
  (projectile-create-missing-test-files t))

(provide 'config-project)
;;; config-project.el ends here
