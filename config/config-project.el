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

;;; Third-party

(use-package direnv
  :disabled t
  :ensure-system-package direnv
  :straight t
  :hook (after-init . direnv-mode))

(use-package editorconfig
  :disabled t
  :straight t
  :hook (after-init . editorconfig-mode))

(use-package projectile
  :straight t
  :after no-littering
  :bind ("C-c p" . projectile-command-map)
  :hook (after-init . projectile-global-mode)
  :commands projectile-golbal-mode
  :functions (projectile-load-known-projects)
  :config
  (progn
    (add-to-list 'projectile-globally-ignored-files ".DS_Store")
    (add-to-list 'projectile-globally-ignored-directories "elpa")
    (advice-add #'projectile-replace :before #'projectile-save-project-buffers)
    (projectile-load-known-projects))
  :custom
  (projectile-completion-system 'default)
  (projectile-mode-line nil)
  (projectile-enable-caching nil)
  (projectile-create-missing-test-files t))

(use-package counsel-projectile
  :if (equal config-completion-system 'ivy)
  :straight t
  :after (counsel projectile)
  :hook ((after-init . counsel-projectile-mode))
  :custom
  (counsel-projectile-remove-current-project t)
  (counsel-projectile-remove-current-buffer t))

(provide 'config-project)
;;; config-project.el ends here
