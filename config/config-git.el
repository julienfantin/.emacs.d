;;; config-git.el --- Git version-control            -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Julien Fantin

;; Author: Julien Fantin(require 'use-config) <julienfantin@gmail.com>
;; Keywords: vc

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


;; * Built-ins

(use-package vc
  :defer t
  :config
  ;; Don't refresh remote files
  (validate-setq
   vc-handled-backends '(Git)
   vc-ignore-dir-regexp (format "\\(%s\\)\\|\\(%s\\)"
                                vc-ignore-dir-regexp
                                tramp-file-name-regexp)))

(use-package ediff
  :defer t
  :config
  (validate-setq
   ;; Avoid the crazy multi-frames setup
   ediff-window-setup-function 'ediff-setup-windows-plain
   ;; Ignore whitespace
   ediff-diff-options "-w"
   ;; Counter-intuitve naming here, but windows should be side-by-side...
   ediff-split-window-function 'split-window-horizontally))


;; * Packages

(use-package git-timemachine :ensure t :defer t)

(use-package magit
  :ensure t
  :defer t
  :config
  (validate-setq magit-save-repository-buffers 'dontask))

(use-package magithub
  :ensure t
  :defer t)

(use-package diff-hl
  :ensure t
  :defer t
  :commands diff-hl-mode
  :init
  (progn
    (add-hook 'prog-mode-hook #'diff-hl-mode)
    (add-hook 'dired-mode-hook #'diff-hl-dired-mode))
  :config
  (progn
    (validate-setq diff-hl-draw-borders t)
    (diff-hl-margin-mode 1)))

(provide 'config-git)
;;; config-git.el ends here
