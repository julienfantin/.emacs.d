;;; config-search.el --- Search in files and buffers  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Julien Fantin

;; Author: Julien Fantin <julienfantin@gmail.com>
;; Keywords: convenience, files, matching, tools

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

;;; Built-ins

(use-package isearch
  :custom
  (isearch-allow-scroll t)
  (lazy-highlight-initial-delay 0)
  (isearch-invisible 'open))

(use-package imenu
  :bind ("C-c i" . imenu)
  :custom
  (imenu-auto-rescan t)
  (imenu-max-item-length 100))

;;; Third-party

(use-package anzu
  :straight t
  :hook (after-init . global-anzu-mode)
  :bind
  (([remap query-replace] . anzu-query-replace)
   ([remap query-replace-regexp] . anzu-query-replace-regexp)
   ([remap isearch-query-replace] . anzu-isearch-query-replace)
   ([remap isearch-query-replace-regexp] . anzu-isearch-query-replace-regexp)))

(use-package imenu-anywhere
  :straight t
  :bind ("C-c I" . imenu-anywhere))

(use-package flimenu
  :straight t
  :after imenu
  :init (flimenu-global-mode))

(use-package imenu-list
  :straight t
  :bind ("C-c M-i" . imenu-list-smart-toggle)
  :hook (imenu-list-major-mode . toggle-truncate-lines)
  :custom
  (imenu-list-focus-after-activation t)
  (imenu-list-auto-resize t)
  (imenu-list-mode-line-format nil))

(use-package avy
  :straight t
  :custom
  (avy-style 'at-full)
  (avy-all-windows t)
  (avy-timeout-seconds 0.28)
  :config
  ;; Use C-' in isearch to bring up avy
  (avy-setup-default))

(use-package deadgrep
  :ensure-system-package (rg . ripgrep)
  :straight t
  :bind (("C-c r" . deadgrep)
         :map deadgrep-mode-map
         ("w" . deadgrep-edit-mode)
         :map deadgrep-edit-mode-map
         ("C-c C-c" . deadgrep-mode)
         ("C-x C-s" . deadgrep-mode)))

(provide 'config-search)
;;; config-search.el ends here
