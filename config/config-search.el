;;; config-search.el --- Search in files and buffers  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Julien Fantin

;; Author: Julien Fantin(require 'use-config) <julienfantin@gmail.com>
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
(require 'use-config)


;; * Builtins

(use-package isearch
  :defer t
  :commands (isearch-forward-symbol-at-point isearch-forward)
  :init
  (progn
    ;; Reveal content of subtrees during isearch, alse see reveal-mode
    (setq-default isearch-invisible 'open)
    (validate-setq isearch-allow-scroll t
                   lazy-highlight-initial-delay 0))

  :config
  (bind-keys
   :map isearch-mode-map
   ;; Allow deleting chars in the search string, use C-r to search backwards
   ([remap isearch-delete-char] . isearch-del-char)))


;; * Packages

(use-package anzu :ensure t :init (global-anzu-mode))
(use-package imenu-anywhere :ensure t :defer t)
(use-package link-hint :ensure t :defer t)
(use-package goto-last-change :ensure t :commands goto-last-change :defer t)
(use-package swiper
  :ensure t
  :defer t
  :config
  (advice-add 'swiper :after #'recenter-top-bottom))

(use-package avy
  :ensure t
  :config
  (progn
    (validate-setq
     avy-style 'at-full
     avy-background t
     avy-all-windows t
     avy-timeout-seconds 0.28)
    ;; Use C-' in isearch to bring up avy
    (avy-setup-default)))


;; * Commands

;;;###autoload
(defun -swiper-at-point (_arg)
  "Swiper with 'thing-at-point'."
  (interactive "P")
  (swiper (thing-at-point 'symbol)))

(provide 'config-search)
;;; config-search.el ends here
