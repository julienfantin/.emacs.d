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


;; * Builtins

(use-package isearch
  :commands (isearch-forward-symbol-at-point isearch-forward)
  :preface
  (defun config-search-isearch-symbol-with-prefix (p)
    "Like isearch, unless prefix argument is provided.
With a prefix argument P, isearch for the symbol at point."
    (interactive "P")
    (let ((current-prefix-arg nil))
      (call-interactively
       (if p #'isearch-forward-symbol-at-point
         #'isearch-forward))))
  :custom
  (isearch-allow-scroll t)
  (lazy-highlight-initial-delay 0)
  (isearch-invisible 'open))


;; * Packages

(use-package imenu-anywhere
  :straight t
  :bind
  ("C-c i" . imenu-anywhere)
  ("C-c I" . imenu))

(use-package flimenu
  :straight t
  :init (flimenu-global-mode))

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
