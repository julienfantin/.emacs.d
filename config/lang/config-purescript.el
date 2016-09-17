;;; config-purescript.el --- PureScript config       -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Julien Fantin

;; Author: Julien Fantin <julienfantin@gmail.com>
;; Keywords: languages

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

(use-package purescript-mode :ensure t :defer t)

(use-package psc-ide
  :ensure t
  :defer t
  :after purescript-mode
  :init
  (progn
    (add-hook 'purescript-mode-hook 'psc-ide-mode)
    (add-hook 'purescript-mode-hook 'turn-on-purescript-indentation)))

(use-package psci
  :ensure t
  :defer t
  :after purescript-mode
  :config
  (add-hook 'purescript-mode-hook 'inferior-psci-mode))

(use-package repl-toggle
  :ensure t
  :defer t
  :after psci
  :config
  (add-to-list 'rtog/mode-repl-alist '(purescript-mode . psci)))

(provide 'config-purescript)
;;; config-purescript.el ends here
