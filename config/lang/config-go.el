;;; config-go.el --- Golang config                   -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Julien Fantin

;; Author: Julien Fantin <julienfantin@gmail.com>
;; Keywords: languages, tools

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:
(require 'use-config)

(use-package go-mode
  :ensure-system-package go
  :straight t)

(use-package eglot
  :straight t
  :ensure-system-package (go-langserver . "go get -u github.com/sourcegraph/go-langserver")
  :after go-mode
  :config
  (add-to-list 'eglot-server-programs '(go-mode . ("go-langserver"))))

(use-package lsp-go
  :straight t
  :ensure-system-package (go-langserver . "go get -u github.com/sourcegraph/go-langserver")
  :after go-mode)

(provide 'config-go)
;;; config-go.el ends here
