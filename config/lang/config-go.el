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

(require 'use-package)
(require 'config-lsp)

(defvar config-go-test-map (make-sparse-keymap)
  "Keymap for go test commands.")

;;; Third-party

(use-package go-mode
  :straight t
  :ensure-system-package "go")

(use-package go-test
  :straight t
  :bind (:map config-go-test-map
              ("t" . go-test-current-test)
              ("f" . go-test-current-file)
              ("p" . go-test-current-project)
              ("c" . go-test-current-coverage)
              ("b" . go-test-current-test-benchmark)))

(use-package gopls
  :if config-lsp-frontend
  :ensure-system-package "gopls")

(use-package eglot
  :if (equal config-lsp-frontend 'eglot)
  :after go-mode
  :hook (go-mode . eglot-ensure))

(use-package lsp-mode
  :straight t
  :if (equal config-lsp-frontend 'lsp-mode)
  :after go-mode
  :hook (go-mode . lsp))

(use-package dap-mode
  :straight t
  :if (equal config-lsp-frontend 'lsp-mode)
  :after go-mode
  :config
  (require dap-dlv-go))

(provide 'config-go)
;;; config-go.el ends here
