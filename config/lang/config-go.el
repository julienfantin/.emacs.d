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
  :ensure-system-package go
  :hook (go-mode . subword-mode)
  :custom
  (tab-width 4))


(use-package gotest
  :straight t
  :bind-keymap ("C-c t" . config-go-test-map)
  :bind (:map config-go-test-map
              ("t" . go-test-current-test)
              ("f" . go-test-current-file)
              ("p" . go-test-current-project)
              ("c" . go-test-current-coverage)
              ("b" . go-test-current-test-benchmark)))

(use-package super-save
  :config
  (add-to-list 'super-save-predicates
               (lambda () (not (eq major-mode 'go-mode)))))

(use-package go-gen-test
  :after go-mode
  :straight t
  :bind (:map config-go-test-map
              ("g" . go-gen-test-dwim))
  :ensure-system-package (gotests . "go get -u github.com/cweill/gotests/..."))

(use-package eglot
  :if (equal config-lsp-frontend 'eglot)
  :after go-mode
  :hook (go-mode . eglot-ensure))

(use-package lsp-mode
  :straight t
  :if (eq config-lsp-frontend 'lsp-mode)
  :after go-mode
  :hook (go-mode . lsp))

(use-package lsp-go
  :if (eq config-lsp-frontend 'lsp-mode)
  :ensure-system-package gopls
  :after (lsp-mode go-mode)
  :demand t
  :custom
  (lsp-go-hover-kind "FullDocumentation")
  (lsp-go-analyses '((fieldalignment . t)
                     (nilness . t)
                     (unusedparams . t)
                     (unusedvariable . t)
                     (unusedwrite . t))))

(use-package dap-go
  :if (eq config-lsp-frontend 'lsp-mode)
  :ensure-system-package (dlv . "go install github.com/go-delve/delve/cmd/dlv@latest")
  :after (go-mode lsp-mode dap-mode)
  :init (require 'dap-dlv-go))

(provide 'config-go)
;;; config-go.el ends here
