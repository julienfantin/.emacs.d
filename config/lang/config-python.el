 ;;; config-python.el --- Python config with LSP      -*- lexical-binding: t; -*-

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

(use-package eglot
  :disabled t
  :ensure t
  :ensure-system-package (pyls . "pip install 'python-language-server[all]' pyls-isort")
  :hook ((python-mode . eglot-ensure)))

(use-package flycheck
  ;;  eglot uses flymake
  :disabled t
  :config
  (add-to-list 'flycheck-enabled-checkers 'flycheck-flake8))


(use-package python
  :hook (python-mode . subword-mode)
  :custom
  (python-shell-interpreter "python3")
  :config
  (add-hook 'python-mode-hook (lambda () (set-fill-column 120))))

(use-package pyenv-mode :ensure t)

(use-package lsp-mode
  :ensure t
  :config
  (defun lsp-set-cfg ()
    (let ((lsp-cfg `(:pyls (:configurationSources ["flake8"]))))
      (lsp--set-configuration lsp-cfg)))
  (add-hook 'lsp-after-initialize-hook 'lsp-set-cfg))

(use-package lsp-imenu
  :hook (lsp-after-open . lsp-enable-imenu))

(use-package lsp-ui
  :ensure t
  :after lsp-mode
  :hook (lsp-mode . lsp-ui-mode))

(use-package company-lsp
  :ensure t
  :after (lsp-mode company)
  :hook (lsp-mode . config-python--enable-company-lsp)
  :preface
  (defun config-python--enable-company-lsp ()
    (setq-local company-backends (cons 'company-lsp company-backends))))

(use-package lsp-python
  :ensure t
  :hook (python-mode . lsp-python-enable))

(use-package dap-mode
  :ensure t
  :config
  (require 'dap-python))

(use-package aggressive-indent-mode
  :config
  (add-to-list 'aggressive-indent-excluded-modes 'python-mode))

(use-package indent-tools
  :ensure t
  :hook (python-mode . indent-tools-minor-mode))

(use-package smartparens-python
  :ensure smartparens
  :hook (python-mode . smartparens-mode))

(provide 'config-python)
;;; config-python.el ends here
