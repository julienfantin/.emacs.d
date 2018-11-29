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



;; * Python

(use-package python
  :hook (python-mode . subword-mode)
  :preface
  (defvar config-python-interpreters
    '(("ipython3" "-i --simple-prompt")
      ("python3")
      ("python"))
    "List of '(\"interpreter\" \"args\")")

  (defun config-python-set-interpreter ()
    "Configure `python-shell-interpreter' according to `config-python-interpreters'."
    (when-let
        ((cell (cl-find-if
                (lambda (cell)
                  (executable-find (car cell)))
                config-python-interpreters)))
      (let ((interpreter (car cell))
            (args (cadr cell)))
        (setq python-shell-interpreter interpreter
              python-shell-interpreter-args args))))
  :custom
  (python-shell-interpreter "ipython3")
  (python-shell-interpreter-args "-i --simple-prompt")
  :config
  (add-hook 'python-mode-hook #'config-python-set-interpreter)
  (add-hook 'python-mode-hook (lambda () (set-fill-column 120))))


;; * Environment


(use-package pyvenv
  ;; Set `pyvenv-workon' to the absolute path for the current venv in a .dir-locals.el
  :ensure t
  :hook (python-mode . pyvenv-mode))

(use-package elpy
  :ensure t
  :hook (python-mode . elpy-enable))


;; * Editing

(use-package indent-tools
  :ensure t
  :hook (python-mode . indent-tools-minor-mode)
  :bind (:map python-mode-map ("C-c SPC" . indent-tools-hydra/body)))

(use-package smartparens-python
  :ensure smartparens
  :hook (python-mode . smartparens-mode))

(use-package markdown-mode
  :config
  (add-to-list 'markdown-code-lang-modes '("python" . python-mode)))


;; * Language server protocol

;; ** Eglot

(use-package eglot
  :disabled t
  :ensure t
  :ensure-system-package (pyls . "pip install 'python-language-server[all]' pyls-isort")
  :hook ((python-mode . eglot)))

(use-package flycheck
  :disabled t ;; disable flake8 when using eglot which uses flymake
  :config
  (add-to-list 'flycheck-enabled-checkers 'flycheck-flake8))

;; ** lsp-mode

(defvar config-python-pyls-lsp-mode-config
  '(:pyls
    (:configurationSources
     ["pycodestyle" "pyflakes" "flake8"]
     ;; There's an annoying completion bug in
     ;; Jedi: https://github.com/palantir/python-language-server/issues/432
     :plugins
     (:jedi_completion
      (:enabled nil)

      :rope_completion
      (:enabled t)

      :pyls_mypy
      (:live_mode t)))))

(use-package lsp-mode
  :ensure t
  :ensure-system-package (pyls . "pip install 'python-language-server[all]' pyls-isort")
  :config
  (defun lsp-set-cfg ()
    (when config-python-pyls-lsp-mode-config
      (lsp--set-configuration config-python-pyls-lsp-mode-config)))
  (add-hook 'lsp-after-initialize-hook 'lsp-set-cfg))

(use-package lsp-imenu
  :hook (lsp-after-open . lsp-enable-imenu))

(use-package lsp-ui
  :ensure t
  :after lsp-mode
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  (lsp-ui-sideline-enable nil))

(use-package company-lsp
  :ensure t
  :after (lsp-mode company)
  :hook (lsp-mode . config-python--enable-company-lsp)
  :preface
  (defun config-python--enable-company-lsp ()
    (setq-local company-backends (cons 'company-lsp company-backends))))

(use-package lsp-python
  :ensure t
  :disabled t
  :hook (python-mode . lsp-python-enable))


;; * Debug adapter protocol

(use-package dap-mode
  :ensure t
  :config
  (require 'dap-python))

;;; config-python.el ends here
(provide 'config-python)
