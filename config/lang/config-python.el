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

(defvar config-python-lsp-frontend 'lsp-mode)
(defvar config-python-lsp-backend 'ms-python)


;; * Python

(use-package python
  :hook (python-mode . subword-mode)
  :preface
  (defvar config-python-interpreters
    '(("ipython" "-i --simple-prompt")
      ("python3" "-i")
      ("python"))
    "List of '(\"interpreter\" \"args\")")
  (defun config-python-set-interpreter ()
    "Configure `python-shell-interpreter' according to `config-python-interpreters'."
    (if-let ((cell (cl-find-if
                    (lambda (cell)
                      (executable-find (car cell)))
                    config-python-interpreters)))
        (setq python-shell-interpreter (car cell)
              python-shell-interpreter-args (cadr cell))
      (error "Interpreter not found")))
  :config
  (add-hook 'python-mode-hook #'config-python-set-interpreter)
  (add-hook 'python-mode-hook (lambda () (set-fill-column 120))))

(use-package pyvenv
  ;; Set `pyvenv-workon' to the absolute path for the current venv in a .dir-locals.el
  :ensure t
  :hook (python-mode . pyvenv-mode)
  :config
  (add-hook 'pyvenv-post-create-hooks #'config-python-set-interpreter))

(use-package python-docstring
  :ensure t
  :after python
  :bind
  (:map python-mode-map
        ([remap fill-paragraph] . python-docstring-fill)))


;; * Environment

(use-package flycheck
  :after elpy
  :hook (elpy-mode . flycheck-mode)
  :config
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules)))

;; pip install elpy rope jedi autopep8 yapf flake8 isort importmagic epc autoflake

(use-package elpy
  :ensure t
  :hook (python-mode . elpy-enable)
  :custom
  (elpy-get-info-from-shell t))

(use-package isortify
  :ensure t
  :hook (python-mode . isortify-mode))

(use-package py-autopep8
  :ensure t
  :hook (python-mode . py-autopep8-enable-on-save))

(use-package importmagic
  :ensure t
  :hook (python-mode . importmagic-mode)
  :config
  (after isortify
    (defun config-python--after-importmagic (&rest _)
      (let ((python-shell-interpreter "python")
            (python-shell-interpreter-args "-i"))
        (unwind-protect
            (isortify-buffer))))
    (advice-add #'importmagic-fix-symbol-at-point :after #'config-python--after-importmagic)
    (advice-add #'importmagic-fix-imports :after #'config-python--after-importmagic)))

(defvar config-python-autoflake-before-save-enabled nil)

(defun config-python-autoflake ()
  "Use Autoflake to remove unused function.

$ autoflake --remove-all-unused-imports -i unused_imports.py"
  (interactive)
  (when (eq major-mode 'python-mode)
    (when-let ((autoflake (executable-find "autoflake")))
      (basic-save-buffer)
      (shell-command
       (format "%s --remove-unused-variables --remove-all-unused-imports -i %s"
	       autoflake
	       (shell-quote-argument (buffer-file-name)))))
    (revert-buffer t t t))
  nil)

(defun config-python-autoflake-before-save ()
  "Run autoflake if `config-python-autoflake-before-save-enabled' is true."
  (when config-python-autoflake-before-save-enabled
    (config-python-autoflake)))

(defun config-python-autoflake-turn-on ()
  "Register a buffer local `before-save-hook' for `config-python-autoflake-before-save'."
  (add-hook 'before-save-hook #'config-python-autoflake-before-save nil t))

(after python
  (add-hook 'before-save-hook 'config-python-autoflake-turn-on))

(use-package pytest
  :ensure t
  :config
  (defun config-python-pytest-integration ()
    (setq pytest-global-name "onepy"
          pytest-project-root-test (lambda (dirname) (equal dirname "/code/lastmile")))))

(use-package python-pytest
    :ensure t
    :config
    (defun python-pytest--project-root ()
      "/code/lastmile")
    :custom
    (python-pytest-executable "onepytest"))


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

;; Disable default flycheck checkers
(use-package flycheck
  :config
  (add-to-list 'flycheck-enabled-checkers 'flycheck-flake8)
  (add-to-list 'flycheck-disabled-checkers 'python-pylint))

;; ** Eglot

(use-package eglot
  :if (eq config-python-lsp-frontend 'eglot)
  :ensure t
  :ensure-system-package (pyls . "pip install 'python-language-server[all]' pyls-isort")
  :hook ((python-mode . eglot-ensure)))

;; ** lsp-mode

(use-package ms-python
  :if (and (eq config-python-lsp-frontend 'lsp-mode)
           (eq config-python-lsp-backend 'ms-python))
  :ensure t
  :init (require 'ms-python)
  :config (add-hook 'python-mode-hook 'lsp t)
  :custom
  (ms-python-server-install-dir (no-littering-expand-etc-file-name "ms-python/server/"))
  (ms-python-dotnet-install-dir (no-littering-expand-etc-file-name "ms-python/dotnet/")))

(use-package lsp-ui
  :if (eq config-python-lsp-frontend 'lsp-mode)
  :after lsp
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  (lsp-ui-sideline-ignore-duplicate t)
  (lsp-ui-doc-header nil)
  (lsp-ui-sideline-enable nil)
  (lsp-ui-peek-always-show t))

(use-package lsp-imenu
  :disabled t
  :if (eq config-python-lsp-frontend 'lsp-mode)
  :hook (lsp-after-open . lsp-enable-imenu))

(use-package company-lsp
  :ensure t
  :after (lsp-mode company)
  :hook (lsp-mode . config-python--enable-company-lsp)
  :preface
  (defun config-python--enable-company-lsp ()
    (setq-local company-backends '(company-lsp))))


;; * Debug adapter protocol

(use-package dap-mode
  :if (eq config-python-lsp-frontend 'lsp-mode)
  :ensure t
  :hook ((lsp-mode . dap-mode)
         (lsp-mode . dap-ui-mode))
  :config (require 'dap-python))

;;; config-python.el ends here
(provide 'config-python)
