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
(require 'use-package)

(defvar config-python-lsp-frontend 'lsp-mode)
(defvar config-python-lsp-backend 'ms-python)


;; * Python

(use-package python
  :hook (python-mode . subword-mode))

(use-package pyvenv
  :disabled t
  ;; Set `pyvenv-workon' to the absolute path for the current venv in a .dir-locals.el
  :straight t
  :hook (python-mode . pyvenv-mode))

(use-package auto-virtualenv
  :disabled t
  :ensure t
  :defer t
  :init
  ;; add .python-version file to project root, then add path of virtualenv eg:~/Envs/venv36/
  (add-hook 'python-mode-hook 'auto-virtualenv-set-virtualenv)
  ;; Activate on changing buffers
  (add-hook 'window-configuration-change-hook 'auto-virtualenv-set-virtualenv)
  ;; Activate on focus in
  (add-hook 'focus-in-hook 'auto-virtualenv-set-virtualenv)
  ;; (add-hook 'projectile-after-switch-project-hook 'auto-virtualenv-set-virtualenv)
  )


(use-package python-docstring
  :straight t
  :after python
  :bind
  (:map python-mode-map
        ([remap fill-paragraph] . python-docstring-fill)))


;; * Elpy

(use-package flycheck
  :if (null config-python-lsp-backend)
  :after elpy
  :hook (elpy-mode . flycheck-mode)
  :config
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules)))

;; pip install elpy rope jedi autopep8 yapf flake8 isort importmagic epc autoflake

(use-package elpy
  :if (null config-python-lsp-backend)
  :straight t
  :hook (python-mode . elpy-enable)
  :custom
  (elpy-get-info-from-shell t))


;; * Environment

(use-package py-autopep8
  :disabled t
  :straight t
  :hook (python-mode . py-autopep8-enable-on-save))

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

(use-package python
  :disabled t
  :hook ((before-save-hook . config-python-autoflake-turn-on)))

(use-package pytest
  :disabled t
  :straight t
  :config
  (defun config-python-pytest-integration ()
    (setq pytest-global-name "onepy"
          pytest-project-root-test (lambda (dirname) (equal dirname "/code/lastmile")))))

(use-package python-pytest
    :straight t
    :config
    (defun python-pytest--project-root ()
      "/code/lastmile")
    :custom
    (python-pytest-executable "onepytest"))


;; * Editing

(use-package indent-tools
    :straight t
    :hook (python-mode . indent-tools-minor-mode)
    :bind (:map python-mode-map ("C-c SPC" . indent-tools-hydra/body)))

(use-package smartparens-python
  :straight smartparens
  :hook (python-mode . smartparens-mode))

(use-package markdown-mode
  :config
  (add-to-list 'markdown-code-lang-modes '("python" . python-mode)))


;; * Language server protocol

;; Disable default flycheck checkers
(use-package flycheck
  :disabled t
  :config
  (add-to-list 'flycheck-enabled-checkers 'flycheck-flake8)
  (add-to-list 'flycheck-disabled-checkers 'python-pylint))

;; ** Eglot

(use-package eglot
  :if (eq config-python-lsp-frontend 'eglot)
  :straight t
  :ensure-system-package (pyls . "pip install 'python-language-server[all]' pyls-isort")
  :hook ((python-mode . eglot-ensure)))

;; ** lsp-mode

(use-package lsp-mode
  :if (eq config-python-lsp-frontend 'lsp-mode)
  :straight t)

(use-package lsp-python-ms
  :if (and (eq config-python-lsp-frontend 'lsp-mode) (eq config-python-lsp-backend 'ms-python))
  :straight t
  :demand t
  :hook (python-mode . lsp)
  :init
  (remhash 'pyls lsp-clients)
  :custom
  (lsp-python-ms-nupkg-channel "daily"))

(use-package lsp-ui
  :if (eq config-python-lsp-frontend 'lsp-mode)
  :after lsp-mode
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  (lsp-ui-doc-use-webkit (featurep 'xwidget-internal))
  (lsp-ui-doc-enable t)
  (lsp-ui-peek-enable t)
  (lsp-ui-sideline-enable t)
  (lsp-ui-imenu-enable t)
  (lsp-ui-flycheck-enable t)
  (lsp-ui-sideline-toggle-symbols-info t)
  (lsp-ui-doc-include-signature t))

(use-package lsp-imenu
  :if (eq config-python-lsp-frontend 'lsp-mode)
  :hook (lsp-after-open . lsp-enable-imenu))

(use-package company-lsp
  :if (eq config-python-lsp-frontend 'lsp-mode)
  :straight t
  :after (lsp-mode compdef)
  :config
  (compdef
   :modes #'python-mode
   :company #'company-lsp))

(use-package lsp-treemacs
  :straight t)


;; * Debug adapter protocol

(use-package dap-mode
  :if (eq config-python-lsp-frontend 'lsp-mode)
  :straight t
  :hook ((lsp-mode . dap-mode)
         (dap-mode . dap-ui-mode)
         (python-mode . (lambda () (require 'dap-python))))
  :demand t)

;;; config-python.el ends here
(provide 'config-python)
