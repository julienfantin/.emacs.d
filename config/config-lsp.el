;;; config-lsp.el --- Language Server Protocol Frontend config  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Julien Fantin

;; Author: Julien Fantin <jfantin@jfantin-mbp143>
;; Keywords: languages, processes, tools

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

(defvar config-lsp-frontend 'lsp-mode)


;;; Third-party
;;;; Eglot

(use-package eglot
  :if (equal config-lsp-frontend 'eglot)
  :straight t)

;;;; LSP mode

(use-package lsp-mode
  :if (equal config-lsp-frontend 'lsp-mode)
  :straight t)

(use-package lsp-ui
  :if (equal config-lsp-frontend 'lsp-mode)
  :straight t
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
  :if (equal config-lsp-frontend 'lsp-mode)
  :hook (lsp-after-open . lsp-enable-imenu))

(use-package company-lsp
  :if (equal config-lsp-frontend 'lsp-mode)
  :straight t
  :after (lsp-mode compdef)
  :config
  (compdef
   :modes #'lsp-mode
   :company #'company-lsp))

(use-package lsp-treemacs
  :if (equal config-lsp-frontend 'lsp-mode)
  :straight t
  :after lsp-mode)

;;;; Debug adapter protocol

(use-package dap-mode
  :if (equal config-lsp-frontend 'lsp-mode)
  :straight t
  :hook ((lsp-mode . dap-mode)
         (dap-mode . dap-ui-mode)))

(provide 'config-lsp)
;;; config-lsp.el ends here
