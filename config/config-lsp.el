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

(defvar config-lsp-on-save-hooks
  '(config-lsp-action-format-buffer
    config-lsp-action-organize-imports))

(defun config-lsp-run-before-save-hooks ()
  "Run `config-lsp-on-save-hooks'."
  (when (config-project-owner-p)
    (run-hooks 'config-lsp-on-save-hooks)))

(defun config-lsp-action-organize-imports ()
  "Organize imports."
  (interactive)
  (cl-case config-lsp-frontend
    (lsp-mode (lsp-organize-imports))
    (eglot (eglot-code-action-organize-imports))))

(defun config-lsp-action-format-buffer ()
  "Organize imports."
  (interactive)
  (cl-case config-lsp-frontend
    (lsp-mode (lsp-format-buffer))
    (eglot (eglot-format-buffer))))

;;; Third-party
;;;; Eglot

(use-package eglot
  :if (eq config-lsp-frontend 'eglot)
  :straight t
  :hook (before-save . config-lsp-run-before-save-hooks)
  :bind (:map eglot-mode-map
              ("C-c l f" . eglot-format-buffer)
              ("C-c l %" . eglot-rename)
              ("C-c l i" . eglot-code-action-inline)
              ("C-c l x" . eglot-code-action-extract)
              ("C-c l r" . eglot-code-action-rewrite)
              ("C-c l q" . eglot-code-action-quickfix)
              ("C-c l o" . eglot-code-action-organize-imports)
              ("C-c l d" . eglot-find-declaration)
              ("C-c l ." . eglot-find-implementation)
              ("C-c l t" . eglot-find-typeDefinition))
  :custom
  (eglot-extend-to-xref t))

;;;; LSP mode

(use-package lsp-mode
  :if (eq config-lsp-frontend 'lsp-mode)
  :straight t
  :hook (before-save . config-lsp-run-before-save-hooks)
  :init
  (setq lsp-keymap-prefix "C-c l")
  :custom
  ;; system
  (lsp-enable-indentation nil)
  (lsp-enable-symbol-highlighting t)
  (lsp-diagnostics-provider (if (eq config-parsers-backend 'flymake) t :auto)))

(use-package lsp-ui
  :if (eq config-lsp-frontend 'lsp-mode)
  :straight t
  :hook
  ((lsp-mode . lsp-ui-mode)
   (lsp-mode . lsp-ui-doc-mode)
   (lsp-mode . lsp-diagnostics-mode)
   (lsp-mode . lsp-completion-mode))
  :bind
  (:map lsp-ui-mode-map
        ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
        ([remap xref-find-references] . lsp-ui-peek-find-references))
  :custom
  ;; completion
  (lsp-enable-snippet t)
  (lsp-completion-provider
   (if (eq config-completion-completion-at-point 'company) :capf :none))
  (lsp-completion-show-kind t)
  (lsp-completion-show-detail t)
  (lsp-enable-completion-at-point t)
  ;; https://emacs-lsp.github.io/lsp-mode/tutorials/how-to-turn-off/
  ;;
  ;; lsp-ui-doc - on hover dialogs
  (lsp-headerline-breadcrumb-enable nil)
  (lsp-ui-doc-enable t)
  (lsp-ui-doc-delay 1.2)
  (lsp-ui-doc-max-width 80)
  (lsp-ui-doc-max-height 100)
  (lsp-ui-doc-show-with-cursor t)
  (lsp-ui-doc-show-with-mouse nil)
  (lsp-ui-doc-use-webkit nil)
  (lsp-ui-doc-include-signature t)
  (lsp-ui-doc-position 'at-point)
  (lsp-ui-doc-border nil)

  ;; lenses are the reference counter overlays that mess up the buffer
  (lsp-lens-enable nil)
  (lsp-headerline-breadcrumb-enable t)

  ;; Sideline code actions
  (lsp-ui-sideline-enable t)
  (lsp-ui-sideline-delay 0.2)
  (lsp-ui-sideline-show-hover nil)
  (lsp-ui-sideline-show-code-actions nil)
  (lsp-ui-sideline-show-symbol nil)
  (lsp-ui-sideline-show-diagnostics nil) ; prefer posframe

  ;; breadcrumbs
  (lsp-headerline-breadcrumb-enable nil)
  (lsp-headerline-breadcrumb-enable-diagnostics nil)
  (lsp-headerline-breadcrumb-enable-symbol-numbers nil)
  (lsp-headerline-breadcrumb-icons-enable nil)

  ;; modeline
  (lsp-modeline-code-actions-enable nil)
  (lsp-modeline-diagnostics-enable nil)

  (lsp-signature-auto-activate t)
  (lsp-signature-render-documentation t)

  ;; eldoc
  (lsp-eldoc-enable-hover t)

  ;; misc
  (lsp-ui-peek-enable nil)
  (lsp-ui-imenu-enable t))

(use-package lsp-imenu
  :if (eq config-lsp-frontend 'lsp-mode)
  :hook (lsp-after-open . lsp-enable-imenu))

;; For call hierarchy
(use-package lsp-treemacs
  :if (eq config-lsp-frontend 'lsp-mode)
  :straight t
  :after lsp-mode)

(use-package treemacs-all-the-icons
  :straight t
  :demand t
  :after (all-the-icons))

;;;; Debug adapter protocol

(use-package dap-mode
  :if (eq config-lsp-frontend 'lsp-mode)
  :straight t
  :hook ((lsp-mode . dap-mode)
         (dap-mode . dap-ui-mode))
  :config
  (dap-mode 1)
  (dap-ui-mode 1)
  (dap-tooltip-mode 1)
  (tooltip-mode 1)
  (dap-ui-controls-mode 1))

(provide 'config-lsp)
;;; config-lsp.el ends here
