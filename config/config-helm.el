;;; config-helm.el --- Helm                          -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Julien Fantin

;; Author: Julien Fantin(require 'use-config) <julienfantin@gmail.com>
;; Keywords: convenience, matching

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


;; * Core

(use-package helm
  :ensure t
  :functions (helm-autoresize-mode)
  :commands (helm-mini helm-find-files)
  :preface (defvar helm-command-prefix-key "C-c h")
  :config
  (progn
    ;; Force vertical split at bottom
    (setq helm-split-window-in-side-p t
          helm-split-window-default-side 'above
          helm-autoresize-min-height 30
          helm-autoresize-max-height 30
          helm-echo-input-in-header-line t
          helm-candidate-number-limit 300)
    (helm-autoresize-mode 1)
    ;; Remap persistent action to TAB
    (bind-keys
     :map helm-map
     ("C-z" . helm-select-action)
     ("<tab>" . helm-execute-persistent-action)
     ("C-i" . helm-execute-persistent-action))
    ;; Remove '..' in helm-find-files
    ;; Seems to breaks with recent version...
    ;; (advice-add
    ;;  'helm-ff-filter-candidate-one-by-one
    ;;  :around (lambda (fcn file)
    ;;            (unless (string-match "\\(?:/\\|\\`\\)\\.\\{2\\}\\'" file)
    ;;              (funcall fcn file))))
    (defvar helm-M-x-fuzzy-match t)
    (defvar helm-ff-fuzzy-matching t)
    (defvar helm-apropos-fuzzy-match t)
    (defvar helm-buffers-fuzzy-matching t)
    (defvar helm-locate-fuzzy-match nil)
    (defvar helm-mode-fuzzy-match t)
    (defvar helm-recentf-fuzzy-match t)))

(use-package helm-config :disabled t :demand t)

(use-package helm-adaptive
  :disabled t
  :after (helm no-littering)
  :init (helm-adaptive-mode 1))

(use-package helm-command
  :after helm
  :commands (helm-M-x)
  :config (setq helm-M-x-always-save-history t))

(use-package helm-mode
  :disabled t
  :demand t
  :init (after-init #'helm-mode)
  :config
  (progn
    (setq helm-input-idle-delay 0.01
          helm-ff-transformer-show-only-basename t
          helm-ff-file-name-history-use-recentf t
          helm-ff-skip-boring-files nil)
    (add-to-list 'helm-boring-file-regexp-list "\\.DS_Store$")
    (add-to-list 'helm-boring-file-regexp-list "\\.git$")))

(use-package helm-org
  :disabled t
  :config
  (progn
    (defun helm-org-in-buffer-headings-preselection ()
      (when (re-search-backward
             org-complex-heading-regexp nil t)
        (regexp-quote (match-string-no-properties 0))))
    (defun helm-org-in-buffer-headings ()
      "Re-definition of the helm-source that pre-selects the heading at point."
      (interactive)
      (let ((helm-org-show-filename nil))
        (helm :sources (helm-source-org-headings-for-files
                        (list (current-buffer)))
              :candidate-number-limit 99999
              :buffer "*helm org inbuffer*"
              :preselect (helm-org-in-buffer-headings-preselection))))))


;; * Packages

(use-package helm-aws :disabled t :ensure t)
(use-package helm-org-rifle :disabled t :ensure t)


(provide 'config-helm)
;;; config-helm.el ends here
