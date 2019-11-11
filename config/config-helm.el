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

(use-package helm-config
  :ensure helm
  :bind
  (("C-x C-f" . helm-find-files)
   ("C-x C-b" . helm-buffers-list)
   ("C-x C-r" . helm-recentf)
   ("C-x b"   . helm-mini)
   ("M-x"     . helm-M-x)
   ("M-y"     . helm-show-kill-ring)
   ("C-h a"   . helm-apropos)
   ("C-c h"   . helm-command-prefix)
   ("C-h C-l" . helm-locate-library))
  :preface (defvar helm-command-prefix-key "C-c h")
  :config
  (progn
    ;; Force vertical split at bottom
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
    )
  :custom
  (helm-split-window-in-side-p t)
  (helm-split-window-default-side 'below)
  (helm-autoresize-min-height 40)
  (helm-autoresize-max-height 40)
  (helm-echo-input-in-header-line t)
  (helm-candidate-number-limit 300)
  (helm-ff-fuzzy-matching t)
  (helm-apropos-fuzzy-match t)
  (helm-buffers-fuzzy-matching t)
  (helm-locate-fuzzy-match nil)
  (helm-mode-fuzzy-match t)
  (helm-recentf-fuzzy-match t))

(use-package helm-adaptive
  :disabled t
  :after (helm no-littering)
  :init (helm-adaptive-mode 1))

(use-package helm-command
  :after helm
  :commands (helm-M-x)
  :custom
  (helm-M-x-always-save-history t)
  (helm-M-x-fuzzy-match t))

(use-package helm-mode
  :after helm
  :init (after-init #'helm-mode)
  :custom
  (helm-input-idle-delay 0.01)
  (helm-ff-transformer-show-only-basename t)
  (helm-ff-file-name-history-use-recentf t)
  (helm-ff-skip-boring-files nil))

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

(use-package helm-aws :disabled t :straight t)

(use-package helm-org-rifle :disabled t :straight t)

(provide 'config-helm)
;;; config-helm.el ends here
