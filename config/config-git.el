;;; config-git.el --- Git version-control            -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Julien Fantin

;; Author: Julien Fantin(require 'use-config) <julienfantin@gmail.com>
;; Keywords: vc

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


;; * Built-ins

(use-package vc
  :custom
  ;; Don't refresh remote files
  (vc-handled-backends '(Git))
  (vc-ignore-dir-regexp (format "\\(%s\\)\\|\\(%s\\)"
                                vc-ignore-dir-regexp
                                tramp-file-name-regexp)))

(use-package ediff
  :custom
  ;; Remove noisy highlights
  (ediff-highlight-all-diffs nil)
  ;; Avoid the crazy multi-frames setup
  (ediff-window-setup-function 'ediff-setup-windows-plain)
  ;; Ignore whitespace
  (ediff-diff-options "-w")
  ;; Counter-intuitve naming here, but windows should be side-by-side...
  (ediff-split-window-function 'split-window-horizontally))


;; * Packages

(use-package git-timemachine :ensure t)

(use-package git-link :ensure t)

(use-package magit
  :ensure t
  :custom
  (magit-save-repository-buffers 'dontask)
  (magit-display-buffer-function
   (lambda (buffer)
     (display-buffer-same-window buffer nil))))

(use-package magit-wip
  :after magit
  :init (after-init #'magit-wip-mode))

(use-package git-commit
  :after magit
  :custom
  (git-commit-summary-max-length 72))

(use-package magithub
  :ensure t
  :after (magit no-littering)
  :config (magithub-feature-autoinject t))

(use-package gited :ensure t)

(use-package helm-hunks
  :ensure t
  :custom (helm-hunks-preview-diffs t))

(use-package ediff
  ;; NOTE using :bind ediff-mode-map somehow binds globally
  :hook
  (ediff-keymap-setup . config-git-add-d-to-ediff-mode-map)
  :config
  (defun config-git-ediff-copy-both-to-C ()
    "Copy both sides of the diff to the C buffer"
    (interactive)
    (ediff-copy-diff
     ediff-current-difference nil 'C nil
     (concat
      (ediff-get-region-contents ediff-current-difference 'A ediff-control-buffer)
      (ediff-get-region-contents ediff-current-difference 'B ediff-control-buffer))))
  (defun config-git-add-d-to-ediff-mode-map ()
    (define-key ediff-mode-map "d" 'config-git-ediff-copy-both-to-C)))

(provide 'config-git)
;;; config-git.el ends here
