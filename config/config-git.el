;;; config-git.el --- Git version-control            -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Julien Fantin

;; Author: Julien Fantin <julienfantin@gmail.com>
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

(require 'use-package)

;;; Built-ins

(use-package vc
  :custom
  ;; Don't refresh remote files
  (vc-handled-backends '(Git))
  (vc-ignore-dir-regexp
   (format "\\(%s\\)\\|\\(%s\\)" vc-ignore-dir-regexp tramp-file-name-regexp)))

(use-package vc-hooks
  :custom
  (vc-follow-symlinks t))

(use-package ediff
  :config
  (defun config-git-ediff-both ()
    (interactive)
    (ediff-copy-diff
     ediff-current-difference nil 'C nil
     (concat
      (ediff-get-region-contents ediff-current-difference 'A ediff-control-buffer)
      (ediff-get-region-contents ediff-current-difference 'B ediff-control-buffer))))
  (defun config-git-ediff-keymap-setup ()
    (define-key ediff-mode-map "d" 'config-git-ediff-both))
  (add-hook 'ediff-keymap-setup-hook 'config-git-ediff-keymap-setup)
  :custom
  ;; Remove noisy highlights
  (ediff-highlight-all-diffs nil)
  ;; Ignore whitespace
  (ediff-diff-options "-w")
  ;; Avoid the crazy multi-frames setup
  (ediff-window-setup-function 'ediff-setup-windows-plain)
  ;; Counter-intuitve naming here, but windows should be side-by-side...
  (ediff-split-window-function 'split-window-horizontally))

;;; Third-party

(use-package magit
  :straight t
  :custom
  ;; sort branches by most recent commit
  (magit-list-refs-sortby "-creatordate")
  (magit-diff-refine-ignore-whitespace t)
  (magit-save-repository-buffers 'dontask)
  (magit-display-buffer-function
   (lambda (buffer)
     (display-buffer-same-window buffer nil))))

(use-package forge
  :straight t
  :after magit)

(use-package magit-todos
  :straight t
  :after magit
  :config
  (magit-todos-mode 1))

(use-package dired-git-info
  :disabled t
  :straight t
  :after dired
  :hook (dired-after-readin . dired-git-info-auto-enable))

(use-package git-link
  :straight t
  :custom
  (git-link-use-commit t))

(use-package git-commit
  :after magit)

(use-package git-timemachine
  :straight (:host github :repo "emacsmirror/git-timemachine" :branch "master"))

(use-package diff-hl
  :straight t
  :hook ((after-init . global-diff-hl-mode)
         (dired-mode . diff-hl-dired-mode-unless-remote))
  :custom
  (diff-hl-draw-borders nil)
  ;; there's a posframe version that shows a nicer diff but it gets corrupted
  (diff-hl-show-hunk-function 'diff-hl-show-hunk-inline-popup)
  :config
  (global-diff-hl-show-hunk-mouse-mode))

;; TODO
(use-package consult-git-log-grep
  :disabled t
  :straight t
  :custom
  (consult-git-log-grep-open-function #'magit-show-commit))

(provide 'config-git)
;;; config-git.el ends here
