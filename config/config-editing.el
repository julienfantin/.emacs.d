;;; config-editing.el --- Editing                    -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Julien Fantin

;; Author: Julien Fantin <julienfantin@gmail.com>
;; Keywords: editing

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

;; Interesting packages:
;; https://github.com/mkcms/interactive-align

;;

;;; Code:

(require 'use-package)
(require 'config-path)

;;; Built-ins

(use-package emacs
  :custom
  (fill-column 80))

(use-package delsel
  :init (delete-selection-mode 1))

(use-package simple
  :custom
  (kill-ring-max most-positive-fixnum)
  (kill-do-not-save-duplicates t))

(use-package newcomment
  :config
  (setq-default comment-auto-fill-only-comments t))

(use-package view-mode
  :disabled t
  :bind (("C-c q" . view-mode)
         :map view-mode-map
         ("i" . view-exit)
         ("n" . next-line)
         ("p" . previous-line))
  :preface
  (defun config-editing-view-mode-update-cursor ()
    (setq cursor-type (if view-mode 'box 'bar)))
  (defun view-mode-activate ()
    ;; autoloads cannot compile if the buffer is read-only?
    (unless (string-match ".+-autoloads.el" (buffer-file-name))
      
      (view-mode 1)))
  :init
  (setq-default cursor-type 'bar)
  (add-hook 'find-file-hook #'view-mode-activate)
  (add-hook 'view-mode-hook #'config-editing-view-mode-update-cursor))

;;; Third-party

(use-package goto-chg
  :straight t
  :bind (("C-z" . goto-last-change)))

(use-package expand-region
  :straight t
  :bind ("C-c u" . er/expand-region))

(use-package goggles
  :straight t
  :hook ((prog-mode text-mode) . goggles-mode)
  :custom
  (goggles-pulse-iterations 10))

(use-package mwim
  :straight t
  :bind (("C-a" . mwim-beginning)
         ("C-e" . mwim-end)))

(use-package vundo
  :straight t
  :bind ("C-x u" . vundo)
  :custom
  (vundo-compact-display t))

(use-package wgrep
  :straight t)

;;; Commands

;;;###autoload
(defun -backward-kill-word-or-region ()
  "Kill backward word or region if active."
  (interactive)
  (if (region-active-p)
      (if (bound-and-true-p paredit-mode)
          (call-interactively 'paredit-kill-region)
        (call-interactively 'kill-region))
    (if (bound-and-true-p paredit-mode)
        (call-interactively 'paredit-backward-kill-word)
      (call-interactively 'backward-kill-word))))

;;;###autoload
(defun -cleanup ()
  "Indent, untabify and cleanup whitespace in region or buffer."
  (interactive)
  (save-excursion
    (unless (use-region-p)
      (goto-char (point-min))
      (push-mark)
      (goto-char (point-max)))
    (untabify (region-beginning) (region-end))
    (indent-region (region-beginning) (region-end))
    (save-restriction
      (narrow-to-region (region-beginning) (region-end))
      (whitespace-cleanup)))
  (message (format "%s cleaned!" (buffer-name))))

;;;###autoload
(defun -unfill-paragraph (&optional region)
  "Turn a multi-line paragraph into a single line of text."
  (interactive (progn (barf-if-buffer-read-only) '(t)))
  (let ((fill-column (point-max))
        ;; This would override `fill-column' if it's an integer.
        (emacs-lisp-docstring-fill-column t))
    (fill-paragraph nil region)))

;;;###autoload
(defun -delete-blank-lines ()
  (interactive)
  (let ((from (point)))
    (replace-regexp "^\n\n+" "\n"  nil (point-min) (point-max) nil)
    (goto-char from)))

(provide 'config-editing)
;;; config-editing.el ends here
