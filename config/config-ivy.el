;;; config-ivy.el --- Ivy completion                 -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Julien Fantin

;; Author: Julien Fantin(require 'use-config) <julienfantin@gmail.com>
;; Keywords: convenience, internal

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


;; * Customs

(defvar config-ivy-fixed-height 12
  "Prevent the mini-buffer from re-sizing.")

(defvar config-ivy-flx-matching nil
  "Enable flx matching.")


;; * Flx

(use-package flx
  :if config-ivy-flx-matching
  :ensure t
  :config (add-to-list 'flx-word-separators ? ))


;; * Swiper

(use-package swiper :ensure t :defer t)

(use-package ivy
  :init (after-init #'ivy-mode)
  :commands (ivy-set-actions)
  :config
  (progn
    (when config-ivy-flx-matching
      (setq ivy-re-builders-alist
            '((ivy-switch-buffer . ivy--regex-plus)
              (t . ivy--regex-fuzzy))))
    (setq ivy-height 15
          ivy-fixed-height-minibuffer t
          ;; Like recentf when switching buffers
          ivy-use-virtual-buffers t
          ;; Allow matching at any point in the term
          ivy-initial-inputs-alist nil)
    (defun -ivy-switch-buffer ()
      "Switch to another buffer, default to the previous one."
      (interactive)
      (let ((this-command 'ivy-switch-buffer))
        (ivy-read "Switch to buffer: " 'internal-complete-buffer
                  :matcher #'ivy--switch-buffer-matcher
                  :preselect (buffer-name (other-buffer (current-buffer) t))
                  :action #'ivy--switch-buffer-action
                  :keymap ivy-switch-buffer-map
                  :caller 'ivy-switch-buffer)))
    (bind-key "C-x b" '-ivy-switch-buffer ivy-mode-map)))

(use-package counsel
  :ensure t
  :init (after-init #'counsel-mode)
  :preface
  (defun config-counsel-delete-file (x)
    (delete-file (expand-file-name x ivy--directory)))
  :config
  (progn
    (ivy-set-actions
     'counsel-find-file
     `(("x" #'config-counsel-delete-file ,(propertize "delete" 'face 'font-lock-warning-face))))
    (setq counsel-find-file-at-point t
          ivy-extra-directories nil)))

;; Counsel makes use of smex
(use-package smex
  :ensure t
  :defer t
  :preface (defvar smex-history-length 100)
  :init (after-init #'smex-initialize)
  :config (setq smex-save-file (user-var-file "smex")))

(provide 'config-ivy)
;;; config-ivy.el ends here

;;  LocalWords:  flx
