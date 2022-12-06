;;; early-init.el ---                                -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Julien Fantin

;; Author: Julien Fantin <julienfantin@gmail.com>
;; Keywords:

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

(require 'cl-lib)
(require 'cl-macs)

;;; Performance

(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

;; Use a hook so the message doesn't get clobbered by other messages.
(add-hook
 'emacs-startup-hook
 (lambda ()
   (message
    "Emacs startup in %s with %d garbage collections."
    (format "%.2f seconds" (float-time (time-subtract after-init-time before-init-time))) gcs-done)))

;;; Config

;; Straight setup recommendation
(setq package-enable-at-startup nil)

;; Allow loading from the package cache.
(setq package-quickstart t)

;; Do not resize the frame at this early stage.
(setq frame-inhibit-implied-resize t)

;;; System

;; Emacs-plus defines a hook to react to system appearance changes, it's called
;; too early in the init process so we record the appearance here

(defvar -ns-system-appearance nil)

(when (boundp 'ns-system-appearance-change-functions)
  (add-hook 'ns-system-appearance-change-functions
            (lambda (appearance)
              (setq -ns-system-appearance appearance)) ))

;;; Path

(setq user-emacs-directory (expand-file-name "~/.emacs.d/"))

(defun -reload-init ()
  "Reload the init file."
  (interactive)
  (load-file (expand-file-name "init.el" user-emacs-directory)))


;;; Early loads

;; Setup various paths before we do anything, rest of init depends on this...
(load-file (expand-file-name "config/config-path.el" user-emacs-directory))
;; Bootstrap straight and package management
(load-file (expand-file-name "config/config-straight.el" user-emacs-directory))

;; Setup our env path here, some configs might need this to be set
(use-package exec-path-from-shell
  :straight t
  :init (exec-path-from-shell-initialize))

(provide 'early-init)
;;; early-init.el ends here
