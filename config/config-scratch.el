;;; config-scratch.el --- Scratch buffer             -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Julien Fantin

;; Author: Julien Fantin <julienfantin@gmail.com>
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
(require 'use-package)
(require 'config-path)


(defvar persistent-scratch-save-file (user-var-file ".persistent-scratch"))

(defun config-scratch-init ()
  "Initialize the *scratch* buffer."
  (when-let (buffer (get-buffer "*scratch*"))
    (with-current-buffer buffer
      (when (require 'elisp-mode nil t)
        (emacs-lisp-mode)))))

(use-package persistent-scratch
  :straight t
  :init
  (progn
    (after-init #'persistent-scratch-setup-default)
    (after-init #'config-scratch-init)))

(use-package unkillable-scratch
  :straight t
  :init (after-init #'unkillable-scratch))

(provide 'config-scratch)
;;; config-scratch.el ends here
