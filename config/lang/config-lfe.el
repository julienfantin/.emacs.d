;;; config-lfe.el --- Lisp flavored Erlang config    -*- lexical-binding: t; -*-

;; Copyright (C) 2016

;; Author:  <julien@nixos>
;; Keywords: lisp

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

;; TODO lispy doesn't play nice with comint and won't send the input on enter

;;

;;; Code:
(require 'use-config)

(use-package lfe-mode
  :straight t
  :config
  (progn
    (defun lfe-eval-sexp-or-defun (&optional and-go)
      (interactive "P")
      (let ((bounds (or (bounds-of-thing-at-point 'sexp) (bounds-of-thing-at-point 'defun))))
        (comint-send-region (inferior-lfe-proc) (car bounds) (cdr bounds))
        (when and-go (switch-to-lfe t))))
    (bind-keys
     :map inferior-lfe-mode-map
     ("C-M-x" . lfe-eval-sexp-or-defun))))

(provide 'config-lfe)
;;; config-lfe.el ends here
