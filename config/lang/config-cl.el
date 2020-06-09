;;; config-cl.el --- Common lisp config              -*- lexical-binding: t; -*-

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

;;

;;; Code:

(require 'use-package)
(require 'config-completion)

(use-package slime :straight t)

(use-package slime-company
  :straight t
  :init
  (after 'slime
    (slime-setup '(slime-company))
    (config-completion-add-backends 'slime-mode 'slime-company)))

(provide 'config-cl)
;;; config-cl.el ends here
