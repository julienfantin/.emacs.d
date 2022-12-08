;;; config-doc.el --- Documentation integration      -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Julien Fantin

;; Author: Julien Fantin <julienfantin@gmail.com>
;; Keywords: convenience, help

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

(use-package eldoc
  :hook (prog-mode . eldoc-mode)
  :custom
  (eldoc-echo-area-use-multiline-p nil)
  (eldoc-idle-delay 0.2)
  :config
  (eldoc-add-command
   'puni-syntactic-forward-punct
   'puni-syntactic-backward-punct
   'puni-forward-sexp
   'puni-backward-sexp
   'puni-syntactic-forward-punct
   'puni-syntactic-backward-punct
   'paredit-backward
   'paredit-forward
   'paredit-backward-delete
   'paredit-close-round))

;;; Third-party

(use-package know-your-http-well :straight t)

(provide 'config-doc)
;;; config-doc.el ends here
