;;; config-doc.el --- Documentation integration      -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Julien Fantin

;; Author: Julien Fantin(require 'use-config) <julienfantin@gmail.com>
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
(require 'use-config)

(defcustom config-doc-default-docsets
  '((common-lisp-mode-hook   . (("Common_Lisp" "Common Lisp")))
    (clojure-mode-hook       . ("Clojure" ("Java_SE8" "Java")))
    (clojurescript-mode-hook . ("Clojure" "JavaScript" "HTML" "CSS"))
    (clojurec-mode-hook      . ("Clojure"))
    (lfe-mode-hook           . ("Erlang"))
    (sql-mode-hook           . ("PostgreSQL"))
    (tuareg-mode-hook        . ("OCaml")))
  "Alist of mode-hook to list of docsets.

  Docsets are either a string or a list where the car is the name
  of the docset to install, and the cadr is the name of the
  docset to use."
  :group 'config-doc
  :type 'alist)

(use-package eldoc
  :ensure t
  :defer t
  :init (add-hook 'prog-mode-hook #'eldoc-mode)
  :config
  (progn
    (validate-setq eldoc-idle-delay .5)
    (after 'paredit
      (eldoc-add-command
       'paredit-backward
       'paredit-forward
       'paredit-backward-delete
       'paredit-close-round))))

(use-package know-your-http-well :ensure t :defer t)

(defvar config-completion-system)       ; silence warning

(use-package counsel-dash
  :if (eq config-completion-system 'ivy)
  :ensure t
  :defer t
  :after config-completion
  :functions (config-doc-set-docsets)
  :config
  (progn
    (validate-setq helm-dash-docsets-path (expand-file-name ".docsets" "~/"))
    (defun config-doc--docset-install-name (docset)
      (if (listp docset) (car docset) docset))
    (defun config-doc--docset-enable-name (docset)
      (if (listp docset) (cadr docset) docset))
    (defun config-docs--docset-installed-p (docset)
      (member
       (config-doc--docset-enable-name docset)
       (helm-dash-installed-docsets)))
    (defun config-doc--ensure-installed (docset)
      (unless (config-docs--docset-installed-p docset)
	(counsel-dash-install-docset (config-doc--docset-install-name docset))))
    (defun config-doc--enable-docsets (docsets)
      (validate-setq counsel-dash-docsets
                     (mapcar #'config-doc--docset-enable-name docsets)))
    (defun config-doc-set-docsets (mode-hook docsets)
      (mapc 'config-doc--ensure-installed docsets)
      (add-hook mode-hook #'(lambda () (config-doc--enable-docsets docsets))))
    (mapc
     (lambda (cell)
       (config-doc-set-docsets (car cell) (cdr cell)))
     config-doc-default-docsets)))

(use-package google-this :disabled t :ensure t :defer t)

(provide 'config-doc)
;;; config-doc.el ends here
