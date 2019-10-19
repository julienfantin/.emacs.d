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
  '((lisp-mode          . (("Common_Lisp" "Common Lisp")))
    (clojure-mode       . ("Clojure" ("Java_SE8" "Java")))
    (clojurescript-mode . ("Clojure" "JavaScript" "HTML" "CSS"))
    (clojurec-mode      . ("Clojure"))
    (sql-mode           . ("PostgreSQL"))
    (tuareg-mode        . ("OCaml"))
    (python-mode        . (("Python_3" "Python 3"))))
  "Alist of mode-hook to list of docsets.

  Docsets are either a string or a list where the car is the name
  of the docset to install, and the cadr is the name of the
  docset to use."
  :group 'config-doc
  :type 'alist)

(use-package eldoc
  :ensure t
  :hook (prog-mode . eldoc-mode)
  :custom
  (eldoc-echo-area-use-multiline-p t)
  (eldoc-idle-delay .5)
  :config
  (after 'paredit
    (eldoc-add-command
     'paredit-backward
     'paredit-forward
     'paredit-backward-delete
     'paredit-close-round)))

(use-package know-your-http-well :ensure t)

(defvar config-completion-system)       ; silence warning

(use-package counsel-dash
  :disabled t
  :if (eq config-completion-system 'ivy)
  :ensure t
  :after config-completion
  :functions (config-doc-set-docsets)
  :commands (counsel-dash-install-docset)
  :preface
  (progn
    (defun config-doc--mode-hook (mode)
      (intern (concat (symbol-name mode) "-hook")))
    (defun config-doc--docset-install-name (docset)
      (if (listp docset) (car docset) docset))
    (defun config-doc--docset-enable-name (docset)
      (if (listp docset) (cadr docset) docset))
    (defun config-doc--enable-docsets ()
      (when-let ((docsets (cdr (assoc major-mode config-doc-default-docsets))))
        (mapc 'helm-dash-ensure-docset-installed
              (mapcar 'config-doc--docset-install-name docsets))
        (setq counsel-dash-docsets
              (mapcar #'config-doc--docset-enable-name docsets))))
    (defun config-doc--init ()
      (mapc
       (lambda (cell)
         (let* ((mode (car cell))
                (hook (config-doc--mode-hook mode)))
           (add-hook hook 'config-doc--enable-docsets)))
       config-doc-default-docsets)))
  :init (after-init #'config-doc--init)
  :custom (counsel-dash-browser-func 'eww-browse-url))

(provide 'config-doc)
;;; config-doc.el ends here
