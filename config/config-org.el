;;; config-org.el --- Org-mode and org-babel         -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Julien Fantin

;; Author: Julien Fantin <julienfantin@gmail.com>
;; Keywords: outlines, tools

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
(eval-when-compile
  (require 'cl-lib))


;; * Customs

(defvar config-org-user-directory "~/org/")

(defvar config-org-structure-templates
  '(("el" "#+begin_src emacs-lisp\n  ?\n#+end_src" "<src lang=\"emacs-lisp\">\n?\n</src>")
    ("clj" "#+begin_src clojure\n  ?\n#+end_src" "<src lang=\"clojure\">\n?\n</src>")
    ("sql" "#+begin_src sql\n  ?\n#+end_src" "<src lang=\"sql\">\n?\n</src>")))

(defvar conf-org-babel-languages
  '(emacs-lisp sql ocaml clojure shell))


;; * Core

(defun config-org-insert-checkbox (_)
  "Insert a check box."
  (interactive "P")
  (insert "- [ ] "))

(use-package org
  :custom
  (org-agenda-files (list config-org-user-directory))
  (org-log-done 'time)
  (org-src-window-setup 'current-window)
  (org-src-fontify-natively t)
  (org-startup-indented t)
  (org-startup-folded nil)
  (org-hide-leading-stars t)
  (org-fontify-whole-heading-line t)
  (org-fontify-quote-and-verse-blocks t)
  (org-use-fast-tag-selection nil)
  (org-use-speed-commands t)
  (org-speed-commands-user
   '(("N" . org-shiftmetadown)
     ("P" . org-shiftmetaup)
     ("F" . org-shiftmetaright)
     ("B" . org-shiftmetaleft)))
  :config
  ;; Add templates from 'config-org-structure-templates'.
  (dolist (template config-org-structure-templates)
    (add-to-list 'org-structure-template-alist template)))

(use-package ob
  :config
  ;; Load languages from 'conf-org-babel-languages'.
  (thread-last conf-org-babel-languages
    (cl-mapcar (lambda (mode) `(,mode . t)))
    (org-babel-do-load-languages 'org-babel-load-languages)))

(use-package ob-core
  :custom (org-confirm-babel-evaluate nil))

(use-package org-capture
  :custom (org-reverse-note-order t))

(provide 'config-org)
;;; config-org.el ends here
