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

(defvar conf-org-babel-languages
  '(emacs-lisp ocaml clojure shell))


;; * Org-mode

(use-package org
  :straight t
  :custom
  (org-ellipsis " … ")
  (org-catch-invisible-edits 'show-and-error)
  (org-agenda-files (list config-org-user-directory))
  (org-src-window-setup 'current-window)
  (org-hide-leading-stars t)
  (org-hide-emphasis-markers t)
  (org-fontify-whole-heading-line t)
  (org-fontify-quote-and-verse-blocks t)
  (org-pretty-entities t)
  (org-fontify-done-headline t)
  (org-use-speed-commands t)
  (org-speed-commands-user
   '(("N" . org-shiftmetadown)
     ("P" . org-shiftmetaup)
     ("F" . org-shiftmetaright)
     ("B" . org-shiftmetaleft)))
  (org-special-ctrl-a/e t))

(use-package org-bullets
  :straight t
  :hook (org-mode . org-bullets-mode)
  :custom
  ;; Use headings fontification to differentiate levels. This is only usable
  ;; with a theme that sets up different font sizes!
  (org-bullets-bullet-list '(" ")))

(use-package org-pretty-table
  :straight (org-pretty-table :type git :host github :repo "Fuco1/org-pretty-table")
  :hook (org-mode . org-pretty-table-mode))


;; * Babel

(use-package ob
  :after org
  :config
  ;; Load languages from 'conf-org-babel-languages'.
  (thread-last conf-org-babel-languages
    (cl-mapcar (lambda (mode) `(,mode . t)))
    (org-babel-do-load-languages 'org-babel-load-languages)))

(use-package ob-core
  :after ob
  :custom (org-confirm-babel-evaluate nil))


;; * Roam

(use-package org-roam
  :straight t
  :after org
  :hook (org-roam-buffer-prepare-hook . hide-mode-line-mode)
  :custom
  (org-roam-directory config-org-user-directory)
  ;; https://github.com/org-roam/org-roam/issues/674
  (org-roam-index-file (expand-file-name "org-roam.db" config-org-user-directory))
  :bind
  (("C-c n l" . org-roam)
   ("C-c n b" . org-roam-switch-to-buffer)
   ("C-c n f" . org-roam-find-file)
   ("C-c n g" . org-roam-show-graph)
   ("C-c n i" . org-roam-insert)))

(use-package company-org-roam
  :straight t
  :after (org-roam compdef)
  :hook (org-mode . company-mode)
  :init
  (compdef
   :modes #'org-mode
   :company '(company-org-roam company-yasnippet company-dabbrev company-capf)
   :capf #'pcomplete-completions-at-point))

(use-package org-journal
  :straight t
  :after org
  :bind
  ("C-c n j" . org-journal-new-entry)
  :custom
  (org-journal-dir config-org-user-directory)
  (org-journal-date-prefix "#+title: ")
  (org-journal-file-format "%Y-%m-%d.org")
  (org-journal-date-format "%A, %d %B %Y")
  (org-journal-enable-agenda-integration t))

(use-package deft
  :straight t
  :bind
  ("C-c n d" . deft)
  :custom
  (deft-directory config-org-user-directory)
  (deft-default-extension "org")
  (deft-recursive t)
  (deft-use-filter-string-for-filename t))

(use-package side-notes
  :straight t
  :bind
  ("C-c n t" . side-notes-toggle-notes)
  :custom
  (side-notes-file "notes.org")
  (side-notes-secondary-file "todos.org")
  (side-notes-display-alist
   '((side . right)
     (window-width . 0.3819660112501052))))

(provide 'config-org)
;;; config-org.el ends here
