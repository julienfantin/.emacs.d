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

;;; Config

(defvar config-org-user-directory "~/org/")

;;; Built-ins

(use-package org
  :straight (:type built-in)
  :bind
  ((:map org-mode-map
         ("C-M-<return>"   . org-insert-subheading)
         ("C-M-S-<return>" . org-insert-todo-subheading)
         ;; Paredit-like movements
         ("C-M-u"          . org-up-element)
         ("C-M-d"          . org-down-element)
         ("C-M-f"          . org-forward-element)
         ("C-M-b"          . org-backward-element)))
  :custom
  (org-ellipsis " … ")
  (org-catch-invisible-edits 'show-and-error)
  (org-agenda-files (list config-org-user-directory))
  (org-src-window-setup 'current-window)
  (org-hide-leading-stars t)
  (org-hide-emphasis-markers t)
  (org-fontify-quote-and-verse-blocks t)
  (org-fontify-whole-block-delimiter-line)
  (org-pretty-entities t)
  (org-use-speed-commands t)
  (org-speed-commands-user
   '(("Outline Navigation")
     ("k" . (org-speed-move-safe 'org-forward-element))
     ("i" . (org-speed-move-safe 'org-backward-element))
     ("l" . (org-speed-move-safe 'org-down-element))
     ("j" . (org-speed-move-safe 'org-up-element))
     ("Outline Structure Editing")
     ("I" . org-metaup)
     ("K" . org-metadown)
     ("L" . org-metaright)
     ("J" . org-metaleft))))

(use-package org-footnote
  :custom
  (org-footnote-auto-adjust t)
  (org-footnote-fill-after-inline-note-extraction t))

(use-package org-refile
  :custom
  (org-refile-targets
   '((nil :maxlevel . 3)
     (org-agenda-files :maxlevel . 3))))

;; https://blog.d46.us/advanced-emacs-startup/

(use-package ob-core
  :after ob
  :custom (org-confirm-babel-evaluate nil))

(use-package ob-emacs-lisp
  :commands (org-babel-execute:emacs-lisp))

(use-package ob-python
  :commands (org-babel-execute:python))

(use-package ob-ocaml
  :commands (org-babel-execute:ocaml))

(use-package ob-clojure
  :commands
  (org-babel-execute:clojure
   org-babel-execute:clojurescript))

(use-package ob-shell
  :commands
  (org-babel-execute:sh
   org-babel-expand-body:sh
   org-babel-execute:bash
   org-babel-expand-body:bash))

;;; Third-party

(use-package org-superstar
  :disabled t
  :straight t
  :after org
  :hook (org-mode . org-superstar-mode)
  :custom
  (org-superstar-remove-leading-stars t)
  (org-superstar-headline-bullets-list '(" ")) ;; '("🞛" "◉" "○" "▷")
  (org-superstar-item-bullet-alist
   '((?+ . ?•)
     (?* . ?➤)
     (?- . ?–))))

(use-package org-pretty-table
  :straight (org-pretty-table :type git :host github :repo "Fuco1/org-pretty-table")
  :hook (org-mode . org-pretty-table-mode))

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
  :after (org-mode org-roam compdef)
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
