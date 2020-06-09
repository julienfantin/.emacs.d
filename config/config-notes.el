;;; config-notes.el --- Reading and note-taking config  -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Julien Fantin

;; Author: Julien Fantin <julienfantin@gmail.com>
;; Keywords: tools, convenience, docs

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

(require 'use-package)


;; * Paths

(defvar config-notes-directory "~/org/reading")
(defvar config-notes-notes-file (expand-file-name "index.org" config-notes-directory))
(defvar config-notes-bib-file (expand-file-name "index.bib" config-notes-directory))
(defvar config-notes-pdf-directory (expand-file-name "lib/" config-notes-directory))


;; * Packages

(use-package pdf-tools :ensure t)

(use-package interleave :ensure t)

(use-package org-ref
  :ensure t
  :after org
  :init (require 'org-ref nil t)
  :custom
  (org-ref-notes-directory config-notes-directory)
  (org-ref-bibliography-notes config-notes-notes-file)
  (org-ref-default-bibliography (list config-notes-bib-file))
  (org-ref-pdf-directory config-notes-pdf-directory))

(use-package helm-bibtex
  :if (eq config-completion-system 'helm)
  :ensure t)

(use-package ivy-bibtex
  :if (eq config-completion-system 'ivy)
  :ensure t
  :config
  (add-to-list 'ivy-re-builders-alist '(ivy-bibtex . ivy--regex-ignore-order)))

(use-package bibtex-completion
  :custom
  (bibtex-completion-bibliography (list config-notes-bib-file))
  (bibtex-completion-library-path config-notes-pdf-directory)
  (bibtex-completion-notes-path config-notes-notes-file)
  :config
  (unless (file-exists-p config-notes-directory)
    (make-directory config-notes-directory t))
  (unless (file-exists-p config-notes-pdf-directory)
    (make-directory config-notes-pdf-directory t))
  (unless (file-exists-p config-notes-notes-file)
    (f-write-text "" 'utf-8 config-notes-notes-file))
  (unless (file-exists-p config-notes-bib-file)
    (f-write-text "" 'utf-8 config-notes-bib-file)))

(provide 'config-notes)
;;; config-notes.el ends here
