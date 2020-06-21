;;; config-wip.el --- Temporary init code for quick testing  -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Julien Fantin

;; Author: Julien Fantin <julienfantin@gmail.com>
;; Keywords: convenience

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


;; * Defaults

(add-to-list 'exec-path "~/bin")

(setq default-truncate-lines t)

(use-package emacs
  :preface
  (defun -set-line-spacing ()
    (setq-local default-text-properties '(line-spacing 0.25 line-height 1.25)))
  :hook (((text-mode . -set-line-spacing)
          (prog-mode . -set-line-spacing))))


;; * UI tweaks
;; ** Show paren expression transparency hook

(defun fix-show-parent-match (theme &optional _no-confirm no-enable)
  (unless (or no-enable (equal 'duotone theme))
    (let* ((face 'show-paren-match)
           (face-bg (face-attribute face :background))
           (bg (face-attribute 'default :background))
           (new-color (chroma-blend
                       (chroma-hex :hex face-bg)
                       (chroma-hex :hex bg)
                       0.95)))
      (set-face-background face (chroma-to-string new-color)))
    (set-face-foreground 'show-paren-match nil)
    ;; (set-face-attribute 'show-paren-match nil)
    ))

(comment
 (advice-add #'load-theme :after #'fix-show-parent-match))



;; * Keybindings

(global-set-key (kbd "C-x =") 'balance-windows-area)

(use-package hercules :straight t)


;; * Org-roam



;; * Document annotation workflow

(use-package pdf-tools
  ;; Documents are still super blurry even after switching to the hi-dpi
  ;; imagemagick backend, docview actually looks better, but it's all pretty
  ;; terrible compared to preview.app :(
  :disabled t
  :straight t
  :hook (pdf-view-mode .  cua-mode)
  :custom
  (pdf-view-use-scaling t)
  (pdf-view-use-imagemagick t)
  (pdf-view-display-size 'fit-width)
  (pdf-annot-activate-created-annotations t)
  :init
  (pdf-tools-install))

(use-package org-noter
  ;; Couldn't get a working session to start from inside a document. Combined
  ;; with the poor rendering and the code complexity that one seems like a
  ;; non-starter...
  :straight t)

(use-package nov
  :straight t)

(use-package org
  :custom
  (org-link-frame-setup
   '((vm . vm-visit-folder-other-frame)
     (vm-imap . vm-visit-imap-folder-other-frame)
     (gnus . org-gnus-no-new-news)
     ;; was find-file-other-window
     (file . find-file)
     (wl . wl-other-frame))))

(use-package explain-pause-mode
  :straight (explain-pause-mode :type git :host github :repo "lastquestion/explain-pause-mode"))

(use-package restart-emacs :straight t :demand t)

;; Prose

(use-package freeze-it :straight t)


(use-package side-notes
  :straight t
  :custom
  (side-notes-file "notes.org")
  (side-notes-secondary-file "todos.org")
  :bind ("C-c n t" . side-notes-toggle-notes))


(provide 'config-wip)
;;; config-wip.el ends here
