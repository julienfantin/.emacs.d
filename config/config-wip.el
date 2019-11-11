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

(require 'use-config)

(add-to-list 'exec-path "~/bin")
(setq default-truncate-lines t)

;; * Packages fixes

(after 'cider-debug
  (defun -cider-debug-no-eldoc ()
    (if (bound-and-true-p cider--debug-mode)
        (eldoc-mode -1)
      (eldoc-mode 1)))
  (add-hook 'cider--debug-mode-hook '-cider-debug-no-eldoc))


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

(advice-add #'load-theme :after #'fix-show-parent-match)


;; * Keybindings

(global-set-key (kbd "C-x =") 'balance-windows-area)

(defvar start-file "~/org/todo.org")
(defun open-start-file () (find-file start-file))
(after-init #'open-start-file)


(provide 'config-wip)
;;; config-wip.el ends here