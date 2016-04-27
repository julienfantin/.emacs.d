;;; config-parsers.el --- Syntax parsers integration  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Julien Fantin

;; Author: Julien Fantin(require 'use-config) <julienfantin@gmail.com>
;; Keywords: tools, convenience, extensions

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



;; * Flycheck

(use-package flycheck
  :ensure t
  :defer t
  :init (after-init #'global-flycheck-mode)
  :commands (flycheck-mode flycheck-list-errors)
  :defines
  (flycheck-error-list-buffer
   flycheck-display-errors-function)
  :functions
  (flycheck-buffer
   flycheck-list-errors
   flycheck-display-error-messages)
  :config
  (progn
    ;; Custom
    (setq flycheck-emacs-lisp-load-path 'inherit)
    ;; Advices
    (defun config-parsers-flycheck-select-window ()
      (select-window (get-buffer-window flycheck-error-list-buffer)))
    (advice-add #'flycheck-list-errors :after #'config-parsers-flycheck-select-window)
    ;; Conditionally disabled error messages
    (defvar config-parsers-flycheck-display-errors-function
      #'flycheck-display-error-messages)
    (defun config-parsers-flycheck-turn-messages-off (&optional _)
      (setq flycheck-display-errors-function nil))
    (defun config-parsers-flycheck-turn-messages-on (&optional _)
      (setq flycheck-display-errors-function
            config-parsers-flycheck-display-errors-function)
      (flycheck-buffer))
    (defun config-parsers-flycheck-edebug-toggle ()
      (if (bound-and-true-p edebug-mode)
          (config-parsers-flycheck-turn-messages-off)
        (config-parsers-flycheck-turn-messages-on)))
    ;; Edebug prints to the echo area as well
    (after 'edebug
      (add-hook 'edebug-mode-hook #'config-parsers-flycheck-edebug-toggle))
    ;; Let company display documentation in the modeline
    (after 'company
      (add-hook 'company-completion-started-hook #'config-parsers-flycheck-turn-messages-off)
      (add-hook 'company-completion-finished-hook #'config-parsers-flycheck-turn-messages-on)
      (add-hook 'company-completion-cancelled-hook #'config-parsers-flycheck-turn-messages-on))))


;; * Semantic

(use-package semantic
  :defer t
  :init (after-init #'semantic-mode)
  :functions
  (semanticdb-file-table-object
   semanticdb-save-all-db)
  :config
  (progn
    (defun config-parsers-semantic-parse-recursively (file-or-dir)
      "Recursively parse all files 'file-or-dir'"
      (cond
       ((null file-or-dir) nil)
       ((file-directory-p file-or-dir)
        (mapcar #'config-parsers-semantic-parse-recursively
                (directory-files-recursively file-or-dir ".+\\.el\\(\\.gz\\)?$")))
       (t (ignore-errors
            (semanticdb-file-table-object file-or-dir)))))
    (defun -semantic-parse-load-path ()
      "Parse all elisp files in 'load-path'"
      (interactive)
      (dolist (path load-path) (config-parsers-semantic-parse-recursively path))
      (semanticdb-save-all-db))))

(use-package semantic/db
  :defer t
  :defines
  (semanticdb-database-list)
  :config
  ;; Redefined as a fix for:
  ;; http://debbugs.gnu.org/cgi/bugreport.cgi?bug=22287
  (defun semanticdb-save-all-db-idle ()
    "Save all semantic tag databases from idle time.
Exit the save between databases if there is user input."
    (save-excursion
      (semantic-exit-on-input 'semanticdb-idle-save
        (mapc (lambda (db)
                (semantic-throw-on-input 'semanticdb-idle-save)
                (semanticdb-save-db db t))
              semanticdb-database-list)))))

(use-package semantic/db-file
  :defer t
  :config
  (setq semanticdb-default-save-directory (user-var-directory "semantic-db/")))

(provide 'config-parsers)
;;; config-parsers.el ends here
