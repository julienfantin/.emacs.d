;;; config-parsers.el --- Syntax parsers integration  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Julien Fantin

;; Author: Julien Fantin <julienfantin@gmail.com>
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
(require 'use-package)



;; * Flycheck

(use-package flycheck
  :straight t
  :init (after-init #'global-flycheck-mode)
  :commands (flycheck-mode flycheck-list-errors -counsel-flycheck)
  :bind (:map flycheck-mode-map
              ("C-c !" . -counsel-flycheck))
  :defines
  (flycheck-error-list-buffer
   flycheck-display-errors-function)
  :functions
  (flycheck-buffer
   flycheck-list-errors
   flycheck-display-error-messages
   flycheck-error-list-mode
   flycheck-error-pos
   flycheck-error-list-set-source
   flycheck-error-list-reset-filter
   config-parsers-flycheck-select-window
   config-parsers-flycheck-turn-messages-on
   config-parsers-flycheck-turn-messages-off)
  :config
  (progn
    ;; Advices
    (defun config-parsers-flycheck-select-window ()
      (select-window (get-buffer-window flycheck-error-list-buffer)))
    (advice-add #'flycheck-list-errors :after #'config-parsers-flycheck-select-window)
    ;; Conditionally disabled error messages
    (defvar config-parsers-flycheck-display-errors-function #'flycheck-display-error-messages)
    (defun config-parsers-flycheck-turn-messages-off (&optional _)
      (setq flycheck-display-errors-function nil))
    (defun config-parsers-flycheck-turn-messages-on (&optional _)
      (setq
       flycheck-display-errors-function config-parsers-flycheck-display-errors-function)
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
      (add-hook 'company-completion-cancelled-hook #'config-parsers-flycheck-turn-messages-on))
    (after 'counsel
      ;; https://github.com/nathankot/dotemacs/blob/ef76773c69cac36c04935edcfa631052bd2c679d/init.el#L566
      (defvar -counsel-flycheck-history nil
        "History for `-counsel-flycheck'")
      (defun -counsel-flycheck ()
        (interactive)
        (if (not (bound-and-true-p flycheck-mode))
            (message "Flycheck mode is not available or enabled")
          (ivy-read "Flycheck: "
                    (let ((source-buffer (current-buffer)))
                      (with-current-buffer (or (get-buffer flycheck-error-list-buffer)
                                               (progn
                                                 (with-current-buffer
                                                     (get-buffer-create flycheck-error-list-buffer)
                                                   (flycheck-error-list-mode)
                                                   (current-buffer))))
                        (flycheck-error-list-set-source source-buffer)
                        (flycheck-error-list-reset-filter)
                        (revert-buffer t t t)
                        (split-string (buffer-string) "\n" t " *")))
                    :action (lambda (s &rest _)
                              (-when-let* ( (error (get-text-property 0 'tabulated-list-id s))
                                            (pos (flycheck-error-pos error)) )
                                (goto-char (flycheck-error-pos error))))
                    :history '-counsel-flycheck-history)))))
  :custom
  (flycheck-emacs-lisp-load-path 'inherit))

(provide 'config-parsers)
;;; config-parsers.el ends here
