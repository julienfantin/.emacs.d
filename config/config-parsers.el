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
(require 'config-completion)

;;; Built-ins

(use-package edebug
  :after flycheck
  :config
  (defun config-parsers-flycheck-edebug-toggle ()
    "Turn off flycheck while edebug is on since they conflict over the echo area."
    (if (bound-and-true-p edebug-mode)
        (config-parsers-flycheck-turn-messages-off)
      (config-parsers-flycheck-turn-messages-on)))
  :hook ((edebug-mode . config-parsers-flycheck-edebug-toggle)))

;;; Third-party

(use-package flycheck
  :straight t
  :hook ((prog-mode . flycheck-mode)
         (flycheck-mode . flycheck-set-indication-mode))
  :config
  (progn
    ;; Advices
    (defun config-parsers-flycheck-select-window ()
      (select-window (get-buffer-window flycheck-error-list-buffer)))
    (advice-add #'flycheck-list-errors :after #'config-parsers-flycheck-select-window)
    ;; Conditionally disabled err messages
    (defvar config-parsers-flycheck-display-errors-function #'flycheck-display-error-messages)
    (defun config-parsers-flycheck-turn-messages-off (&optional _)
      (setq flycheck-display-errors-function nil))
    (defun config-parsers-flycheck-turn-messages-on (&optional _)
      (setq flycheck-display-errors-function config-parsers-flycheck-display-errors-function)
      (flycheck-buffer)))
  :custom
  (flycheck-emacs-lisp-load-path 'inherit)
  (flycheck-indication-mode 'left-fringe))

(use-package flycheck-posframe
  :straight t
  :hook (flycheck-mode . flycheck-posframe-mode))

(use-package company
  :disabled t
  ;; Let company display documentation in the modeline
  :hook (((company-completion-started  . config-parsers-flycheck-turn-messages-off)
          (company-completion-finished  . config-parsers-flycheck-turn-messages-on)
          (company-completion-cancelled  . config-parsers-flycheck-turn-messages-on))))

(use-package counsel
  :if (equal config-completion-system 'ivy)
  :after flycheck
  :bind (("C-c l" . -counsel-flycheck))
  :preface
  (defvar -counsel-flycheck-history nil
    "History for `-counsel-flycheck'")
  :config
  ;; https://github.com/nathankot/dotemacs/blob/ef76773c69cac36c04935edcfa631052bd2c679d/init.el#L566
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
                :action (lambda (_s &rest _)
                          (-when-let* ((err (get-text-property 0 'tabulated-list-id s))
                                       (pos (flycheck-error-pos err)) )
                            (goto-char (flycheck-error-pos err))))
                :history '-counsel-flycheck-history))))

(provide 'config-parsers)
;;; config-parsers.el ends here
