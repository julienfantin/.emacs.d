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

(defvar config-parsers-backend 'flymake)

;;; Built-ins

(use-package edebug
  :disabled t
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
  :if (eq config-parsers-backend 'flycheck)
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
  (flycheck-indication-mode 'left-margin))

(use-package flycheck-posframe
  :if (eq config-parsers-backend 'flycheck)
  :after (flycheck)
  :straight t
  :hook (flycheck-mode . flycheck-posframe-mode))

;; Flymake

(use-package flymake-popon
  :straight (flymake-popon :type git :repo "https://codeberg.org/akib/emacs-flymake-popon.git")
  :if (eq config-parsers-backend 'flymake)
  :hook (flymake-mode . flymake-popon-mode)
  :custom
  (flymake-popon-mode 'posframe)
  ;; avoid overlap with lsp-ui-mode
  (flymake-popon-posframe-extra-arguments
   '( :poshandler posframe-poshandler-point-bottom-left-corner
      :internal-border-width 20)))

(provide 'config-parsers)
;;; config-parsers.el ends here
