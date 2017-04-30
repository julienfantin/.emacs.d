;;; config-indentation.el --- Indentation configuration  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Julien Fantin

;; Author: Julien Fantin(require 'use-config) <julienfantin@gmail.com>
;; Keywords: languages, convenience

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


;; * Customs

(defvar config-indentation-aggressive-indent-max-lines 100)


;; * Spaces over tabs

(setq-default indent-tabs-mode nil)

(use-package makefile
  :defer t
  :preface
  (defun config-indentation-makefile ()
    (set (make-local-variable 'indent-tabs-mode) t)
    (set (make-local-variable 'tab-width) 4))
  :config (add-hook 'makefile-mode-hook 'config-indentation-makefile))


;; * Whitespace cleanup

(defun config-whitespace-show-trailing-whitespace ()
  "Enable 'SHOW-TRAILING-WHITESPACE' in current buffer."
  (setq-local show-trailing-whitespace t))

(add-hook 'prog-mode-hook #'config-whitespace-show-trailing-whitespace)

(use-package clean-aindent-mode
  :ensure t
  :defer t
  :init (add-hook 'prog-mode-hook #'clean-aindent-mode))

(use-package ws-butler
  :ensure t
  :defer t
  :commands ws-butler-mode
  :init (add-hook 'prog-mode-hook #'ws-butler-mode))


;; * Indentation

(use-package dtrt-indent
  :ensure t
  :defer t
  :after cc-mode
  :init (add-hook 'cc-mode-hook #'dtrt-indent-mode)
  :config
  (validate-setq dtrt-indent-verbosity 0))

(use-package aggressive-indent
  :ensure t
  :defer t
  :preface
  (defun config-indentation-aggressive-indent-skip-p ()
    "Return true if the current defun is longer than
'config-indentation-aggressive-indent-max-lines'."
    (save-excursion
      (ignore-errors
        (let ((b (progn (beginning-of-defun) (line-number-at-pos)))
              (e (progn (end-of-defun) (line-number-at-pos))))
          (and b e (<= config-indentation-aggressive-indent-max-lines (- e b)))))))
  :commands aggressive-indent-mode
  :init (after-init 'global-aggressive-indent-mode)
  :config
  (progn
    (validate-setq aggressive-indent-comments-too t)
    ;; Skip large forms
    (add-to-list 'aggressive-indent-dont-indent-if '(config-indentation-aggressive-indent-skip-p))
    ;; Disabled commands
    (dolist (command '(next-line previous-line))
      (add-to-list 'aggressive-indent-protected-commands command))
    ;; Disabled modes
    (dolist (mode '(makefile-mode tuareg-mode cider-repl-mode))
      (add-to-list 'aggressive-indent-excluded-modes mode))))

(provide 'config-indentation)
;;; config-indentation.el ends here
