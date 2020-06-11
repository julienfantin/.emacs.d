;;; config-emacs-lisp.el --- Emacs lisp programming  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Julien Fantin

;; Author: Julien Fantin <julienfantin@gmail.com>
;; Keywords: languages, lisp

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
(require 'map)


;; * Builtins

(use-package elisp-mode
  :bind
  (:map emacs-lisp-mode-map
        ("C-c C-k" . -eval-buffer)
        ("C-c C-p" . pp-eval-last-sexp)))

(use-package simple
  :config
  (add-hook 'eval-expression-minibuffer-setup-hook #'eldoc-mode))

(use-package paredit
  :config
  (add-hook 'eval-expression-minibuffer-setup-hook #'paredit-mode))

(use-package flycheck
  :config
  (add-to-list 'flycheck-disabled-checkers 'emacs-lisp-checkdoc))


;; * Packages

(use-package lisp-extra-font-lock
  :straight t
  :after lisp
  :config (lisp-extra-font-lock-global-mode 1))

(use-package elisp-def
  :straight t
  :hook ((emacs-lisp-mode . elisp-def-mode)
         (ielm-mode . elisp-def-mode)))

(use-package nameless
  :straight t
  :hook (emacs-lisp-mode . nameless-mode)
  :commands (nameless-insert-name)
  :bind (:map emacs-lisp-mode-map
              ("C-c C--" . nameless-insert-name))
  :custom
  (nameless-affect-indentation-and-filling nil)
  ;; the : prefix makes it hard to distinguish between function calls and
  ;; plists
  (nameless-prefix "/"))

(use-package auto-compile
  :straight t
  :commands auto-compile-on-save-mode
  :hook (emacs-lisp-mode . auto-compile-on-save-mode)
  :config
  (progn
    (defun -auto-compile-load-after-compile (success)
      "Reload the current emacs-lisp file after it's recompiled, if
an older version is loaded."
      (when (eq success t)
        (let ((buffer-path (file-truename buffer-file-name)))
          (when (assoc buffer-path load-history)
            (load-file buffer-path)))))
    (advice-add #'auto-compile-byte-compile :filter-return #'-auto-compile-load-after-compile))
  :custom
  (auto-compile-display-buffer nil)
  (auto-compile-use-mode-line nil))

(use-package elisp-demos
  :straight t
  :after helpful
  :init (advice-add 'helpful-update :after #'elisp-demos-advice-helpful-update))


;; * Commands

(defun -eval-buffer (arg)
  "Execute the current buffer as Lisp code.
Top-level forms are evaluated with `eval-defun' so that `defvar'
and `defcustom' forms reset their default values."
  (interactive "P")
  (if (equal arg (list 4))
      (save-excursion
        (goto-char (point-min))
        (while (not (eobp))
          (forward-sexp)
          (eval-defun nil))
        (message "Redefined buffer!"))
    (eval-buffer)))

(provide 'config-emacs-lisp)
;;; config-emacs-lisp.el ends here
