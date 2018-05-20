;;; config-emacs-lisp.el --- Emacs lisp programming  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Julien Fantin

;; Author: Julien Fantin(require 'use-config) <julienfantin@gmail.com>
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
(require 'use-config)
(require 'map)


;; * Builtins

(use-package elisp-mode
  :config
  (bind-key "C-c C-k" #'-eval-buffer emacs-lisp-mode-map)
  (bind-key "C-c C-p" #'pp-eval-last-sexp emacs-lisp-mode-map))

(after (simple paredit)
  (add-hook 'eval-expression-minibuffer-setup-hook #'eldoc-mode)
  (add-hook 'eval-expression-minibuffer-setup-hook #'paredit-mode))

(after 'flycheck
  (add-to-list 'flycheck-disabled-checkers 'emacs-lisp-checkdoc))


;; * Packages

(use-package lisp-extra-font-lock
  :ensure t
  :after lisp
  :config (lisp-extra-font-lock-global-mode 1))

(use-package elisp-slime-nav
  :ensure t
  :commands (turn-on-elisp-slime-nav-mode)
  :init (add-hook 'emacs-lisp-mode-hook #'turn-on-elisp-slime-nav-mode))

(use-package nameless
  :ensure t
  :init (add-hook 'emacs-lisp-mode-hook #'nameless-mode)
  :commands (nameless-insert-name)
  :config
  (bind-key "C-c C--" #'nameless-insert-name emacs-lisp-mode-map)
  (validate-setq
   nameless-affect-indentation-and-filling nil
   ;; the : prefix makes it hard to distinguish between function calls and
   ;; plists
   nameless-prefix "/"))

(use-package sotlisp
  :ensure t
  :commands sotlisp-turn-on-everywhere
  :init (sotlisp-turn-on-everywhere)
  :config
  (progn
    (add-hook 'emacs-lisp-mode-hook #'speed-of-thought-mode)
    (unbind-key "C-c f" sotlisp-mode-map)
    (unbind-key "C-c v" sotlisp-mode-map)))

(use-package auto-compile
  :ensure t
  :commands auto-compile-on-save-mode
  :init (add-hook 'emacs-lisp-mode-hook #'auto-compile-on-save-mode)
  :config
  (setq auto-compile-display-buffer nil
        auto-compile-use-mode-line nil)
  (defun -auto-compile-load-after-compile (success)
    "Reload the current emacs-lisp file after it's recompiled, if
an older version is loaded."
    (when (eq success t)
      (let ((buffer-path (file-truename buffer-file-name)))
        (when (assoc buffer-path load-history)
          (load-file buffer-path)))))
  (advice-add #'auto-compile-byte-compile :filter-return #'-auto-compile-load-after-compile))


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
