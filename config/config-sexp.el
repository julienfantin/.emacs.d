;;; config-sexp.el --- Sexp-based editing            -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Julien Fantin

;; Author: Julien Fantin <julienfantin@gmail.com>
;; Keywords: lisp, convenience

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


;; * Built-ins

(use-package paren
  :preface
  (progn
    (defvar config-sexp-show-paren nil)
    (defun config-sexp-show-paren-turn-off ()
      (when (bound-and-true-p show-paren-mode)
        (setq config-sexp-show-paren t)
        (show-paren-mode -1)))
    (defun config-sexp-show-paren-turn-on ()
      (when (bound-and-true-p config-sexp-show-paren)
        (setq config-sexp-show-paren nil)
        (show-paren-mode 1))))
  :hook
  ((prog-mode                                . show-paren-mode)
   ((activate-mark pulse-eval-before-pulse)  . config-sexp-show-paren-turn-off)
   ((deactivate-mark pulse-eval-after-pulse) . config-sexp-show-paren-turn-on))
  :custom
  (show-paren-style 'expression)
  (show-paren-delay 0))


;; * Lisp minor-mode

(use-package lisp-minor-mode
  :hook (after-init . lisp-global-minor-mode)
  :custom
  (lisp-minor-mode-prettify nil))


;; * Pulse eval

(use-package pulse-eval
  :load-path "./lib"
  :after lisp-minor-mode
  :hook (lisp-minor-mode . pulse-eval-mode)
  :custom
  (pulse-eval-iterations 1)
  (pulse-eval-delay .2))

(use-package pulse-eval
  :after (pulse-eval lispy)
  :config
  (add-to-list
   'pulse-eval-advices-alist
   (cons 'lispy-mode '((lispy-eval . pulse-eval-highlight-forward-sexp-advice)))))


;; * Paredit

(use-package paredit
  :disabled t
  :straight t
  :after (lisp-minor-mode)
  :hook (lisp-minor-mode . paredit-mode))

(use-package lispy
  :straight t
  :after (lisp-minor-mode)
  :hook (lisp-minor-mode . lispy-mode)
  :bind
  (;; Some of the commands in the paredit map are not right
   :map lispy-mode-map-paredit
   ("C-M-f" . forward-sexp)
   ("C-M-b" . backward-sexp)
   :map lispy-mode-map
   ;; Tweak the lispy map from hjkl to a keyboard arrows shape
   ("h" . nil)
   ("i" . special-lispy-up)
   ("k" . special-lispy-down)
   ("j" . special-lispy-left)
   ("l" . special-lispy-right))
  :custom
  (lispy-no-permanent-semantic t)
  (lispy-close-quotes-at-end-p t)
  (lispy-eval-display-style 'overlay)
  (lispy-visit-method 'projectile)
  (lispy-compat '(edebug cider))
  ;; Get the hints off of the symbols!
  (lispy-avy-style-char 'at-full)
  (lispy-avy-style-paren 'at-full)
  (lispy-avy-style-symbol 'at-full)
  (lispy-safe-actions-no-pull-delimiters-into-comments t)
  (lispy-safe-copy t)
  (lispy-safe-delete t)
  (lispy-safe-paste t)
  :config
  ;; Enable the paredit map
  (lispy-set-key-theme '(special c-digit lispy paredit))
  ;; without this, lispy's special wrapping of "/" for lispy-splice, overrides
  ;; cljr-slash so that / just self-inserts
  ;; TODO make that a local change?
  (lispy-define-key lispy-mode-map "/" 'lispy-splice :inserter 'cljr-slash)
  (defun lispy-mini-buffer ()
    (when (eq this-command 'eval-expression)
      (lispy-mode 1)))
  (add-hook 'minibuffer-setup-hook 'lispy-mini-buffer))



(provide 'config-sexp)
;;; config-sexp.el ends here
