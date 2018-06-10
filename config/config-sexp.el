;;; config-sexp.el --- Sexp-based editing            -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Julien Fantin

;; Author: Julien Fantin(require 'use-config) <julienfantin@gmail.com>
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
(require 'use-config)


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
  (show-paren-priority 1000)
  (show-paren-delay 0.05))


;; * Lisp minor-mode

(use-package lisp-minor-mode
  :commands (lisp-minor-mode lisp-global-minor-mode)
  :init (after-init #'lisp-global-minor-mode))


;; * Pulse eval

(use-package pulse-eval
  :after lisp-minor-mode
  :hook (lisp-minor-mode . pulse-eval-mode)
  :custom
  (pulse-eval-iterations 1)
  (pulse-eval-delay .13))


;; * Paredit

(use-package paredit
  :ensure t
  :commands paredit-mode
  :functions
  (paredit-wrap-round paredit-wrap-square paredit-wrap-curly)
  :commands
  (paredit-raise-sexp
   paredit-backward-barf-sexp
   paredit-forward-barf-sexp
   paredit-forward-slurp-sexp
   paredit-bacward-slurp-sexp
   paredit-wrap-round
   paredit-wrap-square
   paredit-wrap-curly
   paredit-kill
   paredit-splice-sexp)
  :init
  (progn
    (after 'lispy
      (defun paredit-wrap-curly-lispy ()
        "Workaround inconsistent point position after 'lispy-wrap-braces'."
        (interactive)
        (paredit-wrap-curly)
        (lispy-space 1))
      (defun paredit-wrap-round-from-behind ()
        (interactive)
        (forward-sexp -1)
        (paredit-wrap-round)
        (lispy-space 1))
      (defun paredit-wrap-square-from-behind ()
        (interactive)
        (forward-sexp -1)
        (paredit-wrap-square)
        (lispy-space 1))
      (defun paredit-wrap-curly-from-behind ()
        (interactive)
        (forward-sexp -1)
        (paredit-wrap-curly)
        (lispy-space 1))
      (let ((map lispy-mode-map))
        (bind-keys
         :map map
         ;; lispy's sometimes don't work when inside a sexp
         ([remap lispy-forward-slurp-sexp]  . paredit-forward-slurp-sexp)
         ([remap lispy-forward-barf-sexp]   . paredit-forward-barf-sexp)
         ([remap lispy-backward-slurp-sexp] . paredit-backward-slurp-sexp)
         ([remap lispy-backward-barf-sexp]  . paredit-backward-barf-sexp)
         ;; lispy's can leave unbalanced strings
         ([remap lispy-kill]                . paredit-kill)
         ;; lispy's doesn't work contextually in strings
         ([remap lispy-splice]              . paredit-splice-sexp))))

    (after 'lispy-mnemonic
      (bind-keys
       :map lispy-mnemonic-mode-map-special
       ("M-{" . paredit-wrap-curly-lispy)
       ("M-)" . paredit-wrap-round-from-behind)
       ("M-]" . paredit-wrap-square-from-behind)
       ("M-}" . paredit-wrap-curly-from-behind))
      (dolist (map (list lispy-mnemonic-mode-map
                         lispy-mnemonic-mode-map-base
                         lispy-mnemonic-mode-map-special))
        (bind-keys
         :map map
         ;; Delete trailing parens
         ("C-<backspace>"                   . backward-delete-char)
         ;; lispy's sometimes don't work when inside a sexp
         ([remap lispy-forward-slurp-sexp]  . paredit-forward-slurp-sexp)
         ([remap lispy-forward-barf-sexp]   . paredit-forward-barf-sexp)
         ([remap lispy-backward-slurp-sexp] . paredit-backward-slurp-sexp)
         ([remap lispy-backward-barf-sexp]  . paredit-backward-barf-sexp)
         ;; lispy's can leave unbalanced strings
         ([remap lispy-kill]                . paredit-kill)
         ;; lispy's doesn't work contextually in strings
         ([remap lispy-splice]              . paredit-splice-sexp))))))


;; * Lispy

(use-package lispy
  :ensure t
  :config
  (progn
    (after 'outline-magic
      (define-key lispy-mode-map [remap lispy-outline-left]  'outline-promote)
      (define-key lispy-mode-map [remap lispy-outline-right] 'outline-demote))
    (after 'pulse-eval
      (add-to-list
       'pulse-eval-advices-alist
       (cons 'lispy-mode '((lispy-eval . pulse-eval-highlight-forward-sexp-advice))))))
  :custom
  (lispy-no-permanent-semantic t)
  (lispy-close-quotes-at-end-p t)
  (lispy-eval-display-style 'overlay)
  (lispy-visit-method 'projectile)
  (lispy-compat '(edebug cider))
  (lispy-avy-style-char 'at-full)
  (lispy-avy-style-paren 'at-full)
  (lispy-avy-style-symbol 'at-full)
  (lispy-safe-copy t)
  (lispy-safe-delete t)
  (lispy-safe-paste t))

(use-package lispy-mnemonic
  :commands lispy-mnemonic-mode
  :after lisp-minor-mode
  :hook (lisp-minor-mode . lispy-mnemonic-mode)
  :config
  (after 'outline-magic
    ;; lispy's outline promotion is hardwired to it's comment convention
    (define-key lispy-mnemonic-mode-map [remap lispy-outline-left] 'outline-promote)
    (define-key lispy-mnemonic-mode-map [remap lispy-outline-right] 'outline-demote)))

(provide 'config-sexp)
;;; config-sexp.el ends here
