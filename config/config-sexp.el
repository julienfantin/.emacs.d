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
  :defer t
  :init (add-hook 'prog-mode-hook #'show-paren-mode)
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
  :config
  (progn
    (setq show-paren-style 'expression
          show-paren-priority 1000
          show-paren-delay 0)
    (add-hook 'activate-mark-hook #'(lambda () (show-paren-mode -1)))
    (add-hook 'deactivate-mark-hook #'(lambda () (show-paren-mode 1)))
    (after 'pulse-eval
      (add-hook 'pulse-eval-before-pulse-hook #'config-sexp-show-paren-turn-off)
      (add-hook 'pulse-eval-after-pulse-hook #'config-sexp-show-paren-turn-on))))


;; * Lisp minor-mode

(use-package lisp-minor-mode
  :commands (lisp-minor-mode lisp-global-minor-mode)
  :init (after-init #'lisp-global-minor-mode))


;; * Pulse eval

(use-package pulse-eval
  :defer t
  :commands pulse-eval-mode
  :init
  (after 'lisp-minor-mode
    (add-hook 'lisp-minor-mode-hook #'pulse-eval-mode)))


;; * Paredit

(use-package paredit
  :ensure t
  :defer t
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
         ;; Prefer our global binding for AHS
         ("M-i" . nil)
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
         ;; Prefer our global binding for AHS
         ("M-i"                             . nil)
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
  :defer t
  :config
  (progn
    (after 'outline-magic
      (define-key lispy-mode-map [remap lispy-outline-left]  'outline-promote)
      (define-key lispy-mode-map [remap lispy-outline-right] 'outline-demote))
    (after 'pulse-eval
      (add-to-list
       'pulse-eval-advices-alist
       (cons 'lispy-mode '((lispy-eval . pulse-eval-highlight-forward-sexp-advice)))))
    (setq lispy-no-permanent-semantic t
          lispy-completion-method 'ivy
          lispy-visit-method 'projectile
          lispy-compat '(edebug cider)
          lispy-avy-style-char 'at-full
          lispy-avy-style-paren 'at-full
          lispy-avy-style-symbol 'at-full)))

(use-package lispy-mnemonic
  :after lispy
  ;; The autoload in the file doesn't work?
  :commands lispy-mnemonic-mode
  :init
  (after 'lisp-minor-mode
    (add-hook 'lisp-minor-mode-hook #'lispy-mnemonic-mode t))
  :config
  (after 'outline-magic
    ;; lispy's outline promotion is hardwired to it's comment convention
    (define-key lispy-mnemonic-mode-map [remap lispy-outline-left] 'outline-promote)
    (define-key lispy-mnemonic-mode-map [remap lispy-outline-right] 'outline-demote)))


(defun config-sexp--closest (regex)
  "Return point for the closest instance of 'REGEX'."
  (let ((f (save-excursion (re-search-forward regex) (point)))
        (b (save-excursion (re-search-backward regex) (point))))
    (cond
     ((and (null f) (null b)) (error "Not found"))
     ((null f) b)
     ((null b) f)
     ((< (abs (- (point) f)) (abs (- (point) b))) f)
     (t b))))


;; * Commands

;;;###autoload
(defun -goto-closest-left-paren ()
  "Move point to the closest opening paren."
  (interactive)
  (goto-char (- (config-sexp--closest "\(") 1)))

(provide 'config-sexp)
;;; config-sexp.el ends here
