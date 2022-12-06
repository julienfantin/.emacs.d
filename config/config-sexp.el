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

;;; Built-ins

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

;; Disabled Cmd-Ctrl-D on macos:
;; defaults write com.apple.symbolichotkeys AppleSymbolicHotKeys -dict-add 70 '<dict><key>enabled</key><false/></dict>'

(use-package puni
  :demand t
  :straight t
  :bind
  ( :map puni-mode-map
    ("C-)"   . puni-slurp-forward)
    ("C-("   . puni-slurp-backward)
    ("C-M-)" . puni-barf-forward)
    ("C-M-(" . puni-barf-backward)
    ("C-="   . puni-expand-region)
    ("M-k"   . puni-kill-line)
    ("M-r"   . puni-raise)
    ("M-s"   . puni-splice)
    ("C-M-t" . puni-transpose)))

(use-package puni-special
  :after puni
  :straight nil
  :after (lisp-minor-mode)
  :load-path "./lib"
  :hook
  ((lisp-minor-mode . puni-special-mode)
   (puni-special-mode . electric-pair-mode)))

(use-package puni-special-avy
  :straight nil
  :after (puni-special)
  :load-path "./lib"
  :demand t)

;;; Third-party

(use-package lisp-minor-mode
  :load-path "./lib"
  :hook (after-init . lisp-global-minor-mode))

(use-package pulse-eval
  :load-path "./lib"
  :after lisp-minor-mode
  :hook (lisp-minor-mode . pulse-eval-mode))

(provide 'config-sexp)
;;; config-sexp.el ends here
