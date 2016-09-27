;;; config-ocaml.el --- Ocaml programming            -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Julien Fantin

;; Author: Julien Fantin <julienfantin@gmail.com>
;; Keywords: languages

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

;; ocp-indent, merlin and utop all have emacs-lisp code shipped as part of their
;; opam packages.  It's prefereable to use those files rather than elpa packages
;; in order to ensure compatibility between Emacs and the external tools.
;;
;; TODO: Robust way to sync with `opam switch` commands?

;;; Code:
(require 'use-config)
(require 'subr-x)


;; * Ocaml dev environment

(use-package opam
  :ensure t
  :defer t
  :preface
  (defun add-opam-load-path ()
    (interactive)
    (when-let ((opam-switch (string-trim (shell-command-to-string "opam config var prefix"))))
      (add-to-list 'load-path (expand-file-name "share/emacs/site-lisp" opam-switch))))
  :init
  (after 'tuareg
    (opam-init)
    (add-opam-load-path)))

(use-package utop
  :defer t
  :commands utop-minor-mode
  :init
  (after 'tuareg
    (add-hook 'tuareg-mode-hook 'utop-minor-mode))
  :config
  (after 'paredit
    (add-hook 'utop-mode-hook 'paredit-mode)
    (add-hook 'utop-mode-hook  #'(lambda () (ocaml-bind-paredit utop-mode-map)))))

(use-package ocp-indent
  :defer t
  :init
  (after 'tuareg
    (require 'ocp-indent nil t)))

;; * Emacs tools

(use-package tuareg
  :ensure t
  :defer t
  :preface
  (defun ocaml-bind-paredit (keymap)
    (bind-keys
     :map keymap
     ("C-M-f" . smie-forward-sexp-command)
     ("C-M-b" . smie-backward-sexp-command)
     ("[" . paredit-open-square)
     ("]" . paredit-close-square)
     ("{" . paredit-open-curly)
     ("}" . paredit-close-curly)
     ("}" . paredit-close-curly)
     ("<backspace>" . paredit-backward-delete)))
  :commands esk-tuareg-eval
  :mode (("\\.ml[ily]?$" . tuareg-mode)
         ("\\.topml$" . tuareg-mode))
  :config
  (progn
    (after 'paredit
      (add-hook 'tuareg-mode-hook 'paredit-mode)
      (add-hook 'tuareg-mode-hook  #'(lambda () (ocaml-bind-paredit tuareg-mode-map)))
      (bind-keys
       :map tuareg-mode-map
       ("RET" . reindent-then-newline-and-indent)
       ("C-c C-k" . tuareg-eval-buffer)
       ("C-c C-s" . utop)))))

(use-package merlin
  :defer t
  :init
  (progn
    ;; NOTE: from the merlin docs
    ;;
    ;; To use merlin-locate to go to the source of things installed with
    ;; opam, you first of all need to keep the source around when
    ;; installing, and let opam create .cmt files:
    ;; Set this in ~/.bash_profile:
    ;; export OPAMKEEPBUILDDIR=true
    ;; export OCAMLPARAM="_,bin-annot=1"
    ;; export OPAMBUILDDOC=true
    (setenv "OPAMKEEPBUILDDIR" "true")
    (setenv "OCAMLPARAM" "_,bin-annot=1")
    (setenv "OPAMBUILDDOC" "true")
    (after 'tuareg
      (add-hook 'tuareg-mode-hook 'merlin-mode)))
  :config
  (after 'tuareg
    (use-package merlin-company :defer t)
    (validate-setq
     merlin-completion-types nil
     merlin-completion-arg-type nil
     merlin-completion-with-doc t
     merlin-completion-dwim nil
     merlin-error-after-save nil
     merlin-command 'opam
     merlin-default-flags '("-principal"))
    (define-key merlin-mode-map (kbd "C-c <up>") 'merlin-type-enclosing-go-up)
    (define-key merlin-mode-map (kbd "C-c <down>") 'merlin-type-enclosing-go-down)))

(use-package flycheck-ocaml
  :ensure t
  :defer t
  :init
  (after 'merlin
    (flycheck-ocaml-setup))
  :config
  (after 'merlin
    (validate-setq merlin-error-after-save nil)))

(provide 'config-ocaml)
;;; config-ocaml.el ends here
