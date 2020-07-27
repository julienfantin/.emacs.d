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

(require 'use-package)
(require 'subr-x)

;;; Third-party

(use-package tuareg
  :straight t
  :after (opam)
  :mode (("\\.ml[ily]?$" . tuareg-mode)
         ("\\.topml$" . tuareg-mode))
  :bind (:map tuareg-mode-map
              ;; ("C-M-f" . smie-forward-sexp-command)
              ;; ("C-M-b" . smie-backward-sexp-command)
              ("C-c C-k" . tuareg-eval-buffer)
              ("C-c C-z" . utop)))

(use-package opam
  :ensure-system-package opam
  :straight t
  :after tuareg
  :preface
  (defun config-ocaml-add-opam-site-lisp-to-load-path ()
    "Add the site-lisp for the current opam switch to the `load-path'."
    (when-let
        ((opam-switch (string-trim (shell-command-to-string "opam config var prefix")))
         (opam-site-lisp (expand-file-name "share/emacs/site-lisp" opam-switch)))
      (add-to-list 'load-path opam-site-lisp)))
  :init
  (config-ocaml-add-opam-site-lisp-to-load-path)
  (opam-init))

(use-package utop
  :straight t
  :ensure-system-package (utop . "opam install utop")
  :after (tuareg opam)
  :hook (tuareg-mode . utop-minor-mode)
  :commands utop-minor-mode
  :custom
  (utop-edit-command nil)
  :hook
  ((tuareg-mode reason-mode) . utop-minor-mode)
  (tuareg-mode
   . (lambda ()
       (setq utop-command "utop -emacs")
       (setq utop-prompt
             (lambda ()
               (let ((prompt (format "utop[%d]> " utop-command-number)))
                 (add-text-properties 0 (length prompt) '(face utop-prompt) prompt)
                 prompt)))))
  (reason-mode
   . (lambda ()
       (setq utop-command "rtop -emacs")
       (setq utop-prompt
             (lambda ()
               (let ((prompt (format "rtop[%d]> " utop-command-number)))
                 (add-text-properties 0 (length prompt) '(face utop-prompt) prompt)
                 prompt))))))

(use-package ocp-indent
  :ensure-system-package (ocp-indent . "opam install ocp-indent")
  :after (tuareg opam)
  :commands (ocp-indent-caml-mode-setup)
  :hook (tuareg-mode . ocp-indent-caml-mode-setup))

(use-package merlin-company
  :after (merlin company))

(use-package merlin-iedit
  :after (merlin)
  :bind (:map merlin-mode-map
              ("C-c C-e" . merlin-iedit-occurrences)))

(use-package merlin-eldoc
  :straight t
  :after (merlin)
  :hook ((reason-mode tuareg-mode caml-mode) . merlin-eldoc-setup))

(use-package flycheck-ocaml
  :straight t
  :after (merlin)
  :init (flycheck-ocaml-setup)
  :custom
  (merlin-error-after-save nil)
  (flycheck-check-syntax-automatically '(save mode-enabled)))

(provide 'config-ocaml)
;;; config-ocaml.el ends here
