;;; config-nix.el --- Nix derivations editing  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Julien Fantin

;; Author: Julien Fantin <julienfantin@gmail.com>
;; Keywords:

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

(use-package nix-mode
  :ensure t
  :config
  (progn
    (after 'paredit
      (add-hook 'nix-mode-hook #'paredit-mode))
    (after 'aggressive-indent
      (add-to-list 'aggressive-indent-excluded-modes 'nix-mode))))

(use-package nixos-options :ensure t)

(use-package company-nixos-options
  :ensure t
  :init
  (after 'config-completion
    (add-to-list 'config-completion-backends-alist
                 (cons 'nix-mode '(company-nixos-options)))))

(use-package nix-sandbox :ensure t)

(after (ivy nix-mode)
  (require 'nixos-options)
  (defun nixos-options-show-description (f)
    "Show description buffer for NixOS option 'f'."
    (switch-to-buffer-other-window
     (nixos-options-doc-buffer
      (nixos-options-get-documentation-for-option f))))
  (defun ivy-nixos-options ()
    "NixOs options with ivy completion."
    (interactive)
    (ivy-read "Nixos Options:" nixos-options :action #'nixos-options-show-description))
  (ivy-set-actions
   'ivy-nixos-options
   '(("i" (lambda (f) (insert (nixos-options-get-name f))) "insert"))))

;; TODO Flycheck integration:
;; (setq flycheck-command-wrapper-function
;;       (lambda (command) (apply 'nix-shell-command (nix-current-sandbox) command))
;;       flycheck-executable-find
;;       (lambda (cmd) (nix-executable-find (nix-current-sandbox) cmd)))

(provide 'config-nix)
;;; config-nix.el ends here
