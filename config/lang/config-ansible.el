;;; config-devops.el --- Ansible -*- lexical-binding: t; -*-

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
(require 'config-yaml)


(use-package ansible
  :ensure t
  :after yaml-mode
  :mode
  (("group_vars/.*" . yaml-mode)
   ("host_vars/.*"  . yaml-mode))
  :init (add-hook 'yaml-mode-hook #'ansible)
  :config
  (progn
    (defun config-devops-ansible-prog ()
      (run-hooks 'prog-mode-hook))
    (add-hook 'ansible-hook 'config-devops-ansible-prog)
    ;; Ansible vault
    (defvar config-devops-vault-pass-filename ".vault_pass")
    (defun config-devops-locate-vault-pass-file ()
      "Locate `config-devops-vault-pass-filename` from the current directory."
      (when-let ((dir (locate-dominating-file "." config-devops-vault-pass-filename)))
        (expand-file-name config-devops-vault-pass-filename dir)))
    (defun config-devops-set-ansible-vault-pass ()
      (setq ansible::vault-password-file (config-devops-locate-vault-pass-file)))
    (add-hook 'ansible-hook 'config-devops-set-ansible-vault-pass)
    (add-hook 'ansible-hook 'ansible::auto-decrypt-encrypt)))

(use-package jinja2-mode
  :ensure t
  :mode "\\.j2?\\'")

(use-package ansible-doc
  :ensure t
  :after ansible
  :init (add-hook 'ansible::hook #'ansible-doc-mode)
  :config
  (progn
    (bind-key "C-c C-d d" 'ansible-doc ansible-doc-mode-map)))

(use-package company-ansible
  :ensure t
  :after ansible
  :init
  (after config-completion
    (config-completion-add-backends
     'yaml-mode
     (config-completion-backend-with-yasnippet #'company-ansible))))

(provide 'config-ansible)
;;; config-ansible.el ends here
