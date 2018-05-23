;;; config-reason.el --- ReasonML config             -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Julien Fantin

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
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:
(require 'use-config)
(require 'config-ocaml)

(defun shell-cmd (cmd)

  (car (ignore-errors (apply 'process-lines (split-string cmd)))))

(use-package reason-mode
  :ensure t
  :after (refmt)
  :ensure-system-package
  ((reason-cli-esy-sandbox . "npm install -g reason-cli@3.1.0-darwin")
   (bsb                    . "npm install -g bs-platform"))
  :hook ((reason-mode . refmt-mode)
         (reason-mode . merlin-mode)))

(use-package refmt
  :defer t
  :preface
  (define-minor-mode refmt-mode
    "Refmt minor-mode"
    :lighter "refmt"
    :keymap (let ((m (make-sparse-keymap)))
              (define-key m (kbd "C-M-\\") #'refmt)
              m)
    (if refmt-mode
        (add-hook 'before-save-hook #'refmt-before-save)
      (remove-hook 'before-save-hook #'refmt-before-save)))
  :config
  (when-let ((cmd (or (shell-cmd "refmt ----where") (shell-cmd "which refmt"))))
    (setq refmt-command cmd)))

(provide 'lang-reasonml)
;;; config-reason.el ends here
