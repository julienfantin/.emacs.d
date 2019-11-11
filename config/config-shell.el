;;; config-shell.el --- Interactive shells           -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Julien Fantin

;; Author: Julien Fantin <julienfantin@gmail.com>
;; Keywords: tools, unix, terminals

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


;; * Comint

(use-package comint
  :custom (comint-prompt-read-only t))


;; * Eshell

(use-package esh-mode
  :custom
  (eshell-scroll-to-bottom-on-output 'this)
  (eshell-scroll-show-maximum-output t))

(use-package em-unix
  :custom
  (eshell-cp-interactive-query t)
  (eshell-ln-interactive-query t)
  (eshell-mv-interactive-query t)
  (eshell-rm-interactive-query t)
  (eshell-mv-overwrite-files nil))

(use-package em-cmpl
  :after eshell
  :custom (eshell-cmpl-ignore-case t))

(use-package em-term
  :after eshell
  :custom
  (eshell-destroy-buffer-when-process-dies t)
  :config
  (setq eshell-visual-commands
        (append '("tmux" "screen" "ssh" "htop" "git log") eshell-visual-commands)))

(use-package em-hist
  :after eshell
  :bind (:map eshell-mode-map
              ("M-r" . counsel-esh-history))
  :custom (eshell-hist-ignoredups t))


;; * Packages

(use-package eshell-z
  :straight t
  :after eshell
  :init (require 'eshell-z nil t))

(use-package eshell-prompt-extras
  :straight t
  :after em-prompt
  :commands epe-theme-lambda
  :custom
  (eshell-highlight-prompt t)
  (eshell-prompt-function #'epe-theme-lambda))

(provide 'config-shell)
;;; config-shell.el ends here
