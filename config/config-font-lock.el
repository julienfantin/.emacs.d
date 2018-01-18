;;; config-font-lock.el --- Font-lock configuration
;;; Commentary:
;;
;;; Code:
(require 'use-config)


;; * Buffer colors

(use-package rainbow-mode
  :ensure t
  :hook (emacs-lisp-mode ccs-mode web-mode))

(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package page-break-lines
  :ensure t
  :hook (prog-mode . page-break-lines-mode))

(use-package hl-todo
  :ensure t
  :hook (prog-mode . hl-todo-mode))

(use-package highlight-numbers
  :ensure t
  :hook (prog-mode . highlight-numbers-mode))

(provide 'config-font-lock)
;;; config-font-lock.el ends here
