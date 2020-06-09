;;; config-font-lock.el --- Font-lock configuration
;;; Commentary:
;;
;;; Code:
(require 'use-package)


;; * Buffer colors

(use-package rainbow-mode
  :straight t
  :hook (help-mode info-mode emacs-lisp-mode ccs-mode web-mode))

(use-package rainbow-delimiters
  :straight t
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package page-break-lines
  :straight t
  :hook (prog-mode . page-break-lines-mode))

(use-package hl-todo
  :straight t
  :hook (prog-mode . hl-todo-mode))

(use-package highlight-numbers
  :straight t
  :hook (prog-mode . highlight-numbers-mode))

(provide 'config-font-lock)
;;; config-font-lock.el ends here
