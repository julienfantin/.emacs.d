;;; config-font-lock.el --- Font-lock configuration
;;; Commentary:
;;
;;; Code:
(require 'use-config)


;; * Buffer colors

(use-package rainbow-mode
  :ensure t
  :defer t
  :commands rainbow-mode
  :init
  (progn
    (after 'elisp-mode
      (add-hook 'emacs-lisp-mode-hook 'rainbow-mode))
    (after 'css
      (add-hook 'css-mode-hook 'rainbow-mode))
    (after 'web-mode
      (add-hook 'web-mode-hook 'rainbow-mode))))

(use-package rainbow-delimiters
  :ensure t
  :defer t
  :commands rainbow-delimiters-mode
  :init (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(use-package page-break-lines
  :disabled t
  :ensure t
  :defer t
  :commands (page-break-lines-mode)
  :functions (page-break-lines--update-display-tables)
  :init (add-hook 'prog-mode-hook #'page-break-lines-mode)
  :preface
  (defun config-font-lock-set-page-break-line-height ()
    "Fix interaction with company overlay.
  https://github.com/purcell/page-break-lines/issues/3"
    (set-fontset-font
     "fontset-default"
     (cons page-break-lines-char page-break-lines-char)
     (face-attribute 'default :family))
    (set-face-attribute
     'page-break-lines nil
     :height (face-attribute 'default :height nil 'default)))
  (defun config-font-lock-page-break-over-tooltip-p ()
    (when (boundp 'company-tooltip-limit)
      (let ((to (save-excursion (forward-line company-tooltip-limit) (point))))
        (save-excursion
          (numberp (re-search-forward "" to t))))))
  (defun config-font-lock-maybe-turn-off-page-breaks (&optional _)
    (when (and (bound-and-true-p page-break-lines-mode)
               (config-font-lock-page-break-over-tooltip-p))
      (put 'page-break-lines-mode :config-toggle t)
      (page-break-lines-mode -1)))
  (defun config-font-lock-maybe-turn-on-page-breaks (&optional _)
    (unless (or (bound-and-true-p page-break-lines-mode)
                (null (get 'page-break-lines-mode :config-toggle)))
      (put 'page-break-lines-mode :config-toggle nil)
      (page-break-lines-mode 1)))
  :config
  (progn
    (add-hook 'page-break-lines-mode-hook #'config-font-lock-set-page-break-line-height)
    ;; Fix wrapping when switching buffers
    (advice-add
     #'switch-to-buffer :after
     (lambda (_ &optional no-confirm no-enable) (page-break-lines--update-display-tables)))
    ;; Disable with company popup
    (after 'company
      (add-hook 'company-completion-started-hook 'config-font-lock-maybe-turn-off-page-breaks)
      (add-hook 'company-completion-finished-hook 'config-font-lock-maybe-turn-on-page-breaks)
      (add-hook 'company-completion-cancelled-hook 'config-font-lock-maybe-turn-on-page-breaks))))

(use-package hl-todo
  :ensure t
  :defer t
  :commands hl-todo-mode
  :init (add-hook 'prog-mode-hook #'hl-todo-mode))

(use-package highlight-numbers
  :ensure t
  :defer t
  :commands highlight-numbers-mode
  :init (add-hook 'prog-mode-hook #'highlight-numbers-mode))

(provide 'config-font-lock)
;;; config-font-lock.el ends here
