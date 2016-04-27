;;; config-completion.el --- Read and auto completion  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Julien Fantin

;; Author: Julien Fantin <julienfantin@gmail.com>
;; Keywords: convenience, languages

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
(require 'map)


;; * Customs

(defvar config-completion-backends-enable-yasnippet t
  "Enable yasnippet for all backends.")

(defvar config-completion-backends-alist
  '((emacs-lisp-mode
     . (company-elisp
        company-files
        company-dabbrev-code company-keywords))
    (eshell-mode
     . (company-files))
    (cider-mode
     . (company-capf
        company-files
        company-dabbrev-code))))

(defvar config-completion--default-backends
  '(company-files (company-dabbrev-code company-keywords)))


;; * Defaults

(setq-default tab-always-indent 'complete)
(setq-default completion-styles '(partial-completion substring basic))


;; * Built-ins
;; ** Abbrev

(use-package abbrev
  :defer t
  :if (file-exists-p abbrev-file-name)
  :config
  (progn
    (setq-default abbrev-mode t)
    (setq save-abbrevs 'silently)
    (quietly-read-abbrev-file)))


;; * Company

(use-package smart-tab :ensure t :defer t)

(defun config-completion-add-backends (mode &rest backends)
  "Add 'MODE' specific 'BACKENDS' to 'config-completion-backends-alist'."
  (let* ((map config-completion-backends-alist)
         (existing (map-elt config-completion-backends-alist mode)))
    (setq config-completion-backends-alist
          (map-put map mode (append existing backends)))))

(use-package company
  :ensure t
  :defer t
  :commands
  (company-mode company-complete-common-or-cycle)
  :preface
  (defun config-completion--company-backends ()
    (append
     (alist-get major-mode config-completion-backends-alist)
     (cl-reduce
      (lambda (acc mode)
        (if-let ((backend (alist-get mode config-completion-backends-alist)))
            (cons backend acc)
          acc))
      (cl-remove-if-not
       (lambda (mode)
         (and (boundp mode) (symbol-value mode)))
       minor-mode-list)
      :initial-value '())))

  (defun config-completion-backend-with-yasnippet (backend)
    (if (or (not config-completion-backends-enable-yasnippet)
            (and (listp backend) (member 'company-yasnippet backend)))
        backend
      (append
       (if (consp backend) backend (list backend))
       '(:with company-yasnippet))))

  (defun config-completion-company-turn-on ()
    (let ((backends (or (config-completion--company-backends) config-completion--default-backends)))
      ;; Set company backends, conditionally enabling yasnippet
      (setq-local company-backends
                  (if config-completion-backends-enable-yasnippet
                      (mapcar #'config-completion-backend-with-yasnippet backends)
                    backends))
      ;; Make smart-tab use company-mode
      (setq-local smart-tab-completion-functions-alist
                  `((,major-mode . company-complete-common)))
      ;; Smart-tab is our completion entry point
      (smart-tab-mode 1)))
  :init (after-init 'global-company-mode)
  :config
  (progn
    (bind-key "TAB" #'company-complete-common-or-cycle company-active-map)
    (add-hook 'company-mode-hook 'config-completion-company-turn-on)
    (setq
     company-idle-delay 0.2
     company-minimum-prefix-length 3
     company-require-match 'never)))

(use-package company-elisp
  :defer t
  :config (setq company-elisp-detect-function-context nil))

(use-package company-quickhelp
  :ensure t
  :defer t
  :init
  (after 'company
    (bind-key "C-h" 'company-quickhelp-mode company-active-map)))

(use-package company-statistics
  :ensure t
  :defer t
  :init
  (after 'company
    (add-hook 'company-mode-hook #'company-statistics-mode))
  :config
  (setq company-statistics-file (user-var-file "company-statistics-cache.el")
        company-statistics-size 200))

(use-package yasnippet
  :if config-completion-backends-enable-yasnippet
  :ensure t
  :defer t
  :init (after-init #'yas-global-mode)
  :config
  (progn
    (unbind-key "<tab>" yas-minor-mode-map)
    (unbind-key "TAB" yas-minor-mode-map)
    (unbind-key "C-c <tab>" yas-minor-mode-map)
    (unbind-key "C-c TAB" yas-minor-mode-map)
    (setq yas-fallback-behavior 'return-nil
          yas-triggers-in-field t
          yas-verbosity 0
          yas-snippet-dirs (list (expand-file-name "snippets/" user-emacs-directory)))))

(provide 'config-completion)
;;; config-completion.el ends here
