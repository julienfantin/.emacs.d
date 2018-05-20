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

(defvar config-completion-backends-alist
  '((emacs-lisp-mode
     . (company-elisp
        company-dabbrev-code
        company-dabbrev
        company-files
        company-keywords))))

(defvar config-completion--default-backends
  '((company-dabbrev-code company-keywords company-capf)
    company-files))


;; * Defaults

(setq-default tab-always-indent 'complete)
(setq-default completion-styles '(partial-completion substring basic))


;; * Built-ins
;; ** Abbrev

(use-package abbrev
  :disabled t
  :defer t
  :if (file-exists-p abbrev-file-name)
  :config
  (progn
    (setq-default abbrev-mode t)
    (validate-setq save-abbrevs 'silently)
    (quietly-read-abbrev-file)))

;; ** Mini-buffer

(setq history-length most-positive-fixnum)


;; * Company

(use-package smart-tab :ensure t :defer t)

(defun config-completion-add-backends (mode &rest backends)
  "Add 'MODE' specific 'BACKENDS' to 'config-completion-backends-alist'."
  (let* ((existing (map-elt config-completion-backends-alist mode)))
    (map-put config-completion-backends-alist mode (append existing backends))))

(use-package company
  :ensure t
  :demand t
  :preface
  (defun config-completion-company-config ()
    (let ((backends (append (config-completion--company-backends) config-completion--default-backends)))
      (setq company-backends backends)
      (setq-local smart-tab-completion-functions-alist
                  `((,major-mode . company-complete-common)))
      (smart-tab-mode 1)
      (company-mode 1)))
  :init (add-hook 'prog-mode-hook 'config-completion-company-config)
  :config
  (progn
    (bind-key "TAB" #'company-complete-common-or-cycle company-active-map)
    (validate-setq
     company-search-regexp-function 'company-search-words-regexp
     company-idle-delay 0.2
     company-minimum-prefix-length 2
     company-tooltip-align-annotations t
     company-require-match nil)))

(use-package company-elisp
  :defer t
  :config
  (validate-setq company-elisp-detect-function-context nil))

(use-package company-quickhelp
  :ensure t
  :defer t
  :after company
  :init (bind-key "C-h" 'company-quickhelp-mode company-active-map)
  :config (validate-setq company-quickhelp-delay 0.2))

(use-package company-statistics
  :ensure t
  :after (no-littering company)
  :hook (company-mode . company-statistics-mode)
  :defer t
  :config
  (validate-setq company-statistics-size 2000))

(provide 'config-completion)
;;; config-completion.el ends here
