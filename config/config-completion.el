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


;; * Defaults

(setq-default tab-always-indent 'complete)
(setq-default completion-styles '(partial-completion substring basic))


;; * Built-ins
;; ** Abbrev

(use-package abbrev
  :after no-littering
  :config
  (progn
    (setq-default abbrev-mode t)
    (quietly-read-abbrev-file))
  :custom
  (save-abbrevs 'silently))

;; ** Mini-buffer

(setq history-length most-positive-fixnum)
(setq enable-recursive-minibuffers t)


;; * Prescient

(use-package prescient
  :ensure t
  :after no-littering
  :config (prescient-persist-mode 1))


;; * Company

(use-package company
  :ensure t
  :init (after-init #'global-company-mode)
  :bind ((:map company-mode-map
               ("TAB" . company-indent-or-complete-common))
         (:map company-active-map
               ("TAB" . company-complete-common-or-cycle)
               ("C-n" . company-select-next)
               ("C-p" . company-select-previous)
               ("M-/" . company-other-backend))
         (:map company-search-map
               ("C-n" . company-select-next)
               ("C-p" . company-select-previous)))
  :custom
  (company-global-modes
   '(not text-mode message-mode git-commit-mode org-mode magit-status-mode))
  (company-backends
   '((company-capf company-dabbrev company-files)
     (company-elisp)
     (company-dabbrev-code company-keywords)))
  (company-idle-delay 0.2)
  (company-minimum-prefix-length 2)
  (company-require-match nil)
  (company-search-regexp-function 'company-search-words-regexp)
  (company-show-numbers t)
  (company-tooltip-align-annotations t)
  (company-tooltip-limit 10)
  (company-tooltip-minimum 10)
  (company-tooltip-minimum-width 50))

(use-package company-dabbrev
  :custom
  (company-dabbrev-downcase nil)
  (company-dabbrev-ignore-case nil)
  (company-dabbrev-minimum-length 2))

(use-package company-elisp
  :custom
  (company-elisp-detect-function-context nil))

;; this frontend properly renders propertized text, variable pitch font and
;; doesn't have to it within the parent-frame
(use-package company-box
  :ensure t
  :after company
  :hook (company-mode . company-box-mode)
  :custom
  ;; Icons are huge!?
  (company-box-enable-icon nil)
  ;; Search is kinda broken this helps mitigate the issue
  (company-search-filtering nil))

(use-package company-prescient
  :ensure t
  :after company
  :init (require 'company-prescient nil t)
  :config (company-prescient-mode 1))

(provide 'config-completion)
;;; config-completion.el ends here
