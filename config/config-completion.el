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
(require 'use-package)
(require 'map)


;; * Defaults

(setq-default tab-always-indent 'complete)
(setq-default completion-styles '(partial-completion substring basic))


;; * Built-ins
;; ** Abbrev

(use-package abbrev
  :disabled t
  :after no-littering
  :config
  (progn
    (setq-default abbrev-mode t)
    (quietly-read-abbrev-file))
  :custom
  (save-abbrevs 'silently))

;; ** Mini-buffer

(setq history-length most-positive-fixnum)

(use-package minibuffer
  :init
  (progn
    (after-init #'minibuffer-depth-indicate-mode)
    (after-init #'minibuffer-electric-default-mode))
  :custom
  (enable-recursive-minibuffers t)
  (completion-styles '(partial-completion substring initials flex))
  (completion-category-overrides
   '((file (styles initials basic))
     (buffer (styles initials basic))
     (info-menu (styles basic))))
  (read-answer-short t)
  (read-buffer-completion-ignore-case t)
  (read-file-name-completion-ignore-case t)
  (resize-mini-windows t))

;; ** Icomplete


;; * Prescient

(use-package prescient
  :disabled t
  :straight t
  :after no-littering
  :config (prescient-persist-mode 1))


;; * Company

(use-package company
  :straight t
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
   '((company-files                     ; files & directory
      company-capf
      company-yasnippet
      company-elisp
      )
     (company-elisp)
     (company-abbrev company-dabbrev)
     )
   ;; '((company-capf company-files)
   ;;   (company-elisp)
   ;;   (company-keywords))
   )
  (company-idle-delay 0.1)
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
  (company-dabbrev-minimum-length 4))

(use-package company-elisp
  :custom
  (company-elisp-detect-function-context nil))

(use-package compdef :straight t)

;; this frontend properly renders propertized text, variable pitch font and
;; doesn't have to it within the parent-frame
(use-package company-box
  :disabled t
  :straight t
  :after company
  :hook (company-mode . company-box-mode)
  :custom
  ;; Icons are huge!?
  (company-box-enable-icon t)
  ;; Search doesn't scroll to focus the current selection
  (company-search-filtering t))

(use-package company-prescient
  :disabled t
  :straight t
  :after company
  :init (after-init #'company-prescient-mode))

(provide 'config-completion)
;;; config-completion.el ends here
