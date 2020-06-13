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

(use-package emacs
  :custom
  (tab-always-indent 'complete)
  (history-length most-positive-fixnum))


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

(use-package minibuffer
  :hook ((after-init . minibuffer-depth-indicate-mode))
  :config (defalias 'yes-or-no-p 'y-or-n-p)
  :custom
  (enable-recursive-minibuffers t)
  (completion-category-overrides
   '((file (styles initials basic))
     (buffer (styles initials basic))
     (info-menu (styles basic))))
  (read-answer-short t)
  (read-buffer-completion-ignore-case t)
  (read-file-name-completion-ignore-case t))

;; ** Icomplete


;; * M-x

(use-package amx
  :straight t
  :init (after-init #'amx-mode))


;; * Completion styles

(use-package orderless
  :straight t
  :custom
  (completion-styles '(orderless)))


;; * Company

(use-package company
  :straight t
  :hook (prog-mode . company-mode)
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
   '((company-capf :with company-yasnippet)
     company-files
     company-dabbrev-code
     company-dabbrev
     company-keywords))
  (company-idle-delay 0.1)
  (company-minimum-prefix-length 2)
  (company-require-match nil)
  (company-search-regexp-function 'company-search-words-in-any-order-regexp)
  (company-show-numbers t)
  (company-tooltip-align-annotations t)
  (company-tooltip-limit 10)
  (company-tooltip-minimum 10)
  (company-tooltip-minimum-width 50))

(use-package company-quickhelp
  :straight t
  :hook (company-mode . company-quickhelp-local-mode))

(use-package company-posframe
  :straight t
  ;; https://debbugs.gnu.org/cgi/bugreport.cgi?bug=41422
  :disabled t
  :hook (company-mode . company-posframe-mode))

(use-package company-dabbrev
  :after company
  :custom
  (company-dabbrev-downcase nil)
  (company-dabbrev-ignore-case nil)
  (company-dabbrev-minimum-length 4))

(use-package compdef
  :straight t
  :after company)

(use-package company-prescient
  :straight t
  :after (company no-littering)
  :hook (company-mode . company-prescient-mode)
  :config
  (prescient-persist-mode 1))

(provide 'config-completion)
;;; config-completion.el ends here
