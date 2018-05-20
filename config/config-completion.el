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
  :if (file-exists-p abbrev-file-name)
  :config
  (progn
    (setq-default abbrev-mode t)
    (validate-setq save-abbrevs 'silently)
    (quietly-read-abbrev-file)))

;; ** Mini-buffer

(setq history-length most-positive-fixnum)


;; * Company

(use-package company
  :ensure t
  :init (after-init #'global-company-mode)
  :config
  (progn
    (bind-key "TAB" #'company-complete-common-or-cycle company-active-map)
    (validate-setq
     company-backends '((company-elisp) (company-capf company-dabbrev company-files) (company-dabbrev-code company-keywords))
     company-echo-delay 0
     company-frontends '(company-pseudo-tooltip-frontend company-echo-metadata-frontend)
     company-idle-delay 0.2
     company-minimum-prefix-length 2
     company-require-match nil
     company-search-regexp-function 'company-search-words-regexp
     company-tooltip-align-annotations t
     company-tooltip-limit 10
     company-transformers '(company-sort-by-occurrence))
    (setq company-global-modes '(not text-mode message-mode git-commit-mode org-mode))))

(use-package company-dabbrev
  :config
  (validate-setq
   company-dabbrev-downcase nil
   company-dabbrev-ignore-case nil
   company-dabbrev-minimum-length 2))

(use-package company-quickhelp
  :ensure t
  :after company
  :init (bind-key "C-h" 'company-quickhelp-mode company-active-map)
  :config (validate-setq company-quickhelp-delay 0.2))

(provide 'config-completion)
;;; config-completion.el ends here
