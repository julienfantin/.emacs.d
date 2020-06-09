;;; config-sql.el --- SQL editing                    -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Julien Fantin

;; Author: Julien Fantin <julienfantin@gmail.com>
;; Keywords: languages

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


;; * Built-ins

(use-package sql
  :config
  (progn
    (after 'aggressive-indent
      (add-hook 'sql-mode-hook #'aggressive-indent-mode))
    (after 'paredit
      (add-hook 'sql-mode-hook #'paredit-mode))
    (setq sql-product 'postgres
          sql-send-terminator t)))


;; * Indentation

(use-package sql-indent
  :straight t
  :init
  (progn
    (after 'sql  (require 'sql-indent))
    (after 'edbi (require 'sql-indent)))
  :config
  (setq sql-indent-offset 2
        sql-indent-first-column-regexp
        (concat "\\(^\\s-*"
                (regexp-opt '("with" "window" "inner" "left" "outer" "right"
                              "select" "update" "insert" "delete"
                              "union" "intersect"
                              "from" "where" "into" "group" "having" "order"
                              "set"
                              "create" "drop" "truncate"
                              "begin" "end" "lock" "commit"
                              "alter" "add" "returning"
                              "copy" "set" "--" "\\^L") t)
                "\\(\\b\\|\\s-\\)\\)\\|\\(^```$\\)")))


;; * EDBI

;; Install:
;; sudo cpan RPC::EPC::Service DBI DBD::Pg
;; perl -MCPAN -e'install DBD::Pg'
;; Connect: dbi:Pg:dbname=mydb

(use-package edbi
  :straight t
  :config
  (add-hook 'edbi:sql-mode-hook #'(lambda () (run-hooks 'sql-mode-hook))))

(use-package edbi-minor-mode
  :straight t
  :init
  (after 'sql
    (add-hook 'sql-mode-hook #'edbi-minor-mode)))

(use-package company-edbi
  :straight t
  :commands (company-edbi)
  :init
  (after (sql company config-completion)
    (add-hook 'sql-mode-hook #'company-mode)
    (add-to-list 'config-completion-backends-alist #'company-edbi)))

(provide 'config-sql)
;;; config-sql.el ends here
