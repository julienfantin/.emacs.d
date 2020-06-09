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
  :custom
  (sql-product 'postgres)
  (sql-send-terminator t))

(use-package paredit
  :after sql
  :hook ((sql-mode . paredit-mode)))

(use-package aggressive-indent
  :after sql
  :hook ((sql-mode . aggressive-indent-mode)))


;; * Indentation

(use-package sql-indent
  :straight t
  :after sql
  :custom
  (sql-indent-offset 2)
  (sql-indent-first-column-regexp
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

(provide 'config-sql)
;;; config-sql.el ends here
