;;; config-web.el --- Web programming  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Julien Fantin

;; Author: Julien Fantin <julienfantin@gmail.com>
;; Keywords:

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

;;; Customs

(defvar config-web-use-lsp t)

;;; Built-ins

(use-package mhtml-mode
  :if config-web-use-lsp
  :ensure-system-package (node)
  :ensure-system-package
  (vscode-html-languageserver-bin . "npm install -g vscode-html-languageserver-bin")
  :after lsp-mode
  :mode "\\.html\\'"
  :hook (mhtml-mode . lsp))

(use-package css-mode
  :if config-web-use-lsp
  :ensure-system-package (node)
  :ensure-system-package
  (vscode-css-languageserver-bin . "npm install -g vscode-css-languageserver-bin")
  :after lsp-mode
  :hook (css-mode . lsp))

(use-package json-mode
  :straight t
  :mode "\\.json?\\'")

(provide 'config-web)
;;; config-web.el ends here
