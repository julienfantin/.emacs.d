;;; config-js.el --- JavaScript config               -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Julien Fantin

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
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:
(require 'use-package)
(require 'config-lsp)

(defvar config-js-use-lsp t)


;; * Major modes

(use-package js3-mode
  :straight t
  :mode (("\\.js$" . js3-mode)))

(use-package typescript-mode
  :ensure-system-package (tsc . typescript)
  :straight t
  :mode ("\\.tsx\\'"))


;; * Packages

(use-package indium
  :if (not config-js-use-lsp)
  :straight t
  :after js3-mode
  :hook (js3-mode . indium-interaction-mode)
  :custom
  (indium-debugger-mode 1)
  (indium-nodejs-inspect-brk t)
  (indium-script-enable-sourcemaps t))

(use-package tide
  :if (not config-js-use-lsp)
  :straight t
  :after typescript-mode
  :hook
  ((typescript-mode . tide-setup)
   (typescript-mode . tide-hl-identifier-mode))
  :custom
  (flycheck-check-syntax-automatically '(save mode-enabled)))


;; * LSP

(use-package lsp-typescript
  :if (and config-js-use-lsp (eq config-lsp-frontend 'lsp-mode))
  :ensure-system-package (node)
  :ensure-system-package (javascript-typescript-stdio . "npm i -g javascript-typescript-langserver")
  :straight lsp-javascript-typescript
  :after typescript-mode
  :hook (typescript-mode . lsp-javascript-typescript-enable))

(provide 'config-js)
;;; config-js.el ends here
