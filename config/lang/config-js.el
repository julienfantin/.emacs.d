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
(require 'use-config)

(use-package js2-mode
  :straight t
  :mode (("\\.js$" . js2-mode)))

(use-package indium
  :disabled t
  :straight t
  :after js2-mode
  :hook (js2-mode . indium-interaction-mode)
  :custom
  (indium-debugger-mode 1)
  (indium-nodejs-inspect-brk t)
  (indium-script-enable-sourcemaps t))

(use-package typescript-mode
  :straight t
  :mode ("\\.tsx\\'")
  :ensure-system-package (tsc . typescript))

(use-package tide
  :straight t
  :disabled t
  :after typescript-mode
  :hook
  ((typescript-mode . tide-setup)
   (typescript-mode . tide-hl-identifier-mode))
  :custom
  (flycheck-check-syntax-automatically '(save mode-enabled)))

;; (use-package lsp-javascript
;;   :straight lsp-javascript-typescript
;;   :ensure-system-package (javascript-typescript-stdio . "npm i -g javascript-typescript-langserver"))

(use-package lsp-typescript
  :after typescript-mode
  :hook (typescript-mode . lsp-javascript-typescript-enable)
  :straight lsp-javascript-typescript
  :ensure-system-package (javascript-typescript-stdio . "npm i -g javascript-typescript-langserver"))


(provide 'config-js)
;;; config-js.el ends here
