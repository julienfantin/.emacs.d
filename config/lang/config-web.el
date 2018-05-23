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
(require 'use-config)


;; * Customs

(defvar config-web-default-indent 2)


;; * Built-ins

(use-package js
  :custom (js-indent-level config-web-default-indent))


;; * Packages

(use-package json-mode
  :ensure t
  :mode "\\.json?\\'")

(use-package web-mode
  :ensure t
  :mode (("\\.html?\\'" . web-mode)
         ("\\.css?\\'"  . web-mode))
  :hook
  ((web-mode . aggressive-indent-mode)
   (web-mode . paredit-mode))
  :custom
  (web-mode-markup-indent-offset config-web-default-indent)
  (web-mode-css-indent-offset config-web-default-indent)
  (web-mode-code-indent-offset config-web-default-indent)
  (web-mode-enable-auto-pairing t)
  (web-mode-enable-css-colorization t)
  (web-mode-enable-current-element-highlight t)
  (web-mode-enable-current-column-highlight t))

(use-package company-web
  :ensure t
  :after (web-mode config-completion)
  :config (config-completion-add-backends 'web-mode 'company-css 'company-web-html))

(provide 'config-web)
;;; config-web.el ends here
