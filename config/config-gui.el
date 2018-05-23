;;; config-gui.el --- Emacs internal GUI (frames & terminal)  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Julien Fantin

;; Author: Julien Fantin(require 'use-config) <julienfantin@gmail.com>
;; Keywords: internal, terminals, frames

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


;; * Built-ins

(validate-setq window-resize-pixelwise t)

(use-package simple
  :config
  (line-number-mode -1))

(use-package fringe
  :disabled t
  :init (fringe-mode 4)
  :config
  (progn
    (validate-setq
     indicate-empty-lines t
     indicate-buffer-boundaries t
     indicate-unused-lines t)
    (setf (cdr (assq 'continuation fringe-indicator-alist))
          '(nil right-curly-arrow))))

(use-package hl-line
  :hook ((magit-mode dired-mode) . hl-line-mode)
  :custom
  (global-hl-line-sticky-flag nil)
  (hl-line-sticky-flag nil))



;; * Packages

(use-package rich-minority
  :ensure t
  :init (rich-minority-mode 1)
  :custom (rm-whitelist '(" λ")))

(use-package focus :disabled t :ensure t)

(provide 'config-gui)
;;; config-gui.el ends here
