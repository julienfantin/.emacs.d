;;; config-theme.el --- Theming                      -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Julien Fantin

;; Author: Julien Fantin <julienfantin@gmail.com>
;; Keywords: faces

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
(require 'config-path)


;; * Customs

(defcustom config-theme-default 'duotone
  "Default theme."
  :type 'symbol
  :group 'config-theme)

(defun config-theme-load-default-theme ()
  "Load the default theme."
  (when config-theme-default
    (load-theme config-theme-default t)))



;; * Built-inss

(use-package custom
  :commands load-theme
  :init
  (progn
    (setq custom-theme-directory
          (expand-file-name "themes/" user-emacs-directory))
    (config-path-add-to-load-path custom-theme-directory)
    (after-init #'config-theme-load-default-theme)))


;; * Themes

(use-package theme-sync
  :disabled t
  :commands theme-sync-mode
  :init (after-init #'theme-sync-mode))

(use-package duotone-theme :load-path "../themes/duotone")
(use-package duotone-reload :disabled t :demand t)

(use-package doom-themes           :straight t :defer t)
(use-package dracula-theme         :straight t :defer t)
(use-package darktooth-theme       :straight t :defer t)
(use-package zerodark-theme        :straight t :defer t)
(use-package gruvbox-theme         :straight t :defer t)
(use-package challenger-deep-theme :straight t :defer t)
(use-package rebecca-theme         :straight t :defer t)
(use-package nord-theme            :straight t :defer t)
(use-package modus-vivendi-theme   :straight t :defer t)
(use-package modus-operandi-theme  :straight t :defer t)
(use-package kaolin-themes
  :straight t
  :defer t
  :custom
  (kaolin-themes-comments-style 'bright)
  (kaolin-themes-distinct-company-scrollbar t))

(provide 'config-theme)
;;; config-theme.el ends here
