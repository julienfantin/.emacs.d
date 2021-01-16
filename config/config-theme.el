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

;;; Config

(defcustom config-theme-light 'modus-operandi
  "Default theme."
  :type 'symbol
  :group 'config-theme)

(defcustom config-theme-dark 'modus-vivendi
  "Default theme."
  :type 'symbol
  :group 'config-theme)

(defun config-theme-load-default-theme (&optional appearance)
  "Load the default theme for the given APPEARANCE."
  (pcase (or appearance
             (when (boundp '-ns-system-appearance) -ns-system-appearance))
    ('dark (load-theme config-theme-dark t))
    (_ (load-theme config-theme-light t))))

;;; Built-ins

(use-package custom
  :commands load-theme
  :hook (after-init . config-theme-load-default-theme)
  :config
  (config-path-add-to-load-path custom-theme-directory)
  :custom
  (custom-safe-themes t)
  (custom-theme-directory (expand-file-name "themes/" user-emacs-directory))
  :config
  ;; Patch from emacs-plus
  (when (boundp 'ns-system-appearance-change-functions)
    (add-hook 'ns-system-appearance-change-functions
              #'(lambda (appearance)
                  (mapc #'disable-theme custom-enabled-themes)
                  (pcase appearance
                    ('light (load-theme config-theme-light t))
                    ('dark (load-theme config-theme-dark t)))))))

;;; Third-party

(use-package doom-themes           :straight t)
(use-package dracula-theme         :straight t)
(use-package darktooth-theme       :straight t)
(use-package zerodark-theme        :straight t)
(use-package gruvbox-theme         :straight t)
(use-package rebecca-theme         :straight t)
(use-package nord-theme            :straight t)

(use-package modus-themes
  :straight t)

(use-package kaolin-themes
  :straight t
  :custom
  (kaolin-themes-comments-style 'bright)
  (kaolin-themes-distinct-company-scrollbar t))

(provide 'config-theme)
;;; config-theme.el ends here
