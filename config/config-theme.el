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


;; * Built-ins

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


;; * Themes

(use-package theme-sync
  :disabled t
  :commands theme-sync-mode
  :hook (after-init . theme-sync-mode))

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

(use-package modus-operandi-theme
  :straight t
  :defer t
  :custom
  (modus-operandi-theme-slanted-constructs t)
  (modus-operandi-theme-bold-constructs t)
  (modus-operandi-theme-visible-fringes nil)
  (modus-operandi-theme-3d-modeline t)
  (modus-operandi-theme-subtle-diffs t)
  (modus-operandi-theme-intense-standard-completions t)
  (modus-operandi-theme-distinct-org-blocks t)
  (modus-operandi-theme-proportional-fonts t)
  (modus-operandi-theme-rainbow-headings nil)
  (modus-operandi-theme-section-headings nil)
  (modus-operandi-theme-scale-headings t)
  (modus-operandi-theme-scale-1 1.05)
  (modus-operandi-theme-scale-2 1.1)
  (modus-operandi-theme-scale-3 1.15)
  (modus-operandi-theme-scale-4 1.2)
  (modus-operandi-theme-scale-5 1.3))

(use-package modus-vivendi-theme
  :straight t
  :defer t
  :custom
  (modus-vivendi-theme-slanted-constructs t)
  (modus-vivendi-theme-bold-constructs t)
  (modus-vivendi-theme-visible-fringes nil)
  (modus-vivendi-theme-3d-modeline t)
  (modus-vivendi-theme-subtle-diffs t)
  (modus-vivendi-theme-intense-standard-completions t)
  (modus-vivendi-theme-distinct-org-blocks t)
  (modus-vivendi-theme-proportional-fonts t)
  (modus-vivendi-theme-rainbow-headings nil)
  (modus-vivendi-theme-section-headings nil)
  (modus-vivendi-theme-scale-headings t)
  (modus-vivendi-theme-scale-1 1.05)
  (modus-vivendi-theme-scale-2 1.1)
  (modus-vivendi-theme-scale-3 1.15)
  (modus-vivendi-theme-scale-4 1.2)
  (modus-vivendi-theme-scale-5 1.3))


(use-package kaolin-themes
  :straight t
  :defer t
  :custom
  (kaolin-themes-comments-style 'bright)
  (kaolin-themes-distinct-company-scrollbar t))

(provide 'config-theme)
;;; config-theme.el ends here
