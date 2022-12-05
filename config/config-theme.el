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

;; Load/enable theme hook

(defvar load-theme-hook nil
  "Normal hook run after loading a theme.")

(defun run-load-theme-hook (&rest _args)
  "Run `load-theme-hook'."
  (run-hooks 'load-theme-hook))

(advice-add 'enable-theme :after #'run-load-theme-hook)
(advice-add 'load-theme :after #'run-load-theme-hook)

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
  (pcase (or appearance (when (boundp '-ns-system-appearance) -ns-system-appearance))
    ('dark (load-theme config-theme-dark t))
    (_ (load-theme config-theme-light t))))

(defun config-theme-inherit-default ()
  "Do not use bold and italic for builtins and documentation."
  (set-face-attribute 'font-lock-comment-face nil :inherit 'default)
  (set-face-attribute 'font-lock-comment-delimiter-face nil :inherit 'default)
  (set-face-attribute 'font-lock-doc-face nil :inherit 'default)
  (set-face-attribute 'font-lock-builtin-face nil :inherit 'default)
  (set-face-attribute 'font-lock-keyword-face nil :inherit 'default))

;;; Built-ins

(use-package custom
  :commands load-theme
  :hook
  ((after-init . config-theme-load-default-theme)
   ;; (load-theme . config-theme-inherit-default)
   )
  :custom
  (custom-safe-themes t)
  :config
  ;; requires a patch from emacs-plus
  (add-hook 'ns-system-appearance-change-functions 'config-theme-load-default-theme))

;;; Third-party

(use-package fontify-face :straight t)

(use-package ef-themes
  :straight t
  :custom
  (ef-themes-mixed-fonts t))

(use-package modus-themes
  :straight t
  :init
  (setq
   modus-themes-bold-constructs t
   modus-themes-box-buttons '(flat accented underline)
   modus-themes-deuteranopia t
   modus-themes-diffs 'desaturated
   modus-themes-italic-constructs t
   modus-themes-lang-checkers '(straight-underline text-also)
   modus-themes-links '(neutral-underline background bold)
   modus-themes-mode-line '(accented borderless (padding . 1))
   modus-themes-prompts '(intense bold)
   modus-themes-region '(bg-only no-extend)
   modus-themes-subtle-line-numbers t
   modus-themes-mixed-fonts t
   modus-themes-variable-pitch-ui nil))

(use-package doom-themes :straight t)

(provide 'config-theme)
;;; config-theme.el ends here
