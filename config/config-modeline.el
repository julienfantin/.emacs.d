;;; config-modeline.el --- Modeline config           -*- lexical-binding: t; -*-

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

;;; Third-party

(use-package all-the-icons
  :straight t
  :init
  (setq all-the-icons-color-icons nil) )

(use-package doom-modeline
  :straight t
  :init (doom-modeline-mode 1)
  :config
  (setq inhibit-compacting-font-caches t)
  :custom
  (doom-modeline-hud t)
  (doom-modeline-icon (display-graphic-p)) ;; NOTE eval (all-the-icons-install-fonts)
  (doom-modeline-bar-width 4)
  (doom-modeline-height 25))

(use-package recursion-indicator
  :straight t
  :demand t
  :hook (after-init . recursion-indicator-mode))

(provide 'config-modeline)
;;; config-modeline.el ends here
