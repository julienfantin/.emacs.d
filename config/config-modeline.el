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

(use-package doom-modeline
  :straight t
  :init (doom-modeline-mode 1)
  :custom
  (doom-modeline-icon (display-graphic-p)) ;; NOTE eval (all-the-icons-install-fonts)
  (doom-modeline-bar-width 36)
  (doom-modeline-height 24))

(use-package hide-mode-line :straight t)

(use-package recursion-indicator
  :straight t
  :demand t
  :hook (after-init . recursion-indicator-mode))

(provide 'config-modeline)
;;; config-modeline.el ends here
