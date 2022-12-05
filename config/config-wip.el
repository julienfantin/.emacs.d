;;; config-wip.el --- Temporary init code for quick testing  -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Julien Fantin

;; Author: Julien Fantin <julienfantin@gmail.com>
;; Keywords: convenience

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

;;; Defaults

(add-to-list 'exec-path "~/bin")

;;; Prose
(use-package freeze-it :straight t)

(use-package spell-fu
  :disabled t                           ;; too many false positives
  :ensure-system-package aspell
  :straight t
  :hook (after-init . global-spell-fu-mode))

(use-package literate-calc-mode :straight t)

(provide 'config-wip)
;;; config-wip.el ends here
