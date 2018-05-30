;;; config-layouts.el --- Windows layouts management  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Julien Fantin

;; Author: Julien Fantin(require 'use-config) <julienfantin@gmail.com>
;; Keywords: convenience, frames

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

;; Most of this code has been adapted from the spacemacs layout layer.

;;; Code:
(require 'use-config)
(eval-when-compile
  (require 'cl-lib))


;; * Eyebrowse

(use-package eyebrowse
  :ensure t
  :init (after-init #'eyebrowse-mode)
  :custom
  (eyebrowse-switch-back-and-forth t)
  (eyebrowse-mode-line-style nil))

(provide 'config-layouts)
;;; config-layouts.el ends here
