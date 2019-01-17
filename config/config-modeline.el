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
(require 'use-config)

;; * Packages

(use-package doom-modeline
  :custom
  (doom-modeline-icon nil)
  (doom-modeline-bar-width 32)
  (doom-modeline-height 32)
  (doom-modeline-buffer-file-name-style 'relative-to-project))

(provide 'config-modeline)
;;; config-modeline.el ends here
