;;; config-debug.el --- Emacs config debugging       -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Julien Fantin

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

;; * Debug on error

(toggle-debug-on-error)
(after-init #'toggle-debug-on-error)


;; * Find bugs in config files

(use-package bug-hunter :straight t)


;; * Profiling

;; (ignore-errors
;;   (profiler-start 'cpu+mem))

(provide 'config-debug)
;;; config-debug.el ends here
