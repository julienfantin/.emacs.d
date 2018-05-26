;;; config-prog-mode.el --- Generic programming mode  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Julien Fantin

;; Author: Julien Fantin(require 'use-config) <julienfantin@gmail.com>
;; Keywords: languages

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


(use-package prog-mode
  :hook
  (prog-mode . auto-fill-mode)
  (prog-mode . display-line-numbers-mode))

;; Spell checking code comments
(use-package flyspell
  :disabled t
  :hook (prog-mode . flyspell-prog-mode))

;; Automatic file headers
(use-package autoinsert
  :init (after-init #'auto-insert-mode))

(provide 'config-prog-mode)
;;; config-prog-mode.el ends here
