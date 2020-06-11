;;; config-defaults.el --- Emacs defaults            -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Julien Fantin

;; Author: Julien Fantin <julienfantin@gmail.com>
;; Keywords: internal

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

;; * Misc
;; ** Initial state

(setq initial-scratch-message ""
      inhibit-startup-message t)

(setq ring-bell-function 'ignore)

;; ** Files encoding

(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(setq default-process-coding-system '(utf-8 . utf-8))
(setq-default buffer-file-coding-system 'utf-8)

;; ** Windows handling

(setq-default cursor-in-non-selected-windows nil)

(provide 'config-defaults)
;;; config-defaults.el ends here
