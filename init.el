;;; init.el --- Config init                          -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Julien Fantin

;; Author: Julien Fantin <julienfantin@gmail.com>
;; Keywords:

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

;;; Bootstrap

(defmacro comment (&rest _body) "Ignore 'BODY'." nil)

;;; Config options

(defvar config-completion-system 'vertico)

;;; Configs

;; (use-package private :demand t :load-path "./lib/" :if (file-exists-p "./lib/private.el"))

;;;; Keybindings





(require 'config-keybindings)

(require 'config-theme)

(require 'config-path)
(require 'config-buffers)
(require 'config-completion)
(require 'config-emacs)
(require 'config-debug)
(require 'config-defaults)
(require 'config-doc)
(require 'config-editing)
(require 'config-files)
(require 'config-font-lock)
(when window-system
  (require 'config-frame))
(require 'config-git)
(require 'config-gui)
(require 'config-help)
(require 'config-indentation)
(require 'config-modeline)
(require 'config-org)
(require 'config-outlines)
(require 'config-parsers)
(require 'config-persistence)
(require 'config-prog-mode)
(require 'config-project)
(require 'config-prose)
(require 'config-scratch)
(require 'config-search)
(require 'config-sexp)
(require 'config-shell)
(require 'config-windows)

;;;; Langs

(require 'config-lsp)
(require 'config-langs)

;;;; WIP

(require 'config-wip)

(provide 'init)
;;; init.el ends here
