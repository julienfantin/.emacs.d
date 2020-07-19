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


;; * Bootstrap

(defmacro comment (&rest body) "Ignore 'BODY'." nil)


;; * Config options

(defvar config-completion-system 'ivy)


;; * Configs

(use-package private :demand t :load-path "./lib/" :if (file-exists-p "./lib/private.el"))
(use-package config-theme :demand t :load-path "./config")
(use-package config-path :demand t :load-path "./config")
(use-package config-browser :demand t :load-path "./config" :disabled t)
(use-package config-buffers :demand t :load-path "./config")
(use-package config-completion :demand t :load-path "./config")
(use-package config-debug :demand t :load-path "./config")
(use-package config-defaults :demand t :load-path "./config")
(use-package config-doc :demand t :load-path "./config")
(use-package config-editing :demand t :load-path "./config")
(use-package config-elpa :demand t :load-path "./config")
(use-package config-files :demand t :load-path "./config")
(use-package config-font-lock :demand t :load-path "./config")
(use-package config-frame :demand t :load-path "./config" :if window-system)
(use-package config-git :demand t :load-path "./config")
(use-package config-gui :demand t :load-path "./config")
(use-package config-help :demand t :load-path "./config")
(use-package config-indentation :demand t :load-path "./config")
(use-package config-irc :demand t :load-path "./config" :disabled t)
(use-package config-ivy :demand t :load-path "./config")
(use-package config-marks :demand t :load-path "./config")
(use-package config-modeline :demand t :load-path "./config")
(use-package config-org :demand t :load-path "./config")
(use-package config-outlines :demand t :load-path "./config" :disabled t)
(use-package config-parsers :demand t :load-path "./config")
(use-package config-persistence :demand t :load-path "./config")
(use-package config-prog-mode :demand t :load-path "./config")
(use-package config-project :demand t :load-path "./config")
(use-package config-prose :demand t :load-path "./config")
(use-package config-scratch :demand t :load-path "./config")
(use-package config-search :demand t :load-path "./config")
(use-package config-sexp :demand t :load-path "./config")
(use-package config-shell :demand t :load-path "./config")
(use-package config-term :demand t :load-path "./config")
(use-package config-windows :demand t :load-path "./config")

;; ** Langs

(use-package config-lsp :demand t :load-path "./config")
(use-package config-langs :demand t :load-path "./config")

;; ** Tools

(use-package config-docker :demand t :load-path "./config" :disabled t)

;; ** Keybindings

(use-package config-keybindings :demand t :load-path "./config")

;; ** WIP

(use-package config-wip :demand t :load-path "./config")

(provide 'init)
;;; init.el ends here
