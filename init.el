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

(use-package private :load-path "./lib/" :if (file-exists-p "./lib/private.el"))
(use-package config-theme :load-path "./config")
(use-package config-path :load-path "./config")
(use-package config-browser :load-path "./config" :disabled t)
(use-package config-buffers :load-path "./config")
(use-package config-completion :load-path "./config")
(use-package config-debug :load-path "./config")
(use-package config-defaults :load-path "./config")
(use-package config-doc :load-path "./config")
(use-package config-editing :load-path "./config")
(use-package config-elpa :load-path "./config")
(use-package config-files :load-path "./config")
(use-package config-font-lock :load-path "./config")
(use-package config-frame :load-path "./config" :if window-system)
(use-package config-git :load-path "./config")
(use-package config-gui :load-path "./config")
(use-package config-help :load-path "./config")
(use-package config-indentation :load-path "./config")
(use-package config-irc :load-path "./config" :disabled t)
(use-package config-ivy :load-path "./config")
(use-package config-marks :load-path "./config")
(use-package config-modeline :load-path "./config")
(use-package config-org :load-path "./config")
(use-package config-outlines :load-path "./config" :disabled t)
(use-package config-parsers :load-path "./config")
(use-package config-persistence :load-path "./config")
(use-package config-prog-mode :load-path "./config")
(use-package config-project :load-path "./config")
(use-package config-prose :load-path "./config")
(use-package config-scratch :load-path "./config")
(use-package config-search :load-path "./config")
(use-package config-sexp :load-path "./config")
(use-package config-shell :load-path "./config")
(use-package config-term :load-path "./config")
(use-package config-windows :load-path "./config")

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
