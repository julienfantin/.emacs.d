;;; config-langs.el --- Programming language modes config  -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Julien Fantin

;; Author: Julien Fantin <julienfantin@gmail.com>
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
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(require 'use-package)

(use-package config-cl :disabled t)
(use-package config-go :demand t)
(use-package config-clojure :demand t)
(use-package config-emacs-lisp :demand t)
(use-package config-js :disabled t)
(use-package config-ocaml :disabled t)
(use-package config-python :disabled t)
(use-package config-sql :disabled t)
(use-package config-web :disabled t)
(use-package config-yaml :demand t)

(provide 'config-langs)
;;; config-langs.el ends here
