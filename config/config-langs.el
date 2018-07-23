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
(require 'use-config)

(use-config config-ansible :disabled t)
(use-config config-cl :disabled t)
(use-config config-clojure)
(use-config config-clojurescript)
(use-config config-emacs-lisp)
(use-config config-js)
(use-config config-nix :disabled t)
(use-config config-ocaml)
(use-config config-sql :disabled t)
(use-config config-web)
(use-config config-yaml)

(provide 'config-langs)
;;; config-langs.el ends here
