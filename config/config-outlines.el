;;; config-outlines.el --- Outlines in source files  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Julien Fantin

;; Author: Julien Fantin <julienfantin@gmail.com>
;; Keywords: outlines, languages, convenience

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


;;; Built-ins

(use-package outline
  :hook ((prog-mode . outline-minor-mode)
         (text-mode . outline-minor-mode)))

;;; Third-party

(use-package outline-minor-faces
  :straight t
  :after outline
  :hook (outline-minor-mode . outline-minor-faces-add-font-lock-keywords))

(use-package backline
  :straight t
  :after outline
  :init (advice-add 'outline-flag-region :after 'backline-update))

(use-package bicycle
  :straight t
  :after outline
  :bind (:map outline-minor-mode-map
              ("C-c TAB" . bicycle-cycle)))

(provide 'config-outlines)
;;; config-outlines.el ends here
