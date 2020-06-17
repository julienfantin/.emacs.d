;;; config-doc.el --- Documentation integration      -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Julien Fantin

;; Author: Julien Fantin <julienfantin@gmail.com>
;; Keywords: convenience, help

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

(use-package eldoc
  :straight t
  :hook (prog-mode . eldoc-mode)
  :custom
  (eldoc-echo-area-use-multiline-p t)
  (eldoc-idle-delay .5))

(use-package paredit
  :after (paredit eldoc)
  :config
  (eldoc-add-command
   'paredit-backward
   'paredit-forward
   'paredit-backward-delete
   'paredit-close-round))

(use-package know-your-http-well :straight t)

(provide 'config-doc)
;;; config-doc.el ends here
