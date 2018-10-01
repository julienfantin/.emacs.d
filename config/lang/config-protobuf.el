;;; config-protobuf.el --- Protobuf-mode config      -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Julien Fantin

;; Author: Julien Fantin <jfantin@jfantin-mbp143>
;; Keywords: tools, data

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


(defconst config-protobuf-style
  '((c-basic-offset . 4)
    (indent-tabs-mode . nil)))

(use-package protobuf-mode
  :ensure t
  :mode "\\.proto\\'"
  :preface
  (defun config-protobuf-add-style ()
    (c-add-style "protobuf" config-protobuf-style t))
  :hook (protobuf-mode . config-protobuf-add-style))


(provide 'config-protobuf)
;;; config-protobuf.el ends here
