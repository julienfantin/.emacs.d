;;; config-buffers.el --- Buffers management         -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Julien Fantin

;; Author: Julien Fantin <julienfantin@gmail.com>
;; Keywords: buffers

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
(eval-when-compile
  (require 'cl-lib))


;; * Commands

(defun --temp-buffers ()
  "Return a list of temp buffers."
  (cl-remove-if-not
   (lambda (buffer)
     (string-match-p  "\\*temp" (buffer-name buffer)))
   (buffer-list)))

;;;###autoload
(defun -temp-buffer (arg)
  "Create or switch to *temp* buffer.
When called with 'ARG' always create a new temp buffer."
  (interactive "P")
  (let* ((n (if (equal '(4) arg) (length (--temp-buffers)) 0))
         (name (format "*temp%s" (if (>= 0 n) "*" (format "-%s*" n))))
         (buffer (get-buffer-create name))
         (mode major-mode))
    (with-current-buffer buffer
      (funcall mode)
      (switch-to-buffer buffer))))

;;;###autoload
(defun -switch-to-last-buffer ()
  "Switch to the most recently used buffer."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) t)))

(provide 'config-buffers)
;;; config-buffers.el ends here
