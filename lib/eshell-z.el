;;; eshell-z.el --- Quickly jump in directories in eshell. -*- lexical-binding: t -*-

;; Copyright © 2014 Travis Jeffery <tj@travisjeffery.com>

;; Author: Travis Jeffery <tj@travisjeffery.com>
;; URL: https://github.com/travisjeffery/eshell-z.el
;; Keywords: eshell, convenience
;; Version: 0.0.1

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; This library provides a way to quickly jump between directories in
;; eshell. See the README for more details.
;;
;;; Code:

(require 'em-dirs)

;;;###autoload
(defvar eshell-z--map nil "Hash map mapping directories to z call count.")

;;;###autoload
(defcustom eshell-z--file
  (expand-file-name "z" eshell-directory-name)
  "The file to save which directories to z."
  :type 'file :group 'eshell-dirs)

(defun eshell-z--load ()
  "Load `eshell-z--map' by serializing `eshell-z--file'."
  (let ((map (make-hash-table :test 'equal)))
    (when (file-exists-p eshell-z--file)
      (dolist (element (with-temp-buffer
                         (insert-file-contents eshell-z--file)
                         (goto-char (point-min))
                         (read (current-buffer))))
        (when (file-directory-p (car element))
          (puthash (car element) (cdr element) map))))
    (setq eshell-z--map map)))

(defun eshell-z--candidates ()
  "A sorted list candidate directories."
  (unless eshell-z--map
    (eshell-z--load))
  (let (keys)
    (maphash (lambda (key value)
               (setq keys (cons key keys)))
             eshell-z--map)
    (sort keys (lambda (a b)
                 (> (gethash a eshell-z--map)
                    (gethash b eshell-z--map))))))

(defun eshell-z--record ()
  "Add or increment `default-directory' in `eshell-z--map'."
  (unless eshell-z--map
    (eshell-z--load))
  (let ((curdir default-directory))
    (if (gethash curdir eshell-z--map)
        (puthash curdir (1+ (gethash curdir eshell-z--map)) eshell-z--map)
      (puthash curdir 1 eshell-z--map))))

(add-hook 'eshell-directory-change-hook 'eshell-z--record)

(defun eshell-z--save ()
  "Save `eshell-z--map' to `eshell-z-file'."
  (when (and eshell-z--file eshell-z--map)
    (with-temp-buffer
      (let ((standard-output (current-buffer)))
        (insert "(")
        (maphash (lambda (key value)
                   (when (> value 0)
                     (insert "(")
                     (prin1 key)
                     (insert " . ")
                     (prin1 (- value 0.01))
                     (insert ")\n")))
                 eshell-z--map)
        (delete-char -1)
        (insert ")"))
      (write-file eshell-z--file))))

(add-hook 'kill-emacs-hook 'eshell-z--save)

;;;###autoload
(defun eshell/z (&rest args)
  "Eshell command to jump to the most-often-jumped directory corresponding to the given `ARGS'."
  (let* ((args (eshell-flatten-list args))
	 (path (car args))
	 (candidates (eshell-z--candidates))
	 result)
    (while (and
            candidates
            (not result)
            (not (file-exists-p path)))
      (if (string-match path (car candidates))
          (setq result (car candidates))
        (setq candidates (cdr candidates))))
    (if (not result)
        (setq result path))
    (eshell/cd result)))

(provide 'eshell-z)
;;; eshell-z.el ends here
