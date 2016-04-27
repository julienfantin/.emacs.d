;;; config-path.el --- Emacs path configuration      -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Julien Fantin

;; Author: Julien Fantin <julienfantin@gmail.com>
;; Keywords: internal, files, terminals

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


;; * Files and directories
;; ** .emacs.d

;;;###autoload
(defun user-file (name)
  "Return a file with `NAME' in `user-emacs-directory'."
  (expand-file-name name user-emacs-directory))

(setq custom-file (user-file "custom.el"))

;; ** var

(defcustom user-var-directory
  (expand-file-name "var/" user-emacs-directory)
  "Directory containing libraries cache or temporary or cache files."
  :type 'directory
  :group 'config-path)

;;;###autoload
(defun user-var-file (name)
  "Return a filename for 'NAME' in 'user-var-directory'."
  (let ((file (expand-file-name name user-var-directory)))
    (unless (file-exists-p file)
      (with-temp-buffer (write-file file)))
    file))

;;;###autoload
(defun user-var-directory (name)
  "Return a directory name for 'NAME' in 'user-var-directory'."
  (let ((dir (expand-file-name name user-var-directory)))
    (unless (file-exists-p dir)
      (make-directory dir 'parents))
    dir))

;; ** lib

(defcustom user-lib-directory
  (expand-file-name "lib/" user-emacs-directory)
  "User libraries directory."
  :type 'directory
  :group 'config-path)

;; ** config

(defcustom user-config-directory
  (expand-file-name "config/" user-emacs-directory)
  "User libraries directory."
  :type 'directory
  :group 'config-path)


;; * Load path

(defcustom config-path-system-type-site-lisp
  (alist-get
   system-type
   '(darwin . ("/usr/local/share/emacs/site-lisp/")))
  "List of addiditional system directories."
  :type 'directory
  :group 'config-path)

(defun config-path-add-to-load-path (dir)
  "Add 'DIR' and its descendants to 'load-path'."
  (let ((default-directory dir))
    (normal-top-level-add-to-load-path (list dir))
    (normal-top-level-add-subdirs-to-load-path)
    (delete-dups load-path)))

;;;###autoload
(defun config-path-update-load-path ()
  "Add config paths to the 'load-path'."
  (config-path-add-to-load-path user-lib-directory)
  (config-path-add-to-load-path user-config-directory)
  (dolist (path config-path-system-type-site-lisp)
    (config-path-add-to-load-path path)))


;; * Entrypoint

;;;###autoload
(config-path-update-load-path)

(provide 'config-path)
;;; config-path.el ends here
