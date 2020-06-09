;;; early-init.el ---                                -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Julien Fantin

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
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:


;; * Performance
;; ** Init GC

(defvar early-init--gc-cons-threshold 16777216) ; 16mb
(defvar early-init--gc-cons-percentage 0.1)

(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

(add-hook
 'after-init-hook
 #'(lambda ()
     (setq gc-cons-threshold early-init--gc-cons-threshold
           gc-cons-percentage early-init--gc-cons-percentage)))

;; ** Defer minibuffer GC

(defun early-init--defer-garbage-collection-h ()
  (setq gc-cons-threshold most-positive-fixnum))

(defun early-init--restore-garbage-collection-h ()
  ;; Defer it so that commands launched immediately after will enjoy the
  ;; benefits.
  (run-at-time
   1 nil (lambda () (setq gc-cons-threshold early-init--gc-cons-threshold))))

(add-hook 'minibuffer-setup-hook #'early-init--defer-garbage-collection-h)
(add-hook 'minibuffer-exit-hook #'early-init--restore-garbage-collection-h)

;; ** Init file handlers

(defvar early-init--file-name-handler-alist file-name-handler-alist)

(setq file-name-handler-alist nil)

(add-hook
 'after-init-hook
 #'(lambda ()
     (setq file-name-handler-alist early-init--file-name-handler-alist)))


;; * Config

;; Straight setup recommendation
(setq package-enable-at-startup nil)

(provide 'early-init)
;;; early-init.el ends here
