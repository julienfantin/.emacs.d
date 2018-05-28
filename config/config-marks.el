;;; config-marks.el --- Marks and bookmarks          -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Julien Fantin

;; Author: Julien Fantin <julienfantin@gmail.com>
;; Keywords: convenience, files

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
(require 'use-config)
(require 'config-path)


;; * Mark ring

(defconst config-marks-max-pop-duplicate-count 10)

(defun config-marks-pop-duplicate-marks-advice (pop-to-mark-command &rest args)
  "Around advice for 'POP-TO-MARK-COMMAND'.
Pops the mark up to 'config-marks-max-pop-duplicate-count' when
the point doesn't move before calling 'POP-TO-MARK-COMMAND' with 'ARGS'."
  (let ((p (point)))
    (dotimes (_i config-marks-max-pop-duplicate-count)
      (when (= p (point))
        (apply pop-to-mark-command args)))))

(use-package simple
  :config
  (progn
    (setq mark-ring-max 128 global-mark-ring-max 128)
    (advice-add #'pop-to-mark-command :around #'config-marks-pop-duplicate-marks-advice)))


;; * Commands

;;;###autoload
(defun -push-mark-no-activate ()
  "Push `point' to `mark-ring' without activating region.
Equivalent to \\[set-mark-command] when \\[transient-mark-mode] is disabled" (interactive)
  (push-mark (point) t nil)
  (message "Pushed mark to ring"))

;;;###autoload

(defun -jump-to-mark ()
  "Jump to the local mark, respecting the `mark-ring' order.
This is the same as using \\[set-mark-command] with the prefix
argument."
  (interactive)
  (set-mark-command 1))

;;;###autoload
(defun -exchange-point-and-mark-no-activate ()
  "Identical to \\[exchange-point-and-mark] but will not activate the region."
  (interactive)
  (exchange-point-and-mark)
  (deactivate-mark nil))

(provide 'config-marks)
;;; config-marks.el ends here
