;;; config-search.el --- Search in files and buffers  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Julien Fantin

;; Author: Julien Fantin <julienfantin@gmail.com>
;; Keywords: convenience, files, matching, tools

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

(use-package isearch
  :custom
  (isearch-allow-scroll t)
  (lazy-highlight-initial-delay 0)
  (isearch-invisible 'open))

(use-package imenu
  :bind ("C-c i" . imenu)
  :custom
  (imenu-auto-rescan t)
  (imenu-max-item-length 1000))

;;; Third-party

(use-package anzu
  :straight t
  :hook (after-init . global-anzu-mode)
  :bind
  (([remap query-replace] . anzu-query-replace)
   ([remap query-replace-regexp] . anzu-query-replace-regexp)
   ([remap isearch-query-replace] . anzu-isearch-query-replace)
   ([remap isearch-query-replace-regexp] . anzu-isearch-query-replace-regexp)))

(use-package imenu-anywhere
  :straight t
  :bind ("C-c I" . imenu-anywhere))

(use-package avy
  :straight t
  :custom
  (avy-style 'at)
  (avy-all-windows nil)
  (avy-timeout-seconds 0.3)
  (avy-background t)
  :custom-face
  (avy-background-face ((t (:foreground unspecified :background unspecified :inherit shadow))))
  :config
  ;; Use C-' in isearch to bring up avy
  (avy-setup-default)
  (with-eval-after-load "embark"
    (defun avy-action-replace-sexp (pt)
      (unwind-protect
          (progn
            (goto-char pt)
            (let ((bounds (bounds-of-thing-at-point 'sexp)))
              (puni-delete-region (car bounds) (cdr bounds)))))
      t)
    (defun avy-action-embark (pt)
      (unwind-protect
          (save-excursion
            (goto-char pt)
            (embark-act))
        (select-window
         (cdr (ring-ref avy-ring 0))))
      t)
    (add-to-list 'avy-dispatch-alist  '(?. . avy-action-embark))
    (add-to-list 'avy-dispatch-alist  '(?r . avy-action-replace-sexp))))

(provide 'config-search)
;;; config-search.el ends here
