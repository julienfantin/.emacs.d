;;; config-editing.el --- Editing                    -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Julien Fantin

;; Author: Julien Fantin <julienfantin@gmail.com>
;; Keywords: editing

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
(require 'config-path)


;; * Builtins

(use-package emacs
  :custom
  (fill-column 80))

(use-package delsel
  :init (delete-selection-mode 1))

(use-package simple
  :custom
  (kill-ring-max most-positive-fixnum)
  (kill-do-not-save-duplicates t))

(use-package newcomment
  :config
  (setq-default comment-auto-fill-only-comments t))


;; * Packages

(use-package multiple-cursors :straight t :after no-littering)
(use-package multiple-cursors
  :straight t
  :after no-littering
  :config
  (add-hook
   'multiple-cursors-mode-hook
   (defun config-editing-multiple-cursors-box ()
     "Bar cursors look weird, so force it to box while enabled."
     (if multiple-cursors-mode
         (setq cursor-type 'box)
       (setq cursor-type 'bar)))))

(use-package phi-search
  :straight t
  :after multiple-cursors
  :config
  (add-hook
   'multiple-cursors-mode-hook
   (defun config-editing-multiple-cursors-phi-search ()
     "Remap isearch to phi-search while multiple-cursors is enabled."
     (if multiple-cursors-mode
         (progn
           (local-set-key [remap isearch-forward] 'phi-search)
           (local-set-key [remap isearch-backward] 'phi-search-backward))
       (progn
         (local-set-key [remap isearch-forward] nil)
         (local-set-key [remap isearch-backward] nil))))))

(use-package iedit
  :straight t
  :bind (:map isearch-mode-map
              ("C-c e i" . iedit-mode-from-isearch)))




;; * Commands

;;;###autoload
(defun -backward-kill-word-or-region ()
  "Kill backward word or region if active."
  (interactive)
  (if (region-active-p)
      (if (bound-and-true-p paredit-mode)
          (call-interactively 'paredit-kill-region)
        (call-interactively 'kill-region))
    (if (bound-and-true-p paredit-mode)
        (call-interactively 'paredit-backward-kill-word)
      (call-interactively 'backward-kill-word))))

;;;###autoload
(defun -cleanup ()
  "Indent, untabify and cleanup whitespace in region or buffer."
  (interactive)
  (save-excursion
    (unless (use-region-p)
      (goto-char (point-min))
      (push-mark)
      (goto-char (point-max)))
    (untabify (region-beginning) (region-end))
    (indent-region (region-beginning) (region-end))
    (save-restriction
      (narrow-to-region (region-beginning) (region-end))
      (whitespace-cleanup)))
  (message (format "%s cleaned!" (buffer-name))))

;;;###autoload
(defun -unfill-paragraph (&optional region)
  "Turn a multi-line paragraph into a single line of text."
  (interactive (progn (barf-if-buffer-read-only) '(t)))
  (let ((fill-column (point-max))
        ;; This would override `fill-column' if it's an integer.
        (emacs-lisp-docstring-fill-column t))
    (fill-paragraph nil region)))

(provide 'config-editing)
;;; config-editing.el ends here
