;;; config-prog-mode.el --- Generic programming mode  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Julien Fantin

;; Author: Julien Fantin <julienfantin@gmail.com>
;; Keywords: languages

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

;;; Third-party

(use-package topsy
  :disabled t ;; messes up scrolling
  :straight (topsy :fetcher github :repo "alphapapa/topsy.el")
  :hook (prog-mode . topsy-mode))

(use-package smart-jump
  :disabled t
  :straight t
  :hook (after-init . smart-jump-setup-default-registers))

(use-package iedit
  :straight t
  :bind
  (("M-i" . -iedit-ensure-function)
   ("M-n" . -iedit-next-occurrence)
   ("M-p" . -iedit-prev-occurrence)
   :map iedit-mode-occurrence-keymap
   ("M-n" . iedit-next-occurrence)
   ("M-p" . iedit-prev-occurrence)
   ("C-g" . -iedit-quit)
   :map isearch-mode-map
   ("C-c e i" . iedit-mode-from-isearch))
  :custom
  (iedit-toggle-key-default nil)
  ;; Messes with undo if turned on...
  (iedit-auto-buffering nil)
  :config
  (defvar --iedit-occurrence-timer nil)

  (defun --iedit-occurrence-stop-timer ()
    (when --iedit-occurrence-timer
      (setq-local
       --iedit-occurrence-timer
       (cancel-timer --iedit-occurrence-timer))))

  (defun -iedit-quit ()
    (interactive)
    (--iedit-occurrence-stop-timer)
    (iedit-done))

  (defun --iedit-at-occurrence-p ()
    (or (get-char-property (point) 'iedit-occurrence-overlay-name)
        ;; Check 1 char to the left when moving at end of occurrence
        (get-char-property (- (point) 1) 'iedit-occurrence-overlay-name)))

  (defun --iedit-occurrence-exit ()
    (condition-case nil
        (unless (--iedit-at-occurrence-p)
          (-iedit-quit))
      (error (--iedit-occurrence-stop-timer))))

  (defun --iedit-occurrence-start-timer ()
    (unless --iedit-occurrence-timer
      (setq-local
       --iedit-occurrence-timer
       (run-with-timer 0 0.25 #'--iedit-occurrence-exit))))

  (defun -iedit-ensure (&optional arg)
    (interactive "P")
    (unless iedit-mode
      (when (and (bound-and-true-p lispy-mode) (lispy-left-p))
        (forward-char 1))
      (if arg
          (iedit-mode 0)
        (iedit-mode))
      (--iedit-occurrence-start-timer)))

  (defun -iedit-next-occurrence (&optional arg)
    (interactive "P")
    (progn
      (-iedit-ensure arg)
      (iedit-next-occurrence)))

  (defun -iedit-prev-occurrence (&optional arg)
    (interactive "P")
    (progn
      (-iedit-ensure arg)
      (iedit-prev-occurrence)))

  (defun -iedit-ensure-function (&optional arg)
    (interactive "P")
    (if iedit-mode
        (-iedit-quit)
      (-iedit-ensure (not arg)))))

(use-package separedit
  :straight t
  :bind ("C-c '" . #'separedit)
  :custom
  (separedit-default-mode 'markdown-mode))

;;; Built-ins

(use-package prog-mode
  :hook ((prog-mode . auto-fill-mode)
         (prog-mode . display-line-numbers-mode))
  :custom
  (display-line-numbers-width-start t)
  (display-line-numbers-grow-only t))

(use-package conf-mode
  :preface (defun config-prog-run-hooks () (run-hooks 'prog-mode-hook))
  :after prog-mode
  :config (add-hook 'conf-mode-hook #'config-prog-run-hooks))

(use-package autoinsert
  :disabled t
  :hook (after-init . auto-insert-mode))

(provide 'config-prog-mode)
;;; config-prog-mode.el ends here
