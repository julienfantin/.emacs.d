;;; config-gui.el --- Emacs internal GUI (frames & terminal)  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Julien Fantin

;; Author: Julien Fantin(require 'use-config) <julienfantin@gmail.com>
;; Keywords: internal, terminals, frames

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


;; * Built-ins

(setq window-resize-pixelwise t)

(use-package fringe
  :init (fringe-mode 4)
  :config
  (progn
    (setq indicate-empty-lines t
          indicate-buffer-boundaries t
          indicate-unused-lines t)
    (setf (cdr (assq 'continuation fringe-indicator-alist))
          '(nil right-curly-arrow))))

(use-package hl-line
  :defer t
  :init
  (progn
    (add-hook 'dired-mode-hook #'hl-line-mode)
    (after 'magit (add-hook 'magit-mode-hook #'hl-line-mode)))
  :config
  (setq global-hl-line-sticky-flag nil
        hl-line-sticky-flag nil))

(use-package linum
  :disabled t
  :defer t
  :init (add-hook 'prog-mode-hook #'linum-mode)
  :defines (config-frame-linum-timer)
  :functions (linum-update-current)
  :preface (defvar-local config-frame-linum-timer nil)
  :config
  (progn
    (setq linum-delay t)
    (setq linum-format " %4d ")
    ;; Redefine linum-schedule for slower updates, fixes laggy scrolling
    ;; behavior
    (defun linum-schedule ()
      (when (timerp config-frame-linum-timer)
        (cancel-timer config-frame-linum-timer))
      (setq config-frame-linum-timer
            (run-with-idle-timer 1 nil #'linum-update-current)))))


;; * Packages

(use-package rich-minority
  :ensure t
  :init (rich-minority-mode 1)
  :config (setq rm-whitelist '(" λ")))

(use-package focus :ensure t :defer t)

(provide 'config-gui)
;;; config-gui.el ends here
