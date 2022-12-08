;;; config-windows.el --- Windows navigation and management  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Julien Fantin

;; Author: Julien Fantin <julienfantin@gmail.com>
;; Keywords: frames, convenience

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

;; Display rules

;; - Info and documentation
;; always select, so it can be dismissed quickly
;; if in code -> new window
;; if traversing from existing doc window -> reuse window

;; - repl
;; right side


;; term
;; from top whole width

;;; Config

(defvar Φ (- 1 (/ 1 1.618033988749895)))

;;; Built-ins

(use-package emacs
  :custom
  (scroll-preserve-screen-position t)
  ;; Required for compatibility with windmove display actions
  ;; (switch-to-buffer-obey-display-actions t)
  (cursor-in-non-selected-windows nil)
  ;; always select the help window so we can dismiss it quickly
  (help-window-select t)
  ;; evenly split windows
  (window-combination-resize t))

(use-package popper
  :disabled t
  :straight t
  :bind (("C-`"   . popper-toggle-latest)
         ("M-`"   . popper-cycle)
         ("C-M-`" . popper-toggle-type))
  :init
  (setq popper-reference-buffers
        '("\\*Messages\\*"
          "\\*Warnings\\*"
          "\\*Compile-Log\\*"
          "Output\\*$"
          "\\*Async Shell Command\\*"
          "\\*vterm\\*"
          help-mode
          compilation-mode))
  (popper-mode +1)
  (popper-echo-mode +1))

(use-package emacs
  :custom
  (display-buffer-alist
   `(;; ("\\`\\*Async Shell Command\\*\\'"
     ;;  (display-buffer-no-window))
     ;; ;; bottom side window
     ;; ("\\*\\(Flycheck errors\\).*"
     ;;  (display-buffer-in-side-window)
     ;;  (window-height . ,(* Φ 0.5))
     ;;  (side . bottom)
     ;;  (slot . -1))
     ;; ("\\*\\(Backtrace\\|Warnings\\|Compile-Log\\)\\*"
     ;;  (display-buffer-in-side-window)
     ;;  (window-height . ,Φ)
     ;;  (side . bottom)
     ;;  (slot . -1))
     ;; top side window
     ;; ("^\\(\\*e?shell\\|vterm\\).*"
     ;;  (display-buffer-in-side-window)
     ;;  (window-height . ,Φ)
     ;;  (side . top)
     ;;  (slot . 0))
     ;; Right side window
     ("^\\*\\(\\help\\|info\\).*"
      (display-buffer-reuse-window display-buffer-same-window)
      ;; (window-width . (- 1 ,Φ))
      ;; (side . right)
      ;; (slot . 0)
      )
     ;; ("^\\*cider.*"
     ;;  (display-buffer-in-side-window)
     ;;  (window-width . (- 1 ,Φ))
     ;;  (side . right)
     ;;  (slot . 0))
     ("\\*Custom.*"
      (display-buffer-reuse-window)
      (window-width . ,Φ)
      (side . right)
      (slot . -2))))
  :hook
  ((help-mode . visual-line-mode)
   (custom-mode . visual-line-mode))
  :hook (after-init . global-visual-line-mode)
  :bind (("s-d" . dedicated-mode)
         ("s-n" . next-buffer)
         ("s-p" . previous-buffer)
         ("s-o" . other-window)
         ;; ("s-2" . split-window-below)
         ;; ("s-3" . split-window-right)
         ;; ("s-0" . delete-window)
         ;; ("s-1" . delete-other-windows)
         ("s-\\" . balance-windows-area)
         ("s-q" . window-toggle-side-windows))
  :config
  (define-minor-mode dedicated-mode
    "Minor mode for dedicating windows.
This minor mode dedicates the current window to the current buffer.
The code is taken from here: https://github.com/skeeto/.emacs.d/blob/master/lisp/extras.el"
    :init-value nil
    :lighter " [D]"
    (let* ((window (selected-window))
           (dedicated (window-dedicated-p window)))
      (set-window-dedicated-p window (not dedicated))
      (message "Window %sdedicated to %s"
               (if dedicated "no longer " "")
               (buffer-name)))))

(use-package tab-bar
  :custom
  (tab-bar-select-tab-modifiers '(super))
  (tab-bar-show nil))

(use-package windmove
  :bind (("s-h" . nil)
         ("s-i" . windmove-up)
         ("s-j" . windmove-left)
         ("s-k" . windmove-down)
         ("s-l" . windmove-right)
         ("M-s-j" . windmove-display-left)
         ("M-s-l" . windmove-display-right)
         ("M-s-i" . windmove-display-up)
         ("M-s-k" . windmove-display-down)
         ("M-s-f" . windmove-display-new-frame)
         ("M-s-t" . windmove-display-new-tab)
         ("C-x s-i" . windmove-delete-up)
         ("C-x s-j" . windmove-delete-left)
         ("C-x s-k" . windmove-delete-down)
         ("C-x s-l" . windmove-delete-right)
         ("s-I" . windmove-swap-states-up)
         ("s-J" . windmove-swap-states-left)
         ("s-K" . windmove-swap-states-down)
         ("s-L" . windmove-swap-states-right)))

(use-package winner
  :hook (after-init . winner-mode)
  :bind
  (("s-u" . winner-undo)
   ("s-r" . winner-redo))
  :custom
  (winner-dont-bind-my-keys t))

;;; Third-party

(use-package windresize
  :straight t
  :commands
  (windresize-up windresize-left windresize-down windresize-right)
  :bind (("M-s-I" . windresize-up)
         ("M-s-J" . windresize-left)
         ("M-s-K" . windresize-down)
         ("M-s-L" . windresize-right))
  :custom
  (windresize-increment 10)
  (windresize-default-increment 10))

(use-package winum
  :straight t
  :hook (after-init . winum-mode)
  :bind
  (("C-c 1" . winum-select-window-1)
   ("C-c 2" . winum-select-window-2)
   ("C-c 3" . winum-select-window-3)
   ("C-c 4" . winum-select-window-4)
   ("C-c 5" . winum-select-window-5)
   ("C-c 6" . winum-select-window-6)
   ("C-c 7" . winum-select-window-7)
   ("C-c 8" . winum-select-window-8)
   ("C-c 9" . winum-select-window-9)))


;;; Commands

;;;###autoload
(defun -switch-to-last-window ()
  "Switch to the most recently used window."
  (interactive)
  (let ((win (get-mru-window t t t)))
    (unless win (error "Last window not found"))
    (let ((frame (window-frame win)))
      (raise-frame frame)
      (select-frame frame)
      (select-window win))))

(bind-key "s-<tab>" '-switch-to-last-window)

(provide 'config-windows)
;;; config-windows.el ends here
