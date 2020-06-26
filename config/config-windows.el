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

(defvar Φ (/ 1 1.618033988749895))

(defvar -Φ (- 1 Φ))

(use-package emacs
  :custom
  (scroll-preserve-screen-position t)
  ;; Required for compatibility with windmove display actions
  (switch-to-buffer-obey-display-actions t))


;; * Windows management

(use-package ace-window
  :straight t
  :commands (aw-window-list aw-switch-to-window)
  :preface
  (dolist (n (number-sequence 1 10))
    (eval
     `(defun ,(intern (format "aw-switch-to-window-%s" n)) ()
        (interactive)
        ,(format "Switch to window at index %s" n)
        (when-let (window (nth (- ,n 1) (aw-window-list)))
          (aw-switch-to-window window)))))
  :bind
  (("C-c 1" . aw-switch-to-window-1)
   ("C-c 2" . aw-switch-to-window-2)
   ("C-c 3" . aw-switch-to-window-3)
   ("C-c 4" . aw-switch-to-window-4)
   ("C-c 5" . aw-switch-to-window-5)
   ("C-c 6" . aw-switch-to-window-6)
   ("C-c 7" . aw-switch-to-window-7)
   ("C-c 8" . aw-switch-to-window-8)
   ("C-c 9" . aw-switch-to-window-9)
   ("C-c 0" . aw-switch-to-window-0))
  :custom
  (aw-keys '(?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9 ?0))
  (aw-scope 'frame))

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
         ("C-x s-l" . windmove-delete-right)))

(use-package winner
  :hook (after-init . winner-mode)
  :bind
  (("s-U" . winner-undo)
   ("s-R" . winner-redo))
  :custom
  (winner-dont-bind-my-keys t))

(use-package zygospore
  :straight t
  :bind
  ("C-x 1" . zygospore-toggle-delete-other-windows))

(use-package buffer-move
  :straight t
  :bind (("s-I" . buf-move-up)
         ("s-J" . buf-move-left)
         ("s-K" . buf-move-down)
         ("s-L" . buf-move-right)))

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


;; * Window configurations

(use-package tab-bar
  :hook
  (after-init . tab-bar-history-mode)
  :bind
  ("C-x t n" . tab-bar-switch-to-next-tab)
  ("C-x t p" . tab-bar-switch-to-prev-tab)
  ("C-x t <tab>" . tab-bar-switch-to-recent-tab))


;; * Buffer display rules

(use-package emacs
  :custom
  (display-buffer-alist
   `(;; bottom side window
     ("\\*\\(Flycheck errors\\).*"
      (display-buffer-in-side-window)
      (window-height . ,-Φ)
      (side . bottom)
      (slot . 0))
     ("\\*\\(Backtrace\\|Warnings\\|Compile-Log\\)\\*"
      (display-buffer-in-side-window)
      (window-height . ,-Φ)
      (side . bottom)
      (slot . -1))
     (".*\\*\\(Completions\\|Embark Live Occur\\).*"
      (display-buffer-in-side-window)
      (window-height . ,-Φ)
      (side . bottom)
      (slot . -2)
      (window-parameters . ((no-other-window . t))))
     ;; top side window
     ("^\\(\\*e?shell\\|vterm\\).*"
      (display-buffer-in-side-window)
      (window-height . ,-Φ)
      (side . top)
      (slot . 0))
     ;; Right side window
     ("\\*Help.*"
      (display-buffer-in-side-window)
      (window-width . ,-Φ)
      (side . right)
      (slot . 0))
     ("\\*Custom.*"
      (display-buffer-in-side-window)
      (window-width . ,-Φ)
      (side . right)
      (slot . 1))))
  :hook ((help-mode . visual-line-mode)
         (custom-mode . visual-line-mode))
  :bind (("s-n" . next-buffer)
         ("s-p" . previous-buffer)
         ("s-o" . other-window)
         ("s-2" . split-window-below)
         ("s-3" . split-window-right)
         ("s-0" . delete-window)
         ("s-1" . delete-other-windows)
         ("s-\\" . balance-windows-area)
         ("s-q" . window-toggle-side-windows)))


;; * Margins

;;*
(use-package emacs
  :hook
  ((window-setup . -set-margins)
   (window-state-change . -set-margins)
   (window-configuration-change . -set-margins))
  :preface
  (defvar config-windows-margin-width 4)
  :config
  (defun -set-margins (&optional _)
    (walk-windows
     (lambda (window)
       (with-current-buffer (window-buffer window)
         (when (or (not (equal config-windows-margin-width left-margin-width))
                   (not (equal config-windows-margin-width right-margin-width)))
           (setq left-margin-width config-windows-margin-width)
           (setq right-margin-width config-windows-margin-width)
           (set-window-buffer window (current-buffer))))))))


;; * Commands

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

;;;###autoload
(defun -window-split-toggle ()
  "Toggle between horizontal and vertical split."
  (interactive)
  (let ((done))
    (dolist (dirs '((right . down) (down . right)))
      (unless done
        (let* ((win (selected-window))
               (nextdir (car dirs))
               (neighbour-dir (cdr dirs))
               (next-win (windmove-find-other-window nextdir win))
               (neighbour1 (windmove-find-other-window neighbour-dir win))
               (neighbour2 (if next-win (with-selected-window next-win
                                          (windmove-find-other-window neighbour-dir next-win)))))
          (setq done (and (eq neighbour1 neighbour2)
                          (not (eq (minibuffer-window) next-win))))
          (if done
              (let* ((other-buf (window-buffer next-win)))
                (delete-window next-win)
                (if (eq nextdir 'right)
                    (split-window-vertically)
                  (split-window-horizontally))
                (set-window-buffer (windmove-find-other-window neighbour-dir) other-buf))))))))

(provide 'config-windows)
;;; config-windows.el ends here
