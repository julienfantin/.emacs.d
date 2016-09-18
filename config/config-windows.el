;;; config-windows.el --- Windows navigation and management  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Julien Fantin

;; Author: Julien Fantin(require 'use-config) <julienfantin@gmail.com>
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
(require 'use-config)


;; * Navigation

(use-package ace-window
  :ensure t
  :defer t
  :config
  (setq aw-keys '(?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9 ?0)
        aw-scope 'frame))

(use-package windmove)


;; * Undo and redo

(use-package winner :defer t :init (after-init #'winner-mode))


;; * Focus

(use-package zygospore :ensure t :defer t)


;; * Drag

(use-package buffer-move :ensure t :defer t)


;; * Resize

(use-package windresize
  :ensure t
  :defer t
  :commands (windresize-left windresize-right windresize-up windresize-down)
  :config (setq windresize-default-increment 5))


;; * Content centering

(use-package centered-window-mode
  :ensure t
  :defer t
  :init (after-init
         (lambda ()
           (centered-window-mode 1)
           (cwm/center)))
  :config
  (progn
    (defun config-windows-cwm-set-width (&optional win)
      (let ((w (/ (frame-width (window-frame win)) 2)))
        (setq cwm-centered-window-width
              (if (cl-oddp w) w (- w 1)))))
    (advice-add #'cwm/calculate-fringe :before #'config-windows-cwm-set-width)
    (after cider
      (add-hook 'cider-popup-buffer-mode-hook #'cwm/center))))


;; * Buffer display rules

(use-package shackle
  :ensure t
  :defer t
  :init (after-init #'shackle-mode)
  :config
  (setq shackle-rules
        '(("*cider-test-report*" :same t :inhibit-window-quit t)
          ("*cider-result*" :select t))))


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
