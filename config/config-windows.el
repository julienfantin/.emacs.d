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
  :commands (aw-window-list)
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
  :config (validate-setq windresize-default-increment 5))


;; * Content centering

(defvar config-windows-centered-window-min-width 80)
(defvar config-windows-centered-window-max-width 240)
(defvar config-windows-centered-window-padding 5)
(defvar config-windows-centered-window-max-buffer-size 1e5)

(use-package centered-window-mode
  :ensure t
  :defer t
  :init
  (after-init
   #'(lambda ()
       (centered-window-mode 1)
       (config-windows-cwm-set-width)
       (cwm/center)))
  :config
  (progn
    (defun config-windows-longest-line-length (buffer)
      (when (< 0
               (buffer-size buffer)
               config-windows-centered-window-max-buffer-size)
        (with-current-buffer buffer
          (thread-last (split-string (buffer-string) "\n" t)
            (mapcar #'length)
            (apply #'max)))))
    (defun config-windows-cwm-set-width (&optional win)
      (when-let ((buffer (window-buffer win))
                 (len (config-windows-longest-line-length buffer)))
        (setq
         cwm-centered-window-width
         (thread-last len
           (+ config-windows-centered-window-padding)
           (max config-windows-centered-window-min-width)
           (min config-windows-centered-window-max-width)))))
    (after cider
      (add-hook 'cider-popup-buffer-mode-hook #'cwm/center))))


;; * Buffer display rules

(use-package shackle
  :ensure t
  :defer t
  :init (after-init #'shackle-mode)
  :config
  (validate-setq
   shackle-default-size 0.38
   shackle-rules
   '(("*Help*" :select t :popup t :align t)
     ("*cider-test-report*" :select t :popup t :align t)
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
