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
  :disabled t
  :ensure t
  :defer t
  :init (after-init #'centered-window-mode)
  :config
  (after cider
    (advice-add
     #'cider--pprint-eval-form :after
     #'(lambda (sexp)
         (when centered-window-mode
           (cwm/center))))))


;; * Purpose

(use-package window-purpose
  :disabled t
  :ensure t
  :init (after-init #'purpose-mode)
  :config
  (progn
    (setq purpose-preferred-prompt 'ivy)
    ;; TODO test vs code
    (add-to-list 'purpose-user-mode-purposes '(term-mode . terminal))
    (add-to-list 'purpose-user-mode-purposes '(eshell-mode . terminal))
    (add-to-list 'purpose-user-regexp-purposes '("\\*.?shell" . terminal))
    (add-to-list 'purpose-user-mode-purposes '(cider-repl-mode . repl))
    (add-to-list 'purpose-user-mode-purposes '(inferior-emacs-lisp-mode . repl))
    (purpose-compile-user-configuration)))

(use-package window-purpose-x
  :disabled t
  :defer t
  :after window-purpose
  :config
  (progn
    (purpose-x-popwin-setup)
    (purpose-x-magit-single-on)
    (cl-pushnew "*Pp Macroexpand Output*" purpose-x-popwin-buffer-names :test 'equal)))

;; ** ivy commands

;; NOTE: adapted from helm-purpose

(after (window-purpose ivy)
  (defvar ivy-purpose--current-purpose 'edit)
  (defun ivy--purpose-buffers-list ()
    "Return names of buffers with the same purpose as current buffer.
The purpose is decided by `ivy-purpose--current-purpose'."
    (mapcar #'buffer-name
            (delq (current-buffer)
                  (purpose-buffers-with-purpose
                   ivy-purpose--current-purpose))))

  (defun ivy-purpose-buffers-ignore-purpose ()
    "Switch buffers without purpose."
    (interactive)
    (without-purpose
      (call-interactively #'switch-to-buffer)))

  (defun ivy-purpose-switch-buffer-with-purpose (&optional purpose)
    "Switch to buffer, choose from buffers with purpose PURPOSE.
PURPOSE defaults to the purpose of the current buffer."
    (interactive)
    (setq ivy-purpose--current-purpose
          (or purpose (purpose-buffer-purpose (current-buffer))))
    (ivy-read "Buffer: " (ivy--purpose-buffers-list)))

  (defun ivy-purpose-switch-buffer-with-some-purpose ()
    "Choose a purpose, then switch to a buffer with that purpose."
    (interactive)
    (ivy-purpose-switch-buffer-with-purpose
     (purpose-read-purpose
      "Purpose: "
      ;; don't show purposes that have no buffers
      (cl-delete-if-not #'purpose-buffers-with-purpose
                        (purpose-get-all-purposes))
      t)))
  (defalias 'purpose-friendly-switch-buffer #'ivy-switch-buffer)
  (defalias 'purpose-friendly-switch-buffer-other-window #'ivy-switch-buffer-other-window)
  (bind-keys
   :map purpose-mode-map
   ([remap purpose-friendly-switch-buffer] #'ivy-purpose-switch-buffer-with-purpose)
   ([remap purpose-switch-buffer-with-purpose] #'ivy-purpose-switch-buffer-with-purpose)
   ([remap switch-buffer-without-purpose] #'ivy-purpose-buffers-ignore-purpose)
   ([remap purpose-switch-buffer-with-some-purpose] #'ivy-purpose-switch-buffer-with-some-purpose)))

;; ** Popup terminal

(after 'window-purpose
  (defun config-windows--purpose-terminal-delete-windows ()
    "Delete all windows with purpose 'terminal."
    (interactive)
    (mapc #'delete-window (purpose-windows-with-purpose 'terminal)))

  (defun config-windows--purpose-terminal-toggle (old-fun &rest args)
    "Toggle window with purpose 'terminal.
Delete 'terminal window if it exists, open it if it doesn't.
If no 'terminal buffer exists, create a `shell' buffer and open it."
    (interactive)
    (cond
     ;; always call when using C-u, used to create new eshells
     ((equal (list 4) current-prefix-arg)
      (call-interactively old-fun args))
     ;; delete window
     ((purpose-windows-with-purpose 'terminal)
      (config-windows--purpose-terminal-delete-windows))
     ;; display existing buffer
     ((purpose-buffers-with-purpose 'terminal)
      (if (> (length (purpose-buffers-with-purpose 'terminal)) 1)
          (purpose-switch-buffer-with-purpose-other-window 'terminal)
        (purpose-switch-buffer-other-window (car (purpose-buffers-with-purpose 'terminal)))))
     ;; create and display `shell' buffer
     (t (call-interactively old-fun args))))

  (defun config-windows--purpose-terminal-auto-dedicate (window)
    "Dedicate WINDOW's purpose if it is a 'terminal window."
    (when (eql (purpose-window-purpose window) 'terminal)
      (purpose-set-window-purpose-dedicated-p window t)))

  ;; make purpose display 'terminal windows at bottom
  (add-to-list 'purpose-special-action-sequences
               '(terminal purpose-display-reuse-window-buffer
                          purpose-display-reuse-window-purpose
                          purpose-display-at-bottom))
  (advice-add #'eshell :around #'config-windows--purpose-terminal-toggle)
  (advice-add #'shell :around #'config-windows--purpose-terminal-toggle)
  (advice-add #'term :around #'config-windows--purpose-terminal-toggle)
  (advice-add #'ansi-term :around #'config-windows--purpose-terminal-toggle)
  ;; auto-dedicate 'terminal windows
  (add-hook 'purpose-display-buffer-functions #'config-windows--purpose-terminal-auto-dedicate))


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
