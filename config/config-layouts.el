;;; config-layouts.el --- Windows layouts management  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Julien Fantin

;; Author: Julien Fantin(require 'use-config) <julienfantin@gmail.com>
;; Keywords: convenience, frames

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

;; Most of this code has been adapted from the spacemacs layout layer.

;;; Code:
(require 'use-config)
(eval-when-compile
  (require 'cl-lib))


;; * Customs

(defcustom config-layouts-restore-on-init nil
  "Restore the last perspectives when Emacs initializes."
  :group 'config-layouts
  :type 'boolean)

;; * Vars

(defvar config-layouts--last-selected-layout nil
  "Previously selected layout.")


;; * Eyebrowse

(use-package eyebrowse
  :ensure t
  :defer t
  :init (after-init #'eyebrowse-mode)
  :config
  (setq eyebrowse-switch-back-and-forth t))


;; * persp-mode

(use-package persp-mode
  :ensure t
  :defer t
  :init (after-init #'persp-mode)
  ;; Clashes with projectile
  :preface (defvar persp-keymap-prefix)
  :functions (config-layouts--autosave config-layouts--sync-last-layout)
  :config
  (progn
    (setq persp-nil-name "default"
          persp-auto-resume-time (if config-layouts-restore-on-init 0.01 0)
          persp-reset-windows-on-nil-window-conf nil
          persp-set-last-persp-for-new-frames nil)
    ;; Ensures the auto-save file is created or persp-mode will error
    (when config-layouts-restore-on-init
      (let ((file (expand-file-name persp-auto-save-fname persp-save-dir)))
        (unless (file-exists-p file)
          (user-var-file file)
          (persp-save-state-to-file))))

    (defun config-layouts--current-layout-name ()
      (safe-persp-name (get-frame-persp)))

    ;; Track the last persp so we can toggle back and forth
    (defun config-layouts--sync-last-layout (_persp &optional _frame-or-window _new-frame)
      (setq config-layouts--last-selected-layout persp-last-persp-name))
    (advice-add #'persp-activate :before #'config-layouts--sync-last-layout)

    ;; Generate commands to switch by position
    (defun config-layouts--switch-by-pos (pos)
      "Switch to perspective of position POS."
      (let ((persp-to-switch
             (nth pos (persp-names-current-frame-fast-ordered))))
        (if persp-to-switch
            (persp-switch persp-to-switch)
          (when (y-or-n-p
                 (concat "Perspective in this position doesn't exist.\n"
                         "Do you want to create one? "))
            (let ((persp-reset-windows-on-nil-window-conf t))
              (persp-switch nil))))))
    (dolist (i (number-sequence 9 0 -1))
      (eval `(defun ,(intern (format "-layouts-switch-to-%s" i)) nil
               ,(format "Switch to layout %s." i)
               (interactive)
               (config-layouts--switch-by-pos ,(if (eq 0 i) 9 (1- i))))))))


;; * Integration
;; ** Eyebrowse x persp-mode

;; NOTE: copied from the spacemacs layouts layer

(after (persp-mode eyebrowse)
  (defun config-layouts--eyebrowse-save-for-perspective (&optional frame)
    "Save FRAME's eyebrowse workspace to FRAME's perspective.
FRAME defaults to the current frame."
    (let ((persp (get-frame-persp frame)))
      (set-persp-parameter 'eyebrowse-window-configs (eyebrowse--get 'window-configs frame) persp)
      (set-persp-parameter 'eyebrowse-current-slot (eyebrowse--get 'current-slot frame) persp)
      (set-persp-parameter 'eyebrowse-last-slot (eyebrowse--get 'last-slot frame) persp)))

  (defun config-layouts--eyebrowse-load-for-perspective (&optional _)
    "Load an eyebrowse workspace according to a perspective's parameters.
FRAME's perspective is the perspective that is considered, defaulting to
the current frame's perspective.
If the perspective doesn't have a workspace, create one."
    (let* ((frame (selected-frame))
           (persp (get-current-persp))
           (window-configs (persp-parameter 'eyebrowse-window-configs persp))
           (current-slot (persp-parameter 'eyebrowse-current-slot persp))
           (last-slot (persp-parameter 'eyebrowse-last-slot persp)))
      (if window-configs
          (progn
            (eyebrowse--set 'window-configs window-configs frame)
            (eyebrowse--set 'current-slot current-slot frame)
            (eyebrowse--set 'last-slot last-slot frame)
            (eyebrowse--load-window-config current-slot))
        (eyebrowse--set 'window-configs nil frame)
        (eyebrowse-init frame)
        (config-layouts--eyebrowse-save-for-perspective frame))))

  (defun config-layouts--eyebrowse-udpate-for-perspective (_name _window)
    (eyebrowse--update-window-config-element
     (eyebrowse--current-window-config
      (eyebrowse--get 'current-slot)
      (eyebrowse--get 'current-tag)))
    (config-layouts--eyebrowse-save-for-perspective))

  (add-hook 'persp-before-switch-functions     #'config-layouts--eyebrowse-udpate-for-perspective)
  (add-hook 'eyebrowse-post-window-switch-hook #'config-layouts--eyebrowse-save-for-perspective)
  (add-hook 'persp-activated-functions         #'config-layouts--eyebrowse-load-for-perspective))

;; ** Projectile x persp-mode

(defvar config-layouts-after-find-file-hook nil)

(defun config-layouts-after-find-file-advice (&rest _args)
  "Run `config-layouts-after-find-file-hook'."
  (run-hooks 'config-layouts-after-find-file-hook))

(advice-add 'find-file :after 'config-layouts-after-find-file-advice)

(after 'projectile
  (def-auto-persp "projectile"
    :parameters '((dont-save-to-file . t))
    :hooks '(config-layouts-after-find-file-hook)
    :switch 'frame
    :predicate
    #'(lambda (buffer)
        (and (buffer-file-name buffer) (projectile-project-p)))
    :get-name-expr
    #'(lambda ()
        (abbreviate-file-name (projectile-project-root))))

  (setq persp-add-buffer-on-find-file 'if-not-autopersp)

  (add-hook 'persp-after-load-state-functions
            #'(lambda (&rest _args)
                (persp-auto-persps-pickup-buffers)) t))


;; * Commands

(after 'persp-mode
  (defun -layouts-jump-to-last-layout ()
    "Switch to the previous layout, if it exists."
    (interactive)
    (let ((last-layout (gethash config-layouts--last-selected-layout *persp-hash* 'none)))
      (unless (eq 'none last-layout)
        (persp-switch config-layouts--last-selected-layout)))))

(provide 'config-layouts)
;;; config-layouts.el ends here
