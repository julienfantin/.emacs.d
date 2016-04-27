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

(defvar config-last-selected-layout nil
  "Previously selected layout.")


;; * Eyebrowse

(use-package eyebrowse
  :ensure t
  :defer t
  :init (after-init #'eyebrowse-mode))


;; * persp-mode

(use-package persp-mode
  :ensure t
  :defer t
  :init (after-init #'persp-mode)
  :preface
  ;; Clashes with projectile
  (defvar persp-keymap-prefix)
  :functions
  (config-layouts--autosave config-layouts--sync-last-layout)
  :config
  (progn
    (setq
     persp-auto-resume-time 0.01
     persp-nil-name "emacs"
     persp-reset-windows-on-nil-window-conf nil
     persp-set-last-persp-for-new-frames nil
     persp-save-dir (user-var-directory "persp/"))
    ;; Ensures the auto-save file is created or persp-mode will error
    (let ((file (expand-file-name persp-auto-save-fname persp-save-dir)))
      (unless (file-exists-p file)
        (user-var-file file)
        (persp-save-state-to-file)))
    ;; Track the last persp so we can toggle back and forth
    (defun config-layouts--sync-last-layout (persp &optional _frame-or-window _new-frame)
      (setq config-last-selected-layout persp-last-persp-name))
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
;; ** Eyebrowse

;; NOTE: copied from the spacemacs layouts layer

(after (persp-mode eyebrowse)
  (defun config-layouts--eyebrowse-save-for-perspective (&optional frame)
    "Save FRAME's eyebrowse workspace to FRAME's perspective.
FRAME defaults to the current frame."
    (let ((persp (get-frame-persp frame)))
      (set-persp-parameter
       'eyebrowse-window-configs (eyebrowse--get 'window-configs frame) persp)
      (set-persp-parameter
       'eyebrowse-current-slot (eyebrowse--get 'current-slot frame) persp)
      (set-persp-parameter
       'eyebrowse-last-slot (eyebrowse--get 'last-slot frame) persp)))

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

  (add-hook 'persp-before-switch-functions #'config-layouts--eyebrowse-udpate-for-perspective)
  (add-hook 'eyebrowse-post-window-switch-hook #'config-layouts--eyebrowse-save-for-perspective)
  (add-hook 'persp-activated-functions         #'config-layouts--eyebrowse-load-for-perspective))

;; ** Projectile

;; NOTE: adapted from persp-projectile (perspective.el integration)

(after (projectile persp-mode)
  (defun -layouts-projectile-switch (project-dir)
    "Switch to a project or perspective we have visited before.
If the perspective of corresponding project does not exist, this
function will call `persp-switch' to create one and switch to
that before `projectile-switch-project' invokes
`projectile-switch-project-action'.
Otherwise, this function calls `persp-switch' to switch to an
existing perspective of the project unless we're already in that
perspective."
    (interactive (list (projectile-completing-read
                        "Switch to project: "
                        (projectile-relevant-known-projects))))
    (let* ((project-name (file-name-nondirectory (directory-file-name project-dir)))
           (persp (gethash project-name *persp-hash*)))
      (cond
       ((not (equal persp (config-layouts--current-layout-name)))
        (persp-switch project-name))
       ((not persp)
        (let ((frame (selected-frame)))
          (persp-switch project-name)
          (projectile-switch-project-by-name project-dir)
          (when (not (equal frame (selected-frame)))
            (with-selected-frame frame
              (persp-kill project-name))))))))
  (define-key projectile-mode-map [remap projectile-switch-project] '-layouts-projectile-switch)
  (defun config-layouts--projectile-persp-bridge (&optional _)
    "Load a persp for the current project and switch to it."
    (when-let ((project-name (projectile-project-name)))
      (persp-load-from-file-by-names project-name nil (list project-name))
      (persp-switch project-name)))
  ;; This will break if 'projectile-switch-project-action' is customized to
  ;; something else...
  (advice-add #'projectile-dired :before #'config-layouts--projectile-persp-bridge)
  (advice-add #'projectile-find-file :before  #'config-layouts--projectile-persp-bridge)
  (defun config-layouts--projectile-rename-frame (frame &optional _new-frame)
    "Rename initial perspective to `projectile-project-name'."
    (with-selected-frame frame
      (when (projectile-project-p)
        (persp-rename (projectile-project-name)))))
  (advice-add #'persp-init-frame :after #'config-layouts--projectile-rename-frame))

;; ** Spaceline

(after (spaceline spaceline-config spaceline-segments persp-mode eyebrowse)
  (require 'ace-window nil t )
  (setq spaceline-workspace-numbers-unicode t
        spaceline-window-numbers-unicode t)
;;;###autoload
  (defun config-layouts--current-layout-name ()
    "Get name of the current perspective."
    (safe-persp-name (get-frame-persp)))

  (spaceline-define-segment ace-window-number
    "The current window number. Requires `ace-window-mode' to be enabled."
    (when-let ((pos (cl-position (selected-window) (aw-window-list)))
               (key (nth pos aw-keys))
               (str (char-to-string key)))
      (if spaceline-window-numbers-unicode
          (spaceline--unicode-number str)
        str))
    :when (and (boundp 'ace-window-mode)
               (> (length (aw-window-list)) 1)))

  (spaceline-define-segment eyebrowse
    (let* ((num (eyebrowse--get 'current-slot))
           (num-str (spaceline--unicode-number (int-to-string num)))
           (num-tag (when num (nth 2 (assoc num (eyebrowse--get 'window-configs))))))
      (if (and num-tag (< 0 (length num-tag)))
          (format "%s %s" num-str num-tag)
        num-str))
    :when (bound-and-true-p eyebrowse-mode))

  (spaceline-define-segment persp
    (let ((name (config-layouts--current-layout-name)))
      (if (file-directory-p name)
          (file-name-nondirectory (directory-file-name name))
        name))
    :when (bound-and-true-p persp-mode))

  (apply
   'spaceline--theme
   '(persp eyebrowse)
   '(((buffer-id remote-host ace-window-number) :face highlight-face))))


;; * Commands

(after 'persp-mode
  (defun -layouts-jump-to-last-layout ()
    "Switch to the previous layout, if it exists."
    (interactive)
    (unless (eq 'non-existent
                (gethash config-last-selected-layout
                         *persp-hash* 'non-existent))
      (persp-switch config-last-selected-layout))))

(after (persp-mode ivy)
  (defun -layouts-current-layout-buffers ()
    "Ivy completion for current persp buffers."
    (interactive)
    (with-persp-buffer-list
     ()
     (let ((ivy-use-virtual-buffers))
       (ivy-switch-buffer))))
  (defun -layouts ()
    "Ivy persp switch or create."
    (interactive)
    (ivy-read
     "Persp:" (persp-names)
     :action #'persp-switch))
  (ivy-set-actions
   'ivy-persp
   '(("k" persp-kill-without-buffers "Kill persp")
     ("K" persp-kill "Kill with buffers"))))

(after (persp-mode projectile ivy)
  (defun -layout-switch-project (arg)
    (interactive "P")
    (ivy-read
     "Switch to Project Layout: "
     (if (projectile-project-p)
         (cons (abbreviate-file-name (projectile-project-root))
               (projectile-relevant-known-projects))
       projectile-known-projects)
     :action
     (lambda (project)
       (let ((persp-reset-windows-on-nil-window-conf t))
         (persp-switch project)
         (let ((projectile-completion-system 'ivy))
           (projectile-switch-project-by-name project)))))))

(provide 'config-layouts)
;;; config-layouts.el ends here
