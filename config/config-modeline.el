;;; config-modeline.el --- Modeline config           -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Julien Fantin

;; Author: Julien Fantin <julienfantin@gmail.com>
;; Keywords: faces

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

;; * Packages

(use-package all-the-icons
  :ensure t
  :defer t
  :commands (all-the-icons-icon-for-buffer))

(use-package spaceline-segments :demand t :after spaceline)

(use-package spaceline
  :ensure t
  :demand t
  :commands (config-modeline-install)
  :functions (spaceline-install)
  :init (after-init #'config-modeline-install)
  :config
  (progn
    (validate-setq spaceline-highlight-face-func #'spaceline-highlight-face-default)

    (after 'anzu
      (validate-setq anzu-cons-mode-line-p nil))

    (spaceline-define-segment config-modeline-eyebrowse
      (when (bound-and-true-p eyebrowse-mode)
        (let* ((num (eyebrowse--get 'current-slot))
               (num-str (int-to-string num))
               (num-tag (when num (nth 2 (assoc num (eyebrowse--get 'window-configs))))))
          (if (and num-tag (< 0 (length num-tag)))
              (format "%s[%s]" num-str num-tag)
            num-str))))

    (spaceline-define-segment config-modeline-version-control
      "Version control information."
      (when (bound-and-true-p vc-mode)
        (let ((icon (all-the-icons-alltheicon "git"))
              (sym (when (buffer-file-name)
                     (pcase (vc-state (buffer-file-name))
                       (`up-to-date " ")
                       (`edited " *")
                       (`added " +")
                       (`unregistered " ??")
                       (`removed " -")
                       (`needs-merge " M")
                       (`needs-update " X")
                       (`ignored " ")
                       (_ " Unk"))))
              (desc (s-replace "Git:" "" vc-mode)))
          (powerline-raw (concat icon sym desc " ")))))

    (spaceline-define-segment config-modeline-buffer-icon
      (when (stringp (all-the-icons-icon-for-buffer))
        (substring-no-properties (all-the-icons-icon-for-buffer))))

    (spaceline-define-segment config-modeline-persp
      (when (bound-and-true-p persp-mode)
        (let ((name (safe-persp-name (get-frame-persp))))
          (if (file-directory-p name)
              (file-name-nondirectory (directory-file-name name))
            name))))

    (spaceline-define-segment config-modeline-ace-window-number
      (when (and (featurep 'ace-window)
                 (> (length (aw-window-list)) 1))
        (when-let ((pos (cl-position (selected-window) (aw-window-list)))
                   (key (nth pos aw-keys)))
          (char-to-string key))))

    (spaceline-define-segment config-modeline-buffer-id
      "Name of buffer."
      (concat " " (s-trim (powerline-buffer-id 'mode-line-buffer-id)) " "))

    (defun config-modeline-install ()
      (spaceline-install
       `(config-modeline-ace-window-number
         ((config-modeline-persp config-modeline-eyebrowse) :separator ":")
         buffer-id
         config-modeline-buffer-icon
         ;; (buffer-size  hud buffer-position)
         ;; line-column
         (remote-host (global :when active)) selection-info)
       `(((flycheck-error flycheck-warning flycheck-info) :when active)
         (buffer-encoding-abbrev :when (not (eq buffer-file-coding-system 'utf-8-unix)))
         (config-modeline-version-control :when active)))
      (setq-default mode-line-format '("%e" (:eval (spaceline-ml-main))))
      (force-mode-line-update))
    (config-modeline-install)))

(use-package powerline
  :ensure t
  :defer t
  :functions (powerline-reset)
  :config
  (progn
    (validate-setq powerline-default-separator nil)
    (advice-add 'load-theme :after
                (lambda (_theme &optional _no-confirm _no-enable)
                  (powerline-reset)))))

(provide 'config-modeline)
;;; config-modeline.el ends here
