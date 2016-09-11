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


;; * Modeline

(use-package all-the-icons :ensure t :defer t)

(use-package spaceline :ensure t :demand t)

(use-package spaceline-config
  :demand t
  :init
  (progn
    ;; Theme definition
    (spaceline-emacs-theme)

    ;; Noisy
    (spaceline-toggle-minor-modes-off)

    ;; Cause too many refreshes
    (spaceline-toggle-line-column-off)
    (spaceline-toggle-line-off)

    (after 'anzu
      (setq anzu-cons-mode-line-p nil))

    (after (spaceline spaceline-config spaceline-segments persp-mode eyebrowse)
      (require 'ace-window nil t )
      (setq spaceline-workspace-numbers-unicode t
            spaceline-window-numbers-unicode t)

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
       '(((buffer-id remote-host ace-window-number) :face highlight-face))))))

(use-package powerline
  :ensure t
  :commands (powerline-reset)
  :defer t
  :config
  (progn
    (setq powerline-default-separator 'utf-8)
    (advice-add
     'load-theme :after
     (lambda (_theme &optional _no-confirm _no-enable)
       (powerline-reset)))))

(provide 'config-modeline)
;;; config-modeline.el ends here
