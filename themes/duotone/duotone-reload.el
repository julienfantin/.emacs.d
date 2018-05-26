;;; duotone-reload.el --- Automatic theme reloading for duotone  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Julien Fantin

;; Author: Julien Fantin <julienfantin@gmail.com>
;; Keywords: convenience, files, faces

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
(require 'duotone-theme)

(defvar duotone-reload--debounce-timer nil)

(defun duotone-reload-apply-theme (&rest _)
  "Apply the duotone theme if currently enabled."
  (when (member 'duotone custom-enabled-themes)
    (when duotone-reload--debounce-timer
      (cancel-timer duotone-reload--debounce-timer))
    (setq duotone-reload--debounce-timer (run-with-idle-timer 1 nil #'duotone-theme-apply))))

(defun duotone-reload-install-load-hook ()
  "Install a hook to apply the theme when a file is loaded."
  (add-hook 'after-load-functions #'duotone-reload-apply-theme))

(defun duotone-reload-install-theme-advice ()
  "Add an advice to re-apply duotone after a theme is loaded."
  (advice-add #'load-theme :after #'duotone-reload-apply-theme))

(add-hook 'after-init-hook #'duotone-reload-install-load-hook)

(provide 'duotone-reload)
;;; duotone-reload.el ends here
