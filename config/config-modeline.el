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

(require 'use-package)

;;; Third-party

(use-package keycast
  :straight t
  :commands (-keycast-mode)
  :hook (after-init . -keycast-mode)
  :custom
  (keycast-separator-width 1)
  ;; Don't display the command counter
  (mode-line-keycast-format "%s%k%c")
  :config
  ;; Redefine the minor mode so it only updates internal state without trying to
  ;; change the modeline format...
  (define-minor-mode -keycast-mode
    "Show current command and its key binding in the mode line."
    :global t
    (if -keycast-mode
        (add-hook 'pre-command-hook 'keycast-mode-line-update t)
      (remove-hook 'pre-command-hook 'keycast-mode-line-update))))

(use-package doom-modeline
  :straight t
  :after keycast
  :init (doom-modeline-mode 1)
  :custom
  (doom-modeline-icon (display-graphic-p)) ;; NOTE eval (all-the-icons-install-fonts)
  (doom-modeline-bar-width 36)
  (doom-modeline-height 24)
  :config
  ;; Define a doom-modeline segment for keycast
  (doom-modeline-def-segment keycast mode-line-keycast)
  ;; Define a custom modeline
  (doom-modeline-def-modeline '-modeline
    '(bar workspace-name window-number matches buffer-info remote-host selection-info keycast)
    '(misc-info github debug repl lsp major-mode process vcs checker))
  ;; Bug with the default option which doesn't apply to the current buffer?
  (doom-modeline-set-modeline '-modeline nil)
  (doom-modeline-set-modeline '-modeline t))

(use-package hide-mode-line :straight t)

(use-package recursion-indicator
  :straight t
  :demand t
  :hook (after-init-hook . recursion-indicator-mode))

(provide 'config-modeline)
;;; config-modeline.el ends here
