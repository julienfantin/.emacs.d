;;; config-prog-mode.el --- Generic programming mode  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Julien Fantin

;; Author: Julien Fantin <julienfantin@gmail.com>
;; Keywords: languages

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

(use-package prog-mode
  :defer t
  :hook ((prog-mode . auto-fill-mode)
         (prog-mode . display-line-numbers-mode)))

(use-package conf-mode
  :defer t
  :preface (defun config-prog-run-hooks () (run-hooks 'prog-mode-hook))
  :after prog-mode
  :config (add-hook 'conf-mode-hook #'config-prog-run-hooks))

(use-package autoinsert
  :hook (after-init . auto-insert-mode))

(use-package highlight-symbol
  :straight t
  :hook
  ((highlight-symbol-mode . highlight-symbol-nav-mode)
   (prog-mode . highlight-symbol-mode))
  :custom
  (highlight-symbol-highlight-single-occurrence nil)
  (highlight-symbol-idle-delay 0.25)
  (highlight-symbol-on-navigation-p t))

(provide 'config-prog-mode)
;;; config-prog-mode.el ends here
