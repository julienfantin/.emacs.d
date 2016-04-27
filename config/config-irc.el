;;; config-irc.el --- IRC chat                       -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Julien Fantin

;; Author: Julien Fantin(require 'use-config) <julienfantin@gmail.com>
;; Keywords: comm

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


(use-package private :demand t)

(use-package erc
  :defer t
  :commands erc
  :config
  (progn
    ;; Ignoring
    (setq erc-hide-list '("JOIN" "PART" "QUIT"))
    ;; Tracking
    (setq erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT"))
    ;; Filling chan buffers
    (setq erc-fill-function 'erc-fill-static
          erc-fill-static-center 15)))

(use-package erc-hl-nicks
  :ensure t
  :defer t
  :init
  (after 'erc
    (add-hook 'erc-mode-hook 'erc-hl-nicks-mode)))

(provide 'config-irc)
;;; config-irc.el ends here
