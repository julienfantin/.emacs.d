;;; config-elpa.el --- Emacs packages management     -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Julien Fantin

;; Author: Julien Fantin <julienfantin@gmail.com>
;; Keywords: internal

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

(require 'package)
(require 'subr-x)
(require 'tls)
(require 'gnutls)

;;; Config

(setq package-archives
      '(("melpa"        . "https://melpa.org/packages/")
        ("melpa-stable" . "https://stable.melpa.org/packages/")
        ("gnu"          . "https://elpa.gnu.org/packages/")))

;;; Bootstrap use-package

(with-eval-after-load 'gnutls

  ;; `use-package' does this for us normally.
  (eval-when-compile
    (require 'gnutls))

  ;; Do not allow insecure TLS connections.
  (setq gnutls-verify-error t)

  ;; Bump the required security level for TLS to an acceptably modern value.
  (setq gnutls-min-prime-bits 2048))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package)
  (package-install 'bind-key))

(eval-when-compile (require 'use-package))

;;; Third-party

(use-package use-package-ensure-system-package
  :straight t)

(use-package system-packages
  :straight t
  :custom
  (system-packages-use-sudo nil)
  (system-packages-package-manager 'brew))

(use-package paradox
  :straight t
  :custom
  (paradox-execute-asynchronously t)
  (paradox-github-token t))

(provide 'config-elpa)
;;; config-elpa.el ends here
