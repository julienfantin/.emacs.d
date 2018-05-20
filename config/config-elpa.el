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


;; * Package archives

(setq package-archives
      '(("melpa"        . "https://melpa.org/packages/")
        ("melpa-stable" . "https://stable.melpa.org/packages/")
        ("gnu"          . "https://elpa.gnu.org/packages/")))

;; ** TLS configuration for elpa over HTTPS

(defvar config-elpa-pip-tls-trustfile
  (if-let ((out (shell-command-to-string "python -m certifi")))
      (replace-regexp-in-string "\n" "" out)
    (error "Install required python package with: python -m pip install --user certifi"))
  "Path to a TLS trust-file generated by pip.")

(setq gnutls-verify-error t
      gnutls-trustfiles (list config-elpa-pip-tls-trustfile))

(setq tls-checktrust t
      tls-program
      `(,(format "gnutls-cli --x509cafile %s -p %%p %%h" config-elpa-pip-tls-trustfile)))

(defun config-test-tls ()
  "Error if TLS isn't properly configured."
  (let ((bad-hosts
         (cl-loop for bad
                  in `("https://wrong.host.badssl.com/"
                       "https://self-signed.badssl.com/")
                  if (condition-case _e
                         (url-retrieve
                          bad (lambda (_retrieved) t))
                       (error nil))
                  collect bad)))
    (if bad-hosts
        (error (format "tls misconfigured; retrieved %s ok" bad-hosts)) ;
      (url-retrieve "https://badssl.com" (lambda (_retrieved) t)))))


;; * Package helpers

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(defvar use-package-always-defer t)

(require 'use-package)

(use-package system-packages
  :ensure t
  :config
  (setq
   system-packages-use-sudo nil
   system-packages-package-manager 'brew))

(use-package use-package-ensure-system-package :ensure t)

(use-package paradox
  :ensure t
  :config
  (progn
    (setq paradox-execute-asynchronously t
          paradox-github-token t)))

(provide 'config-elpa)
;;; config-elpa.el ends here
