;;; init.el --- Config init                          -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Julien Fantin

;; Author: Julien Fantin <julienfantin@gmail.com>
;; Keywords:

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

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.

(setq gc-cons-threshold most-positive-fixnum)

(require 'cl-lib)
(require 'cl-macs)
(require 'package)

(set-default 'truncate-lines t)
(setq use-package-verbose t)

(add-hook 'after-init-hook #'(lambda () (setq gc-cons-threshold 800000)))



;; * Config options

(defvar config-completion-system 'ivy)


;; * Bootstrap
;; ** Path

(setq user-emacs-directory (expand-file-name "~/.emacs.d/"))

(defun -reload-init ()
  "Reload the init file."
  (interactive)
  (load-file (expand-file-name "init.el" user-emacs-directory)))

(eval-and-compile
  ;; Setup various paths before we do anything, use-config depends on this
  (load-file (expand-file-name "config/config-path.el" user-emacs-directory))
  ;; Setup elpa
  (load-file (expand-file-name "config/config-elpa.el" user-emacs-directory))
  ;; Manually load use-config, it also provide some useful helpers
  (load-file (expand-file-name "lib/use-config/use-config.el" user-emacs-directory)))

(require 'use-package)

;; Setup our env path here, some configs might need this to be set
(use-package exec-path-from-shell
  :ensure t
  :init (exec-path-from-shell-initialize))


;; * Configs

;; Load order notes:
;; - Demand use-config and config-path
;; - Load keybindings last

(use-package use-config)
(use-package private)

(use-config config-theme)
(use-config config-path)
(use-config config-browser)
(use-config config-buffers)
(use-config config-completion)
(use-config config-debug)
(use-config config-defaults)
(use-config config-doc)
(use-config config-editing)
(use-config config-elpa)
(use-config config-files)
(use-config config-font-lock)
(use-config config-frame :if window-system)
(use-config config-git)
(use-config config-gui)
(use-config config-helm :disabled t)
(use-config config-help)
(use-config config-indentation)
(use-config config-irc)
(use-config config-ivy :if (equal 'ivy config-completion-system))
(use-config config-layouts)
(use-config config-marks)
(use-config config-modeline :disabled t)
(use-config config-org)
(use-config config-outlines)
(use-config config-parsers)
(use-config config-persistence)
(use-config config-prog-mode)
(use-config config-project)
(use-config config-prose)
(use-config config-scratch)
(use-config config-search)
(use-config config-sexp)
(use-config config-shell)
(use-config config-windows)

;; ** Langs

(use-config config-ansible :disabled t)
(use-config config-cl :disabled t)
(use-config config-clojure)
(use-config config-clojurescript)
(use-config config-emacs-lisp)
(use-config config-nix :disabled t)
(use-config config-ocaml)
(use-config config-sql :disabled t)
(use-config config-web)
(use-config config-yaml)

;; ** Tools

(use-config config-docker)

;; ** Keybindings

(use-config config-keybindings)

;; ** WIP

(use-config config-wip)

(provide 'init)
;;; init.el ends here
