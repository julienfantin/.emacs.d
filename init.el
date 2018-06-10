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


;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.

(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

(require 'cl-lib)

(require 'cl-macs)
(require 'package)
(set-default 'truncate-lines t)

(setq use-package-verbose t)
(defvar init--file-name-handler-alist file-name-handler-alist)


(setq file-name-handler-alist nil)


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
(require 'no-littering nil t)

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
(use-config config-browser :defer 5)
(use-config config-buffers)
(use-config config-completion :defer 1)
(use-config config-debug)
(use-config config-defaults)
(use-config config-doc :defer 1)
(use-config config-editing :defer 1)
(use-config config-elpa)
(use-config config-files)
(use-config config-font-lock)
(use-config config-frame :if window-system)
(use-config config-git :defer 1)
(use-config config-gui)
(use-config config-helm :if (equal 'helm config-completion-system))
(use-config config-help :defer 1)
(use-config config-indentation :defer 1)
(use-config config-irc :defer 5)
(use-config config-ivy :if (equal 'ivy config-completion-system))
(use-config config-layouts)
(use-config config-marks :defer 1)
(use-config config-modeline)
(use-config config-org)
(use-config config-outlines :disabled t)
(use-config config-parsers :defer 1)
(use-config config-persistence :defer 2)
(use-config config-prog-mode)
(use-config config-project :defer 2)
(use-config config-prose :defer 2)
(use-config config-scratch)
(use-config config-search :defer 1)
(use-config config-sexp)
(use-config config-shell :defer 1)
(use-config config-windows :defer 1)

;; ** Langs

(use-config config-ansible :disabled t)
(use-config config-cl :disabled t)
(use-config config-clojure :defer 2)
(use-config config-clojurescript :defer 2)
(use-config config-emacs-lisp)
(use-config config-js :defer 2)
(use-config config-nix :disabled t)
(use-config config-ocaml :defer 2)
(use-config config-sql :disabled t)
(use-config config-web :defer 2)
(use-config config-yaml :defer 2)

;; ** Tools

(use-config config-docker :defer 2)

;; ** Keybindings

(use-config config-keybindings)

;; ** WIP

(use-config config-wip)

(add-hook 'after-init-hook
          #'(lambda ()
              (setq gc-cons-threshold 16777216
                    gc-cons-percentage 0.1)))

(add-hook 'after-init-hook
          #'(lambda ()
              (setq file-name-handler-alist init--file-name-handler-alist)))

(provide 'init)
;;; init.el ends here
